;; -*- lexical-binding: t; -*-

(require 'h-project-environment)

(cl-defstruct (h-package-table (:constructor h-package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  penv table)

(cl-defmethod h-package-table-create ((this h-project-environment))
  "The public constructor for HMAKE-PACKAGE-TABLE objects."
  (let ((ptable (h-package-table--create
                 :penv this)))
    (let ((known-packages (make-hash-table :test #'equal)))
      (dolist (file (h-project-environment-files this) known-packages)
        (let* ((package (h-get-package this file))
               (files (gv-ref (gethash package known-packages (list)))))
          (puthash package (cons file (gv-deref files)) known-packages)))
      (setf (h-package-table-table ptable) known-packages)
      ptable)))

(cl-defmethod h-get-file ((this h-package-table) package-path &key type)
  (h-get-file (h-package-table-penv this) package-path :type type))

(cl-defmethod h-get-files ((this h-package-table) package)
  "Return the list of files under PACKAGE."
  (gethash package (h-package-table-table this)))

(cl-defmethod h-package-p ((this h-package-table) package)
  "Check whether PACKAGE is one of the project's packages."
  (gethash package (h-package-table-table this)))

(cl-defmethod h-get-parent-package ((this h-package-table) package-path)
  "Return the parent package of PACKAGE-PATH."
  (h-get-package (h-package-table-penv this) package-path))

(cl-defmethod h-member-default-p ((this h-package-table) package-path)
  "Return whether PACKAGE-PATH is a member of the default package."
  (equal "default" (h-get-parent-package this package-path)))

(cl-defmethod h-get-siblings ((this h-package-table) package-path)
  "Return the files that belong to the same package as PACKAGE-PATH."
  (h-get-files this (h-get-parent-package this package-path)))

(cl-defmethod h-analyze-imports ((this h-package-table) tmp-buffer)
  "Analyze the various import statements inside TMP-BUFFER.

Return the list of dependencies DEPS.

IMPORT: If the penultimate prefix is a package, then do the
following: If the argument has a glob, add the implied members to
DEPS. Else, add the argument itself to DEPS.

IMPORT STATIC: Use HMAKE-UTILS-GET-SUCCESSIVE-PREFIXES to find
the type being used."
  (with-current-buffer tmp-buffer
    (goto-char (point-min))
    (let ((deps (list)))
      (while (re-search-forward "import" nil t)
        (let* ((import-static-p (re-search-forward "static" (line-end-position) t))
               ;; Trailing semicolon is removed here. See
               ;; 'hmake-utils-get-rest-of-line'.
               (argument (hu-get-rest-of-line))
               (prefixes (hu-get-successive-prefixes argument)))
          (let ((dep
                 (cond
                   ;; Prefix-format for static imports: (static-member
                   ;; parent-class parent-package)
                   ;;
                   ;; The parent class is the package member we're
                   ;; interested in as a dependency. The parent
                   ;; package is simply referred to here as `package'.
                   ;; The static member being imported is ignored.
                   (import-static-p
                    (cl-assert (>= 3 (length prefixes)))
                    (seq-let (_ package-member package) prefixes
                      (and (h-package-p this package) package-member)))

                   ;; Glob used with (non-static) import.
                   ((string-match-p "\\.\\*\\'" argument)
                    ;; The asterisk isn't included among the
                    ;; PREFIXES. Hence, we must iterate over the files
                    ;; given by the package represented by the first
                    ;; prefix. (In prior iterations of the code, we
                    ;; would tokenize ARGUMENT, and so it was included
                    ;; in those schemes.)
                    (let ((package (cl-first prefixes)))
                      (when (h-package-p this package)
                        (cl-loop
                              for file in (h-get-files this package)
                              collect (h-get-file this file :type 'package)))))

                   ;; Ordinary import.
                   (t
                    (seq-let (package-member package) prefixes
                      (and (h-package-p this package) package-member))))))
            ;; DEP can be a list of packages imported by a glob.
            (if (listp dep)
                (setq deps (nconc deps dep))
              (push dep deps)))))
      ;; Upon exiting the while loop, we've reached the end of the
      ;; buffer; so let's find our way back to the last import
      ;; statement, and then move forward to the beginning of the next
      ;; line.
      ;;
      ;; If there's no import statement to go back to, simply move to
      ;; the beginning of the buffer. Make sure to skip the package
      ;; statement if present.
      (or (and (re-search-backward "import" nil t)
               (forward-line 1))
          (progn
            (goto-char (point-min))
            (and (re-search-forward "package" nil t)
                 (forward-line 1))))
      ;; Return the dependencies we've found so far.
      deps)))

(cl-defmethod h-find-dependencies ((this h-package-table) package-path)
  "Find and return the list of immediate dependencies of
PACKAGE-PATH."
  (with-temp-buffer
    (insert-file-contents (h-get-file this package-path :type 'full))
    (hu-strip-non-code-artefacts)

    (let ((deps (h-analyze-imports this (current-buffer)))
          (local-files (remove (file-name-base package-path)
                               (mapcar #'file-name-base (h-get-siblings this package-path)))))
      (while (re-search-forward (rx hu-java-compound-identifier) nil t)
        (let* ((identifier (match-string-no-properties 0))
               (prefixes (hu-get-successive-prefixes identifier 'ascending))
               (dep
                (let ((basename (car prefixes))
                      (parent-package (h-get-parent-package this package-path)))
                  (if (and (null (cdr prefixes))
                           (member basename local-files))
                      (concat parent-package "." basename)
                    (let ((intended-package (cl-member-if (lambda (prefix) (h-package-p this prefix)) prefixes)))
                      (cond (intended-package (cl-second intended-package))
                            ((and (h-member-default-p this package-path)
                                  (member basename local-files))
                             (concat "default." basename))
                            ;; Package-local static reference
                            ((and (cdr (assoc (concat parent-package "." basename)
                                              (h-project-environment-package-to-file-alist (h-package-table-penv this)))))
                             ;; Avoid overcapturing any mention of the
                             ;; public class' datatype.
                             (unless (string= basename (file-name-base package-path))
                               (concat parent-package "." basename)))))))))
          (and dep (push dep deps))))
      ;; Return the dependencies we found, removing duplicates.
      (cl-remove-duplicates deps :test #'equal))))

;; Tests
(require 'ert-fixtures)

(defvar ht-fixture1 (efs-define-fixture ((ht-project-root "~/Java/FakeProject/")
                                         (ht-penv (h-project-environment-create ht-project-root))
                                         (ht-ptable (h-package-table-create ht-penv)))))

(defvar ht-fixture2 (efs-define-fixture ((ht-project-root "~/eclipse-workspace2/UCSDGraphs/")
                                         (ht-penv (h-project-environment-create ht-project-root))
                                         (ht-ptable (h-package-table-create ht-penv)))))

(efs-use-fixtures test1 (ht-fixture1)
  :tags '(fake-project package-table find-dependencies)
  (defvar ht-ptable)
  (should (equal '("andaluz.Malaga" "andaluz.Sevilla" "defs.Data")
                 (sort (h-find-dependencies ht-ptable "Study.java") #'string<))))

(efs-use-fixtures test2 (ht-fixture1)
  :tags '(fake-project package-table get-package)
  (defvar ht-ptable)
  (should (equal '("andaluz.Sevilla" "default.Berlin" "default.Paris")
                 (sort (h-find-dependencies ht-ptable "Madrid.java") #'string<))))

(efs-use-fixtures test3 (ht-fixture2)
  :tags '(ucsd-mapapp package-table find-dependencies)
  (defvar ht-ptable)
  (should (equal '("week3example.MazeLoader" "week3example.MazeNode")
                 (sort (h-find-dependencies ht-ptable "week3example/Maze.java") #'string<))))

(provide 'h-package-table)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-") ("hu-" . "hmake-utils-") ("efs-" . "ert-fixtures-") ("ht-" . "hmake-tests-"))
;; End:
