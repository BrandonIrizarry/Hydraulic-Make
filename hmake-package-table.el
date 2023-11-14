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
                   ;; Static imports must have at least one package
                   ;; prefixing everything, followed by the datatype,
                   ;; followed by what's imported.
                   (import-static-p
                    (cl-assert (>= 3 (length prefixes)))
                    (let ((package (cl-third prefixes)))
                      (when (h-package-p this package)
                        (cl-second prefixes))))

                   ;; Glob used with (non-static) import
                   ((string-match-p "\\.\\*;\\'" argument)
                    ;; Note: we flatten DEPS at the end in case
                    ;; this code path gets hit.
                    (cl-loop
                          for file in (h-get-files this penultimate-prefix)
                          when (h-get-file this file :type 'package)
                          collect (h-get-file this file :type 'package)))

                   ;; Ordinary import.
                   (t
                    (let ((package (cl-second prefixes)))
                      (and (h-package-p this package) package))))))
            (push dep deps))))
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
      ;;
      ;; Flatten the list in case there are instances of plain-import
      ;; globs.
      (flatten-list deps))))

(cl-defmethod h-find-dependencies ((this h-package-table) package-path)
  "Find and return the list of immediate dependencies of
PACKAGE-PATH."
  (with-temp-buffer
    (insert-file (h-get-file this package-path :type 'full))
    (hu-strip-non-code-artefacts)

    (let ((deps (h-analyze-imports this (current-buffer)))
          (local-files (remove (file-name-base package-path)
                               (mapcar #'file-name-base (h-get-siblings this package-path)))))
      (while (re-search-forward (rx hu-java-compound-identifier) nil t)
        (let* ((identifier (match-string-no-properties 0))
               (prefixes (hu-get-successive-prefixes identifier 'ascending))
               (dep
                (let ((basename (car prefixes)))
                  (if (and (null (cdr prefixes))
                           (member basename local-files))
                      (concat (h-get-parent-package this package-path) "." basename)
                    (let ((intended-package (cl-member-if (lambda (prefix) (h-package-p this prefix)) prefixes)))
                      (cond (intended-package (cl-second intended-package))
                            ((and (h-member-default-p this package-path)
                                  (member basename local-files))
                             (concat "default." basename))))))))
          (and dep (push dep deps))))
      ;; Return the dependencies we found, removing duplicates.
      (cl-remove-duplicates deps :test #'equal))))

;; Tests
(require 'ert-fixtures)

(defvar ht-fixture1 (efs-define-fixture ((project-root "~/Java/FakeProject/")
                                         (penv (h-project-environment-create project-root))
                                         (ptable (h-package-table-create penv)))))

(defvar ht-fixture2 (efs-define-fixture ((project-root "~/eclipse-workspace2/UCSDGraphs/")
                                         (penv (h-project-environment-create project-root))
                                         (ptable (h-package-table-create penv)))))

(efs-use-fixtures test1 (ht-fixture1)
  :tags '(package-table find-dependencies)
  (should (equal (h-find-dependencies ptable "Study.java")
                 '("defs.Data"))))

(efs-use-fixtures test2 (ht-fixture1)
  :tags '(package-table get-package)
  (should (equal '("andaluz.Sevilla" "default.Berlin" "default.Paris")
                 (sort (h-find-dependencies ptable "Madrid.java") #'string<))))

;; BUGS:
;; default.MazeLoader - correct, but not in default package
;; default.MazeNode - same.
;; week3example.MazeNode - correct. But why is this one here, and not MazeLoader?
(efs-use-fixtures test3 (ht-fixture2)
  :tags '(package-table find-dependencies)
  (should (equal '("week3example.MazeLoader" "week3example.MazeNode")
                 (sort (h-find-dependencies ptable "week3example/Maze.java") #'string<))))

(provide 'h-package-table)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-") ("hu-" . "hmake-utils-") ("efs-" . "ert-fixtures-") ("ht-" . "hmake-tests-"))
;; End:
