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
                    (let ((package (nth (- (length prefixes) 3) prefixes)))
                      (when (h-package-p this package)
                        (nth (- (length prefixes) 2) prefixes))))

                   ;; Glob used with (non-static) import
                   ((string-match-p "\\.\\*;\\'" argument)
                    ;; Note: we flatten DEPS at the end in case
                    ;; this code path gets hit.
                    (cl-loop
                          for file in (h-get-files this penultimate-prefix)
                          when (h-get-file this file :type 'package)
                          collect (h-get-file this file :type 'package)))

                   ;; Ordinary import.
                   (t argument))))
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
    (let* ((deps (list))
           (parent-package (h-get-package (h-package-table-penv this) package-path))
           (local-files (remove (file-name-base package-path)
                                (mapcar #'file-name-base (h-get-files this parent-package)))))

      (setq deps (nconc deps (h-analyze-imports this (current-buffer))))

      (while (re-search-forward (rx hu-java-compound-identifier) nil t)
        (let* ((identifier (match-string-no-properties 0))
               (prefixes (hu-get-successive-prefixes identifier))
               (dep (if (= 1 (length prefixes))
                        (progn
                          (cl-assert (equal (car prefixes) identifier))
                          (when (member identifier local-files)
                            (concat parent-package "." identifier)))
                      (let ((intended-package (cl-find-if (lambda (prefix) (h-package-p this prefix)) prefixes :from-end t)))
                        (when-let ((index (seq-position prefixes intended-package)))
                          (nth (1+ index) prefixes))))))
          (push dep deps)))
      ;; Return the dependencies we found, removing duplicates, and
      ;; flattening to remove occurrences of NIL.
      (cl-remove-duplicates (flatten-list deps) :test #'equal))))

;; Tests
(require 'ert-fixtures)

(defvar ht-fixture1 (efs-define-fixture ((project-root "~/Java/FakeProject/")
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


(provide 'h-package-table)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-") ("hu-" . "hmake-utils-") ("efs-" . "ert-fixtures-") ("ht-" . "hmake-tests-"))
;; End:
