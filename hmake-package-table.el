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

;; Not used in this file
(cl-defmethod h-pretty-print ((this h-package-table))
  "Pretty-print HMAKE-PACKAGE-TABLE."
  (mapcar (pcase-lambda (`(,package-name . ,package-files))
              (cons package-name package-files))
          (map-pairs (h-package-table-table this))))

(cl-defmethod h-get-files ((this h-package-table) package)
  "Return the list of files under PACKAGE."
  (gethash package (h-package-table-table this)))

;; Not used in this file
(cl-defmethod h-get-all-packages ((this h-package-table))
  "Return a list of all packages."
  (hash-table-keys (h-package-table-hash-table this)))

(cl-defmethod h-package-p ((this h-package-table) package)
  "Check whether PACKAGE is one of the project's packages."
  (gethash package (h-package-table-table this)))

(cl-defmethod h-find-dependencies ((this h-package-table) package-path)
  "Find and return hash-set of dependencies of PACKAGE-PATH."
  (with-temp-buffer
    (insert-file (h-get-file this package-path :type 'full))
    (hu-strip-non-code-artefacts)
    (rx-let ((java-identifier (: (any alpha "_") (* (any alnum "_"))))
             ;; The following is the one we'll be using. It'll match
             ;; object fields and methods, but it'll also match
             ;; package uses.
             (java-compound-identifier (: (group (* java-identifier ".")) (group java-identifier))))
      ;; MENTIONS is our return value. It's defined as a hash table,
      ;; to avoid duplicates.
      (let* ((mentions (make-hash-table :test #'equal))
             (parent-package (h-get-package (h-package-table-penv this) package-path))
             (local-files (remove (file-name-base package-path)
                                  (mapcar #'file-name-base (h-get-files this parent-package)))))
        (while (re-search-forward (rx java-compound-identifier) nil t)
          ;; Strangely, this is beneath the while-test, yet the
          ;; while-test still executes!
          (catch 'continue
            (let ((identifier (match-string-no-properties 0)))

              ;; Check import statements for globs.
              (when (and (equal identifier "import")
                         (string-match-p "\\.\\*;\\'"
                                         (hu-get-current-line)))
                (let ((package (progn (re-search-forward (rx java-compound-identifier)
                                                         (line-end-position))
                                      (match-string-no-properties 0))))
                  (dolist (file (h-get-files this package))
                    (puthash (h-get-file (h-package-table-penv this) file :type 'package)
                             t
                             mentions))
                  (goto-char (line-end-position))
                  (throw 'continue nil)))

              ;; If a non-glob import, we still may as well skip the
              ;; word "import" itself (simplifies debugging).
              (when (equal identifier "import")
                (throw 'continue nil))

              ;; Similarly, skip package statement.
              (when (equal identifier "package")
                (goto-char (line-end-position))
                (throw 'continue nil))

              ;; Since this check doesn't depend on 'prefix' or
              ;; 'terminal' (see below), putting it here may let us
              ;; simplify what follows.
              (when (and (equal parent-package "default")
                         (member identifier (h-get-files this "default")))
                ;; For now, use this format for identifiers that
                ;; belong to the default package.
                (puthash (concat "default." identifier) t mentions)
                (throw 'continue nil))

              ;; Whatever else, will fall through to here.
              (let ((prefix (string-remove-suffix "." (match-string-no-properties 1)))
                    (terminal (match-string-no-properties 2)))
                (cond ((h-package-p this prefix)
                       (puthash identifier t mentions))
                      ((and (string-empty-p prefix)
                            (member identifier local-files))
                       (puthash (concat parent-package "." identifier) t mentions)))))))
        mentions))))

(cl-defmethod h-list-deps ((this h-project-environment) package-path)
  "Return the list of dependencies of PACKAGE-PATH, given a project
environment."
  (let* ((ptable (h-package-table-create this)))
    (hash-table-keys (h-find-dependencies ptable package-path))))

(provide 'h-package-table)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-") ("hu-" . "hmake-utils-"))
;; End:
