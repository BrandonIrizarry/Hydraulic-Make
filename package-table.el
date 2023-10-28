;; -*- lexical-binding: t; -*-

(require 'project-environment)

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  penv table)

(cl-defmethod package-table-create ((this project-environment))
  "The public constructor for PACKAGE-TABLE objects."
  (let ((ptable (package-table--create
                 :penv this)))
    (let ((known-packages (make-hash-table :test #'equal)))
      (dolist (file (project-environment-files this) known-packages)
        (let* ((package (get-package this file))
               (files (gv-ref (gethash package known-packages (list)))))
          (puthash package (cons file (gv-deref files)) known-packages)))
      (setf (package-table-table ptable) known-packages)
      ptable)))

(cl-defmethod get-file ((this package-table) package-path &key type)
  (get-file (package-table-penv this) package-path :type type))

(cl-defmethod pretty-print ((this package-table))
  "Pretty-print PACKAGE-TABLE."
  (mapcar (pcase-lambda (`(,package-name . ,package-files))
              (cons package-name package-files))
          (map-pairs (package-table-table this))))

(cl-defmethod get-files ((this package-table) package)
  "Return the list of files under PACKAGE."
  (gethash package (package-table-table this)))

(cl-defmethod get-all-packages ((this package-table))
  "Return a list of all packages."
  (hash-table-keys (package-table-hash-table this)))

(cl-defmethod package-p ((this package-table) package)
  "Check whether PACKAGE is one of the project's packages."
  (gethash package (package-table-table this)))

(cl-defmethod find-dependencies ((this package-table) package-path)
  (with-temp-buffer
    (insert-file (get-file this package-path :type 'full))
    (strip-non-code-artefacts)
    (rx-let ((java-identifier (: (any alpha "_") (* (any alnum "_"))))
             ;; The following is the one we'll be using. It'll match
             ;; object fields and methods, but it'll also match
             ;; package uses.
             (java-compound-identifier (: (group (* java-identifier ".")) (group java-identifier))))
      ;; A hash table is used to avoid duplicates.
      (let* ((mentions (make-hash-table :test #'equal))
             (parent-package (get-package (package-table-penv this) package-path))
             (local-files (remove (file-name-base package-path)
                                  (mapcar #'file-name-base (get-files this parent-package)))))
        (while (re-search-forward (rx java-compound-identifier) nil t)
          (let* ((identifier (match-string-no-properties 0))
                 (prefix (string-remove-suffix "." (match-string-no-properties 1)))
                 (terminal (match-string-no-properties 2)))
            (if (package-p this prefix)
                (puthash identifier t mentions)
              (if (string-empty-p prefix)
                  (if (member identifier local-files)
                      (puthash identifier t mentions)
                    (when (and (equal parent-package "default")
                               (member identifier (get-files this "default")))
                      (puthash identifier t mentions)))))))
        mentions))))

(cl-defmethod list-deps ((this project-environment) package-path)
  "Return the list of dependencies of PACKAGE-PATH, given a project
environment."
  (let* ((ptable (package-table-create this)))
    (hash-table-keys (find-dependencies ptable package-path))))

(provide 'package-table)
