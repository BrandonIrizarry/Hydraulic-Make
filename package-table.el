;; -*- lexical-binding: t; -*-

(require 'project-environment)

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  table)

(cl-defmethod package-table-create ((this project-environment))
  "The public constructor for PACKAGE-TABLE objects."
  (let ((known-packages (make-hash-table :test #'equal)))
    (dolist (file (project-environment-files this) known-packages)
      (let* ((package (get-package this file))
             (files (gv-ref (gethash package known-packages (list)))))
        (puthash package (cons file (gv-deref files)) known-packages)))
    (package-table--create :table known-packages)))

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

(provide 'package-table)
