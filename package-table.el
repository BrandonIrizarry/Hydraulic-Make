;; -*- lexical-binding: t; -*-

(require 'filename-selector)

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  hash-table assoc-table)

(defun package-table-create (project-root package-subdir class-subdir)
  "The public constructor for PACKAGE-TABLE objects."
  (let ((source-files (java-project-get-source-files project-root package-subdir class-subdir))
        (known-packages (make-hash-table :test #'equal)))
    (dolist (source-file source-files known-packages)
      (let* ((package-prefix (get-package-prefix source-file))
             ;; Get the reference to the list of files under this
             ;; prefix, to make adding to it less verbose.
             (files (gv-ref (gethash package-prefix known-packages (list)))))
        ;; Add the current project file under the corresponding
        ;; package prefix.
        (puthash package-prefix (cons source-file (gv-deref files)) known-packages)))
    (package-table--create :hash-table known-packages :assoc-table (map-pairs known-packages))))

(cl-defmethod pretty-print ((this package-table))
  "Pretty-print PACKAGE-TABLE."
  (mapcar (pcase-lambda (`(,package-name . ,package-files))
              (cons package-name (mapcar #'pretty-print package-files)))
          (package-table-assoc-table this)))

(cl-defmethod get-files ((this package-table) package-prefix)
  "Given PACKAGE-PREFIX, get the list of files (as file-selector
objects) that belong to it."
  (gethash package-prefix (package-table-hash-table this)))

(cl-defmethod get-all-packages ((this package-table))
  "Return a list of all packages."
  (hash-table-keys (package-table-hash-table this)))

