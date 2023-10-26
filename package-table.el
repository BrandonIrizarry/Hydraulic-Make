;; -*- lexical-binding: t; -*-

(require 'filename-selector)

(defvar *java-project-all-files*
  (mapcar #'filename-selector-create
          (directory-files-recursively *java-project-package-root*
                                       (rx bol (not (any ".#")) (* not-newline) ".java" eol)))
  "A list of all source files in the current Java project, converted
to FILE-SELECTOR objects.")

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  table)

(defun package-table-create ()
  "The public constructor for PACKAGE-TABLE objects."
  (let ((known-packages (make-hash-table :test #'equal)))
    (dolist (file *java-project-all-files* known-packages)
      (let* ((package-prefix (get-package-prefix file))
             (files (gv-ref (gethash package-prefix known-packages (list)))))
        ;; Add the current project file under the corresponding
        ;; package prefix.
        (puthash package-prefix (cons file (gv-deref files)) known-packages))))
  (package-table--create :table (generate-package-table)))

(cl-defmethod get-files ((this package-table) package-name)
  (gethash package-name (package-table-table this)))

(cl-defmethod get-all-packages ((this package-table))
  (hash-table-keys (package-table-table this)))

(cl-defmethod find-dependencies ((this package-table) file)
  (let* ((lines (get-program-lines full-filename))
         ;; For inline package references. We're using a hash table
         ;; here as a hash set, to avoid duplicates.
         (mentions (make-hash-table :test #'equal))
         ;; Here, we explicitly assume that the directory hierarchy
         ;; and the package hierarchy are one and the same thing.
         (local-files (thread-last
                        full-filename
                        (find-package-name)
                        (get-file-list this)
                        (remove (find-simple-filename full-filename)))))
    (dolist (line lines)
      (pcase line
        ;; Skip the 'package' declaration, since these contain
        ;; "dotted" statements which our default case would otherwise
        ;; catch.
        ((rx bos (* space) "package") nil)
        ((rx bos (* space) "import")
         (let* ((words (reverse (string-split line (rx (any " ;")) t)))
                (package-unit (car words))
                (package-name (extract-package-name-from-unit package-unit)))
           (if (string-match-p (rx "*" eos) package-unit)
               (let ((file-list (get-file-list this package-name)))
                 (dolist (file file-list)
                   (puthash file t mentions)))
             (let ((file (lookup-file this package-unit)))
               ;; Avoid 'nil' (for example, when dealing with
               ;; something like 'java.util')
               (when (stringp file)
                 (puthash file t mentions))))))
        (_

         ;; Find local dependencies
         (let ((live-local-files
                (cl-remove-if-not (lambda (local-file)
                                    (let ((local (file-name-base local-file)))
                                      (when (string-match (rx-to-string `(seq (not (any alpha "_")) (group ,local) (not (any alnum "_"))))
                                                          line)
                                        (match-string-no-properties 1 line))))
                                  local-files)))
           (dolist (local-file live-local-files)
             (puthash local-file t mentions)))

         ;; Find inline dependencies
         (dolist (package-name (get-packages this))
           (if (string-match (rx-to-string `(seq ,package-name "." java-identifier)) line)
               (let ((package-unit (match-string-no-properties 0 line)))
                 (puthash (lookup-file this package-unit) t mentions)))))))
    (hash-table-keys mentions)))

(cl-defmethod generate-dependency-graph ((this package-table))
  (let ((all-files (get-all-files))
        (graph (make-hash-table :test #'equal)))
    (dolist (file all-files graph)
      (puthash (find-simple-filename file) (find-dependencies this file) graph))))
