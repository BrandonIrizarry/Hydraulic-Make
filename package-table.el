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
  "Pretty-print the package-table object using its assoc-table,
where the assoc values are also pretty-printed."
  (mapcar (pcase-lambda (`(,package-name . ,package-files))
              (cons package-name (mapcar #'pretty-print package-files)))
          (package-table-assoc-table this)))

(cl-defmethod get-files ((this package-table) package-prefix)
  "Given PACKAGE-PREFIX, get the list of files (as file-selector
objects) that belong to it."
  (gethash package-prefix (package-table-hash-table this)))

(cl-defmethod get-all-packages ((this package-table))
  "Return a list of all package prefixes (strings) in PACKAGE-TABLE."
  (hash-table-keys (package-table-hash-table this)))

(cl-defmethod find-inline-dependencies ((this-pt package-table) (this-fs filename-selector))
  (with-temp-buffer
    (insert-file (filename-selector-full this-fs))
    (strip-non-code-artefacts)
    (rx-let ((java-identifier (: (any alpha "_") (* (any alnum "_"))))
             ;; This is the one we'll be using. It'll match object
             ;; fields and methods, but it'll also match package uses.
             (java-compound-identifier (: (group (* java-identifier ".")) java-identifier)))
      (let ((mentions (make-hash-table :test #'equal)))
        (while (re-search-forward (rx java-compound-identifier) nil t)
          (let ((prefix (thread-last
                          (match-string-no-properties 1)
                          (string-remove-suffix ".")))
                (terminal (match-string-no-properties 2)))
            (when-let ((package-files (get-files this-pt prefix)))
              package-files)))))))

(defun main ()
  "A mock main function to test the current state of the
application."'
  (let ((ptable (package-table-create "~/eclipse-workspace2/UCSDGraphs/" "src/" "bin/"))
        (filename-selector-create (create-project-environment "~/eclipse-workspace2/UCSDGraphs/src/application/MapApp.java" "src/" "bin/")))
    (find-inline-dependencies ptable (funcall filename-selector-create "~/eclipse-workspace2/UCSDGraphs/src/application/MapApp.java"))))

;;; Obsolete code follows

(cl-defmethod find-dependencies ((this package-table) filename-selector)
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
