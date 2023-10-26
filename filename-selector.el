;; -*- lexical-binding: t; -*-

(cl-defstruct (filename-selector (:constructor filename-selector--create))
  "A record summarizing several different formats of the name of a
Java source file:

1. the full path (full)
2. the relative path corresponding to the package name (simple)
3. the full path to the corresponding class file (class)
4. the fully-qualified package name (package-unit)
5. the basename: no path prefix, no filetype suffix (basename)"
  full simple class package basename)

(defun strip-non-code-artefacts ()
  "Strip the current temporary buffer of non-code artefacts."
  (when (string-match-p (rx bos " *temp*") (buffer-name))
    (save-excursion
      (rx-let ((java-multi-line-comment
                (: "/*" (*
                         (| (not "*")
                            (: "*" (not "/"))))
                   (+ "*") "/"))
               (java-line-comment
                (: "//" (* not-newline) eol))
               (java-string
                (: ?\" (*? anything) ?\")))
        (replace-regexp (rx (or java-multi-line-comment
                                java-line-comment
                                java-string))
                        "")))))

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (strip-non-code-artefacts)
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
    (match-string-no-properties 1)))

(defun create-project-environment (project-root package-subdir class-subdir)
  "Return the public constructor for FILENAME-SELECTOR objects."
  (let ((package-root (concat project-root package-subdir))
        (class-root (concat project-root class-subdir)))
    (lambda (full-filename)
      (let* ((simple-filename (string-remove-prefix package-root full-filename))
             (class-filename (let ((partial-path (concat class-root simple-filename)))
                               (replace-regexp-in-string (rx ".java" eos) ".class" partial-path)))
             (basename (file-name-base full-filename))
             (package-name (find-package-name full-filename)))
        (let ((filename-selector (filename-selector--create
                                  :full full-filename
                                  :simple simple-filename
                                  :class class-filename
                                  :basename basename))
              (package-unit (if package-name
                                (format "%s.%s" package-name basename)
                              basename)))
          (setf (filename-selector-package filename-selector) package-unit)
          filename-selector)))))

(cl-defmethod get-package-prefix ((this filename-selector))
  "Given a FILENAME-SELECTOR object, return the package prefix
associated with it."
  (let ((package-unit (filename-selector-package this)))
    (string-join (butlast (string-split package-unit (rx "."))) ".")))

(cl-defmethod get-lines ((this filename-selector))
  "Return the (non-empty) lines of FULL-FILENAME, after first having
stripped away comments."
  (with-temp-buffer
    (insert-file (filename-selector-full this))
    (strip-non-code-artefacts)
    (thread-last
      (string-lines (buffer-string) t)
      (mapcar #'string-trim)
      (cl-remove-if #'string-empty-p))))

(cl-defmethod find-imports ((this filename-selector))
  "Extract the contents of all import statements in THIS as a list
of package units.

Note that if the package unit is a glob, it remains unexpanded."
  (with-temp-buffer
    (insert-file (filename-selector-full this))
    (strip-non-code-artefacts)
    (let (package-names)
      (while (re-search-forward (rx "import" (+ space) (group (+ not-newline)) ";") nil t)
        (push (match-string-no-properties 1) package-names))
      package-names)))

(cl-defmethod pretty-print ((this filename-selector))
  (filename-selector-simple this))

(provide 'filename-selector)
