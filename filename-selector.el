;; -*- lexical-binding: t; -*-

(defvar *java-project-root* "~/eclipse-workspace2/UCSDGraphs/"
    "The top of the UCSDGraphs project hierarchy.")

(defvar *java-project-package-root* (concat *java-project-root* "src/")
  "An immediate subdirectory of the project root, serving as the
top-level package directory.")

(defvar *java-project-class-root* (concat *java-project-root* "bin/")
  "An immediate subdirectory of the project root, where class files
are stored in a manner reflecting the package hierarchy.")

(cl-defstruct (filename-selector (:constructor filename-selector--create))
  "A record summarizing the four different formats of the name of a
Java source file:

1. the full path (full)
2. the relative path corresponding to the package name (simple)
3. the full path to the corresponding class file (class)
4. the fully-qualified package name (package-unit)"
  full simple class package)

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
    (or (match-string-no-properties 1)
        "default")))

(defun filename-selector-create (full-filename)
  "Public constructor for FILENAME-SELECTOR objects."
  (let* ((simple-filename (string-remove-prefix *java-project-package-root* full-filename))
         (class-filename (let ((partial-path (concat *java-project-class-root* simple-filename)))
                           (replace-regexp-in-string (rx ".java" eos) ".class" partial-path)))
         (package-unit (format "%s.%s"
                               (find-package-name full-filename)
                               (file-name-base full-filename))))
    (filename-selector--create :full full-filename :simple simple-filename :class class-filename :package package-unit)))

(cl-defmethod get-package-prefix ((this filename-selector))
  "Given a FILENAME-SELECTOR object, return the package prefix
associated with it."
  (let ((package-unit (filename-selector-package this)))
    ;; Weird, but works.
    (mapconcat #'identity
               (butlast (string-split package-unit (rx ".")))
               ".")))

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

(provide 'filename-selector)
