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
  "A record summarizing the four different formats of the name
of a Java source file:

1. the full path (full)
2. the relative path corresponding to the package name (simple)
3. the full path to the corresponding class file (class)
4. the fully-qualified package name (package-unit)"
  full simple class package)

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";"))
    (match-string-no-properties 1)))

(defun filename-selector-create (full-filename)
  (let* ((simple-filename (replace-regexp-in-string (rx-to-string `(seq bos ,*java-project-package-root*))
                                                    ""
                                                    full-filename))
         (class-filename (let ((partial-path (concat *java-project-class-root* simple-filename)))
                           (replace-regexp-in-string (rx ".java" eos) ".class" partial-path)))
         (package-unit (format "%s.%s"
                               (find-package-name full-filename)
                               (file-name-base full-filename))))
    (filename-selector--create :full full-filename :simple simple-filename :class class-filename :package package-unit)))

(provide 'filename-formats)
