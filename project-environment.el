;; -*- lexical-binding: t; -*-

(require 'utils)

(cl-defstruct (project-environment (:constructor project-environment--create))
  "A struct containing directory information about a project, along
with all its source files.

PROJECT-ROOT: the top-level directory.

PACKAGE-ROOT: the top-level package directory (for example,
PROJECT-ROOT/src).

CLASS-ROOT: the top-level class-file directory (for example,
PROJECT-ROOT/bin).

FILES: the list of files belonging to the project, as full-path
strings."
  project-root package-root class-root files)

(defun project-environment-create (project-root package-subdir class-subdir)
  "The public constructor for PROJECT-ENVIRONMENT objects."
  (let ((package-root (concat project-root package-subdir)))
    (project-environment--create :project-root project-root
                                 :package-root package-root
                                 :class-root (concat project-root class-subdir)
                                 :files (mapcar (lambda (filename)
                                                  (string-remove-prefix package-root filename))
                                                (directory-files-recursively package-root
                                                                             (rx bol (not (any ".#")) (* not-newline) ".java" eol))))))


(cl-defmethod get-package ((this project-environment) package-path)
  "Return the package PACKAGE-PATH belongs to, as a string.

For example, if PACKAGE-PATH is
gmapsfx/shapes/CircleOptions.java, this method returns
\"gmapsfx.shapes\"."
  (with-temp-buffer
    (insert-file (get-file this package-path :type 'full))
    (strip-non-code-artefacts)
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
    (or (match-string-no-properties 1)
        "default")))

(cl-defmethod get-file ((this project-environment) package-path &key type)
  "Get a filename equivalent to PACKAGE-PATH, selecting the format
using the keyword argument TYPE."
  (pcase type
    ('full (concat (project-environment-package-root this)
                   package-path))
    ('class (concat (project-environment-class-root this)
                    (replace-regexp-in-string "\\.java\\'" ".class"
                                              package-path)))
    ('package (concat (get-package this package-path)
                      "."
                      (file-name-base package-path)))
    ('basename (file-name-base package-path))))

(provide 'project-environment)
