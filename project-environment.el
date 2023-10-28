;; -*- lexical-binding: t; -*-

(require 'utils)

(cl-defstruct (project-environment (:constructor project-environment--create))
  "A project environment defined by a root directory, a package
subdirectory, and a class-file subdirectory."
  project-root package-root class-root files)

(defun project-environment-create (project-root package-subdir class-subdir)
  "The public constructor for PROJECT-ENVIRONMENT objects."
  (project-environment--create :project-root project-root
                               :package-root (concat project-root package-subdir)
                               :class-root (concat project-root class-subdir)
                               :files (directory-files-recursively (concat project-root package-subdir)
                                                                   (rx bol (not (any ".#")) (* not-newline) ".java" eol))))


(cl-defmethod get-package ((this project-environment) package-path)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file (get-file this package-path :type 'full))
    (strip-non-code-artefacts)
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
    (match-string-no-properties 1)))

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
