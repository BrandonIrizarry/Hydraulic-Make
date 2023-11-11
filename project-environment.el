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
strings.

PACKAGE-TO-FILE-ALIST: map between fully qualified names and their
associated files (alist)."
  project-root package-root class-root files package-to-file-alist)

(defun project-environment-create (project-root package-subdir class-subdir)
  "The public constructor for PROJECT-ENVIRONMENT objects."
  (let* ((package-root (concat project-root package-subdir))
         (penv (project-environment--create
                :project-root project-root
                :package-root package-root
                :class-root (concat project-root class-subdir)))
         (fullpaths (directory-files-recursively package-root
                                                 (rx bol (not (any ".#")) (* not-newline) ".java" eol))))
    (setf (project-environment-files penv)
          (cl-loop for fullpath in fullpaths collect
                (let ((package-path (string-remove-prefix package-root fullpath)))
                  ;; Use GET-PACKAGE call to make sure all files are
                  ;; cached beforehand
                  (get-package penv package-path)
                  package-path)))
    penv))

(cl-defmethod get-package ((this project-environment) package-path)
  "Return the package PACKAGE-PATH belongs to, as a string.

For example, if PACKAGE-PATH is
gmapsfx/shapes/CircleOptions.java, this method returns
\"gmapsfx.shapes\"."
  (catch 'cached
    (let ((suffix (concat "." (file-name-base package-path))))
      (when-let ((cached-package (car (rassoc package-path (project-environment-package-to-file-alist this)))))
        (throw 'cached (string-remove-suffix suffix cached-package)))
      (let ((package
             (with-temp-buffer
               (insert-file (get-file this package-path :type 'full))
               (strip-non-code-artefacts)
               (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
               (or (match-string-no-properties 1)
                   "default"))))
        ;; Cache the package-name-to-file association.
        (push (cons (concat package suffix)
                    package-path)
              (project-environment-package-to-file-alist this))
        package))))

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
