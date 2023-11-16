;; -*- lexical-binding: t; -*-

(require 'h-utils)

(cl-defstruct (h-project-environment (:constructor h-project-environment--create))
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

(cl-defun h-project-environment-create (project-root &key src bin &aux (package-subdir src) (class-subdir bin))
  "The public constructor for HMAKE-PROJECT-ENVIRONMENT objects."
  (setq package-subdir (or package-subdir "src/"))
  (setq class-subdir (or class-subdir "bin/"))

  (let* ((package-root (concat project-root package-subdir))
         (penv (h-project-environment--create
                :project-root project-root
                :package-root package-root
                :class-root (concat project-root class-subdir)))
         (fullpaths (directory-files-recursively package-root
                                                 (rx bol (not (any ".#")) (* not-newline) ".java" eol))))
    (setf (h-project-environment-files penv)
          (cl-loop for fullpath in fullpaths collect
                (let ((package-path (string-remove-prefix package-root fullpath)))
                  ;; Use HMAKE-GET-PACKAGE call to make sure all files are
                  ;; cached beforehand
                  (h-get-package penv package-path)
                  package-path)))
    penv))

(cl-defmethod h-get-package ((this h-project-environment) package-path)
  "Return the package PACKAGE-PATH belongs to, as a string.

For example, if PACKAGE-PATH is
gmapsfx/shapes/CircleOptions.java, this method returns
\"gmapsfx.shapes\"."
  (catch 'cached
    (let ((suffix (concat "." (file-name-base package-path))))
      (when-let ((cached-package (car (rassoc package-path (h-project-environment-package-to-file-alist this)))))
        (throw 'cached (string-remove-suffix suffix cached-package)))
      (let ((package
             (with-temp-buffer
               (insert-file (h-get-file this package-path :type 'full))
               (hu-strip-non-code-artefacts)

               ;; Find the package declaration, if any.
               (if (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
                   (match-string-no-properties 1)
                 "default"))))
        ;; Cache the package-name-to-file association.
        (push (cons (concat package suffix)
                    package-path)
              (h-project-environment-package-to-file-alist this))
        package))))

(cl-defmethod h-get-file ((this h-project-environment) package-path &key type)
  "Get a filename equivalent to PACKAGE-PATH, selecting the format
using the keyword argument TYPE."
  (pcase type
    ('full (concat (h-project-environment-package-root this)
                   package-path))
    ('class (concat (h-project-environment-class-root this)
                    (concat (replace-regexp-in-string (rx ".")
                                                      "/"
                                                      (h-get-file this package-path :type 'package))
                            ".class")))
    ('package (let ((package (h-get-package this package-path))
                    (base (file-name-base package-path)))
                (if (equal package "default")
                    base
                  (concat package "." base))))
    ('basename (file-name-base package-path))))

(provide 'h-project-environment)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-") ("hu-" . "hmake-utils-"))
;; End:
