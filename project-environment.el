;; -*- lexical-binding: t; -*-

(let ((load-path (cons (expand-file-name ".") load-path)))
  (require 'filename-selector))

(cl-defstruct (project-environment (:constructor project-environment--create))
  "A project environment defined by a root directory, a package
subdirectory, and a class-file subdirectory.

FILENAME-SELECTOR-CREATE is the constructor function for filename
selector objects used by the current Java project."
  project-root package-subdir class-subdir fs-create)

(defun project-environment-create (project-root package-subdir class-subdir)
  "The public constructor for PROJECT-ENVIRONMENT objects."
  (let ((penv (project-environment--create :project-root project-root
                                           :package-subdir package-subdir
                                           :class-subdir class-subdir)))
    ;; Define the filename-selector constructor
    (let* ((package-root (concat project-root package-subdir))
           (class-root (concat project-root class-subdir)))
      (setf (penv :fs-create)
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
                  filename-selector))))
      ;; Return the project-environment object
      penv)))

(cl-defmethod get-source-files ((this project-environment))
  "Return a list of all source files in the current Java project, converted
to FILE-SELECTOR objects."
  (cl-flet ((file-selector-create (project-environment-fs-create this)))
    (mapcar #'file-selector-create
            (directory-files-recursively (concat project-root package-subdir)
                                         (rx bol (not (any ".#")) (* not-newline) ".java" eol)))))
(provide 'project-environment)
