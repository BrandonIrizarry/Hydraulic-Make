;; -*- lexical-binding: t; -*-

;; Eshell integration.

(require 'h-dependency-graph)

(cl-defmethod h-generate-invocation ((penv h-project-environment) target class-subdir)
  (catch 'done
    (let* ((modified-dependencies (h-get-modified-dependencies penv target))
           (full-filenames (mapcar (lambda (moddep)
                                     (h-get-file penv moddep :type 'full))
                                   modified-dependencies)))
      (unless full-filenames
        (throw 'done nil))

      (let ((command (list (format "javac -g -cp \"lib/*:%s\" -d %s" class-subdir class-subdir))))
        (mapconcat #'identity (append command full-filenames) " ")))))

(defun eshell/hmake-setup (&rest args)
  ;; I'm not sure if it's possible to temporarily set
  ;; ELISP-LISP-REGEXP for the scope of these calls, so we must intern
  ;; keyword-like strings manually.
  (cl-flet ((keywordify ()
              (mapcar (lambda (arg)
                        ;; Don't touch Lisp arguments (for example,
                        ;; the user's ELISP-LISP-REGEXP might
                        ;; recognize keyword syntax.) Else, assume
                        ;; everything is a string.
                        (cond ((not (stringp arg)) arg)
                              ((string-prefix-p ":" arg) (intern arg))
	                      ((string-suffix-p "/" arg) arg)
                              (t (concat arg "/"))))
	              args)))
    (let* ((kwargs (keywordify))
           (project-root (or (plist-get kwargs :root) "./"))
           (package-subdir (or (plist-get kwargs :src) "src/"))
           (class-subdir (or (plist-get kwargs :bin) "bin/")))
      (eshell-print (format "project root: '%s'\nsource directory: '%s'\nclass-file directory: '%s'\n----\n" project-root package-subdir class-subdir))
      (hmake--setup project-root package-subdir class-subdir))))

(defun hmake--setup (project-root package-subdir class-subdir)
  "Use an approach where the build and run commands are predefined
by this setup command."
  (let ((penv (h-project-environment-create (expand-file-name project-root) package-subdir class-subdir)))

    (defun eshell/hmake-build-command (&optional target)
      "Insert the appropriate build command into the Eshell promqpt.

Note that TARGET is required\; it's marked with '&optional'
because NIL is a valid input (the user, for example, may forget
to supply an argument. Otherwise, not supplying an argument
results in a somewhat long error message."

      (unless target
        (user-error "You must supply a path to a file (relative to source directory)"))

      ;; Let's define this right away, so that we can use it to insert
      ;; the run command right away if there's nothing to build.
      (defun eshell/hmake-run-command ()
        "Insert the appropriate run command into the Eshell prompt."
        (eshell-kill-input)
        (let ((package (h-get-package penv target)))
          (insert (format "java -cp \"lib/*:%s\" %s"
                          class-subdir
                          (concat package "." (file-name-base target))))))

      ;; Generate the build command (or else insert the run command.)
      (eshell-kill-input)

      (let ((invocation (h-generate-invocation penv target class-subdir)))
        (if invocation
            (insert invocation)
          (eshell-print "Nothing to build; inserting run command\n----\n")
          (eshell/hmake-run-command))))))

(provide 'hmake)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-"))
;; End:
