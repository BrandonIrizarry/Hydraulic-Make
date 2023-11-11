;; -*- lexical-binding: t; -*-

(require 'project-environment)

(cl-defstruct (package-table (:constructor package-table--create))
  "A hash table mapping a package prefix to the files it encompasses."
  penv table)

(cl-defmethod package-table-create ((this project-environment))
  "The public constructor for PACKAGE-TABLE objects."
  (let ((ptable (package-table--create
                 :penv this)))
    (let ((known-packages (make-hash-table :test #'equal)))
      (dolist (file (project-environment-files this) known-packages)
        (let* ((package (get-package this file))
               (files (gv-ref (gethash package known-packages (list)))))
          (puthash package (cons file (gv-deref files)) known-packages)))
      (setf (package-table-table ptable) known-packages)
      ptable)))

(cl-defmethod get-file ((this package-table) package-path &key type)
  (get-file (package-table-penv this) package-path :type type))

(cl-defmethod pretty-print ((this package-table))
  "Pretty-print PACKAGE-TABLE."
  (mapcar (pcase-lambda (`(,package-name . ,package-files))
              (cons package-name package-files))
          (map-pairs (package-table-table this))))

(cl-defmethod get-files ((this package-table) package)
  "Return the list of files under PACKAGE."
  (gethash package (package-table-table this)))

(cl-defmethod get-all-packages ((this package-table))
  "Return a list of all packages."
  (hash-table-keys (package-table-hash-table this)))

(cl-defmethod package-p ((this package-table) package)
  "Check whether PACKAGE is one of the project's packages."
  (gethash package (package-table-table this)))

(cl-defmethod find-dependencies ((this package-table) package-path)
  "Find and return hash-set of dependencies of PACKAGE-PATH."
  (with-temp-buffer
    (insert-file (get-file this package-path :type 'full))
    (strip-non-code-artefacts)
    (rx-let ((java-identifier (: (any alpha "_") (* (any alnum "_"))))
             ;; The following is the one we'll be using. It'll match
             ;; object fields and methods, but it'll also match
             ;; package uses.
             (java-compound-identifier (: (group (* java-identifier ".")) (group java-identifier))))
      ;; MENTIONS is our return value. It's defined as a hash table,
      ;; to avoid duplicates.
      (let* ((mentions (make-hash-table :test #'equal))
             (parent-package (get-package (package-table-penv this) package-path))
             (local-files (remove (file-name-base package-path)
                                  (mapcar #'file-name-base (get-files this parent-package)))))
        (while (re-search-forward (rx java-compound-identifier) nil t)
          ;; Strangely, this is beneath the while-test, yet the
          ;; while-test still executes!
          (catch 'continue
            (let ((identifier (match-string-no-properties 0)))

              ;; Check import statements for globs.
              (when (and (equal identifier "import")
                         (string-match-p "\\.\\*;\\'"
                                         (get-current-line)))
                (let ((package (progn (re-search-forward (rx java-compound-identifier)
                                                         (line-end-position))
                                      (match-string-no-properties 0))))
                  (dolist (file (get-files this package))
                    (puthash (get-file (package-table-penv this) file :type 'package)
                             t
                             mentions))
                  (goto-char (line-end-position))
                  (throw 'continue nil)))

              ;; If a non-glob import, we still may as well skip the
              ;; word "import" itself (simplifies debugging).
              (when (equal identifier "import")
                (throw 'continue nil))

              ;; Similarly, skip package statement.
              (when (equal identifier "package")
                (goto-char (line-end-position))
                (throw 'continue nil))

              ;; Since this check doesn't depend on 'prefix' or
              ;; 'terminal' (see below), putting it here may let us
              ;; simplify what follows.
              (when (and (equal parent-package "default")
                         (member identifier (get-files this "default")))
                ;; For now, use this format for identifiers that
                ;; belong to the default package.
                (puthash (concat "default." identifier) t mentions)
                (throw 'continue nil))

              ;; Whatever else, will fall through to here.
              (let ((prefix (string-remove-suffix "." (match-string-no-properties 1)))
                    (terminal (match-string-no-properties 2)))
                (cond ((package-p this prefix)
                       (puthash identifier t mentions))
                      ((and (string-empty-p prefix)
                            (member identifier local-files))
                       (puthash (concat parent-package "." identifier) t mentions)))))))
        mentions))))

(cl-defmethod list-deps ((this project-environment) package-path)
  "Return the list of dependencies of PACKAGE-PATH, given a project
environment."
  (let* ((ptable (package-table-create this)))
    (hash-table-keys (find-dependencies ptable package-path))))

;; Graph prototype

(cl-defstruct (dependency-graph (:constructor dependency-graph--create))
              graph)

(cl-defmethod dependency-graph-create ((penv project-environment))
  (let ((graph (make-hash-table :test #'equal)))
    (dolist (package-path (project-environment-files penv) graph)
      (puthash package-path
               (cl-loop for pkg-dep in (list-deps penv package-path) collect
                     (cdr (assoc pkg-dep (project-environment-package-to-file-alist penv))))
               graph))
    (dependency-graph--create :graph graph)))

(cl-defmethod get-dependencies ((this dependency-graph) package-path)
  (let ((graph (dependency-graph-graph this)))
    (gethash package-path graph)))

(cl-defmethod get-modified-dependencies ((penv project-environment) package-path)
  (let* ((graph (dependency-graph-create penv))
         ;; Because of the potentially cyclic nature of Java
         ;; dependency graphs, we must keep track of visited nodes.
         (visited (make-hash-table :test #'equal))
         modified)
    (cl-labels ((add-to-modified-when-changed (dep)
                  (when (file-newer-than-file-p (get-file penv dep :type 'full)
                                                (get-file penv dep :type 'class))
                    (push dep modified)))
                (search (dependencies)
                  ;; DEP refers to the current recompilation candidate.
                  (dolist (dep dependencies)
                    (catch 'continue
                      (when (gethash dep visited)
                        (throw 'continue nil))
                      (puthash dep t visited)
                      (add-to-modified-when-changed dep)
                      (search (get-dependencies graph dep))))))
      (search (get-dependencies graph package-path))
      ;; It's possible that PACKAGE-PATH is its own indirect
      ;; dependency, and so was already visited! Note that this code
      ;; is now included _after_ the recursive search; else, VISITED
      ;; will have no entries, and this code-path will therefore
      ;; always be taken.
      (unless (gethash package-path visited)
        (add-to-modified-when-changed package-path)))
    modified))

;; Eshell integration.

(cl-defmethod generate-invocation ((penv project-environment) target class-subdir)
  (catch 'done
    (let* ((modified-dependencies (get-modified-dependencies penv target))
           (full-filenames (mapcar (lambda (moddep)
                                     (get-file penv moddep :type 'full))
                                   modified-dependencies)))
      (unless full-filenames
        (throw 'done nil))

      (let ((command (list (format "javac -g -cp \"lib/*:%s\" -d %s" class-subdir class-subdir))))
        (mapconcat #'identity (append command full-filenames) " ")))))

(defun eshell/setup-java-invocation (&rest args)
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
      (--setup-java-invocation project-root package-subdir class-subdir))))

(defun --setup-java-invocation (project-root package-subdir class-subdir)
  "Use an approach where the build and run commands are predefined
by this setup command."
  (let ((penv (project-environment-create (expand-file-name project-root) package-subdir class-subdir)))

    (defun eshell/java-build (target)
      "Insert the appropriate build command into the Eshell prompt."

      ;; Let's define this right away, so that we can use it to insert
      ;; the run command right away if there's nothing to build.
      (defun eshell/java-run ()
        "Insert the appropriate run command into the Eshell prompt."
        (eshell-kill-input)
        (let ((package-unit-no-extension (thread-last
                                           target
                                           (replace-regexp-in-string (rx "/") ".")
                                           (replace-regexp-in-string (rx ".java" eos) ""))))
          (insert (format "java -cp \"lib/*:bin\" %s" package-unit-no-extension))))


      ;; Generate the build command (or else insert the run command.)
      (eshell-kill-input)

      (let ((invocation (generate-invocation penv target class-subdir)))
        (if invocation
            (insert invocation)
          (eshell-print "Nothing to build; inserting run command\n----\n")
          (eshell/java-run))))))

(provide 'package-table)
