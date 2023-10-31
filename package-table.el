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
      ;; A hash table is used to avoid duplicates.
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
                (puthash identifier t mentions)
                (throw 'continue nil))

              ;; Whatever else, will fall through to here.
              (let ((prefix (string-remove-suffix "." (match-string-no-properties 1)))
                    (terminal (match-string-no-properties 2)))
                (cond ((package-p this prefix)
                       (puthash identifier t mentions))
                      ((and (string-empty-p prefix)
                            (member identifier local-files))
                       (puthash identifier t mentions)))))))
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
      (puthash package-path (list-deps penv package-path) graph))
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
    (cl-labels ((search (dependencies)
                  ;; DEP refers to the current recompilation candidate.
                  (dolist (dep dependencies modified)
                    (unless (gethash dep visited)
                      (puthash dep t visited)
                      (when (file-newer-than-file-p (get-file penv dep :type 'full)
                                                    (get-file penv dep :type 'class))
                        (push dep modified))
                      (search (get-dependencies graph dep))))))
      (search (cons package-path (get-dependencies graph package-path))))
    (map-pairs modified)))

(provide 'package-table)
