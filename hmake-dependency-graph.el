;; -*- lexical-binding: t; -*-

(require 'h-package-table)

(cl-defstruct (h-dependency-graph (:constructor h-dependency-graph--create))
  graph)

(cl-defmethod h-dependency-graph-create ((penv h-project-environment))
  (let ((graph (make-hash-table :test #'equal))
        (ptable (h-package-table-create penv)))
    (dolist (package-path (h-project-environment-files penv) graph)
      (puthash package-path
               (cl-loop for pkg-dep in (h-find-dependencies ptable package-path) collect
                     ;; Get back the file we need for the build
                     ;; command
                     (cdr (assoc pkg-dep (h-project-environment-package-to-file-alist penv))))
               graph))
    (h-dependency-graph--create :graph graph)))

(cl-defmethod h-get-dependencies ((this h-dependency-graph) package-path)
  (let ((graph (h-dependency-graph-graph this)))
    (gethash package-path graph)))

(cl-defmethod h-get-modified-dependencies ((penv h-project-environment) package-path)
  (let* ((graph (h-dependency-graph-create penv))
         ;; Because of the potentially cyclic nature of Java
         ;; dependency graphs, we must keep track of visited nodes.
         (visited (make-hash-table :test #'equal))
         modified)
    (cl-labels ((add-to-modified-when-changed (dep)
                  (when (file-newer-than-file-p (h-get-file penv dep :type 'full)
                                                (h-get-file penv dep :type 'class))
                    (push dep modified)))
                (search (dependencies)
                  ;; DEP refers to the current recompilation candidate.
                  (dolist (dep dependencies)
                    (catch 'continue
                      (when (gethash dep visited)
                        (throw 'continue nil))
                      (puthash dep t visited)
                      (add-to-modified-when-changed dep)
                      (search (h-get-dependencies graph dep))))))
      (search (h-get-dependencies graph package-path))
      ;; It's possible that PACKAGE-PATH is its own indirect
      ;; dependency, and so was already visited! Note that this code
      ;; is now included _after_ the recursive search; else, VISITED
      ;; will have no entries, and this code-path will therefore
      ;; always be taken.
      (unless (gethash package-path visited)
        (add-to-modified-when-changed package-path)))
    modified))

(provide 'h-dependency-graph)

;; Local Variables:
;; read-symbol-shorthands: (("h-" . "hmake-"))
;; End:
