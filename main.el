;; -*- lexical-binding: t; -*-


(defun main (project-root src bin)
  (let* ((penv (project-environment-create project-root src bin)))
    (map-pairs (dependency-graph-graph (dependency-graph-create penv)))))
