;; -*- lexical-binding: t; -*-

(defun main ()
  "A mock main function to test the current state of the
application."
  ;; Preamble
  (add-to-list 'load-path (expand-file-name ".") t)
  (require 'package-table)

  (let* ((penv (project-environment-create "~/eclipse-workspace2/UCSDGraphs/" "src/" "bin/"))
         (ptable (package-table-create penv)))
    (pretty-print ptable)
    (map-pairs (find-inline-dependencies ptable "application/MapApp.java"))))
