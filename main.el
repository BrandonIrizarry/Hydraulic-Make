;; -*- lexical-binding: t; -*-

(defvar ucsd-root "~/eclipse-workspace2/UCSDGraphs/")

(cl-defmethod main ((penv project-environment) entry-point)
  (get-modified-dependencies penv entry-point))


(main (project-environment-create ucsd-root "src/" "bin/")
      "application/MapApp.java")
