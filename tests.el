;; -*- lexical-binding: t; -*-

;;; TESTS FOR 'FILENAME-SELECTOR.EL'

(add-to-list 'load-path (expand-file-name ".") t)
(require 'package-table)

(let* ((project-root "~/eclipse-workspace2/UCSDGraphs/")
       (package-root "~/eclipse-workspace2/UCSDGraphs/src/")
       (class-root "~/eclipse-workspace2/UCSDGraphs/bin/")
       (example-file "gmapsfx/shapes/CircleOptions.java")
       (example-class "gmapsfx/shapes/CircleOptions.class")
       (example-package-unit "gmapsfx.shapes.CircleOptions")
       (penv (project-environment-create project-root "src/" "bin/")))

  (ert-deftest get-full-filename ()
    (should (equal (get-file penv example-file :type 'full)
                   (concat package-root example-file))))

  (ert-deftest get-path-to-class-file ()
    (should (equal (get-file penv example-file :type 'class)
                   (concat class-root example-class))))

  (ert-deftest get-package-unit ()
    (should (equal (get-file penv example-file :type 'package)
                   "gmapsfx.shapes.CircleOptions")))

  (ert-deftest get-package ()
    (should (equal (get-package penv example-file)
                   "gmapsfx.shapes")))

  (ert-deftest get-basename ()
    (should (equal (get-file penv example-file :type 'basename)
                   "CircleOptions"))))

(ert-deftest mapapp-deps ()
  "A mock main function to test the current state of the
application."
  :tags '(main)
  (add-to-list 'load-path (expand-file-name ".") t)
  (require 'package-table)

  (let* ((penv (project-environment-create "~/eclipse-workspace2/UCSDGraphs/" "src/" "bin/"))
         (ptable (package-table-create penv)))
    (should (equal (map-pairs (find-inline-dependencies ptable "application/MapApp.java"))
                   '(("application.controllers" . t)
                     ("application.services" . t)
                     ("gmapsfx" . t)
                     ("gmapsfx.javascript.object" . t)
                     ("geography" . t))))))
