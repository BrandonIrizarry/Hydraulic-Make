;; -*- lexical-binding: t; -*-

;;; TESTS FOR 'FILENAME-SELECTOR.EL'

(require 'project-environment)

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

;; Second suite
(let* ((filename-selector-create (create-project-environment "~/Java/DukeIntro/Course_2/parsing_weather_data/"))
       (full-filename "/home/demo/Java/DukeIntro/Course_2/parsing_weather_data/Weather.java")
       (filename-selector (funcall filename-selector-create full-filename)))

  (ert-deftest find-the-various-imports ()
    "There are several import statements in this file.

Note there is also no package declaration present."
    :tags '(filename-selector)
    (should (equal (sort (find-imports filename-selector) #'string<)
                   '("edu.duke.*" "java.io.File" "org.apache.commons.csv.*"))))

  (ert-deftest belongs-to-default-package ()
    "That is, it isn't marked as belonging to a package."
    (should (equal (filename-selector-package filename-selector)
                   "Weather"))))
