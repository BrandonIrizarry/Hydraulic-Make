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

(let ((penv (project-environment-create "~/eclipse-workspace2/UCSDGraphs/" "src/" "bin/")))
  (ert-deftest mapapp-deps ()
    "Examine the dependencies of 'application/MapApp.java' (UCSD
project)."
    :tags '(duke)
    (should (equal (list-deps penv "application/MapApp.java")
                   '("MarkerManager" "SelectManager" "geography.GeographicPoint" "CLabel" "DataSet" "gmapsfx.javascript.object.MapTypeIdEnum" "gmapsfx.javascript.object.MapOptions" "gmapsfx.javascript.object.LatLong" "gmapsfx.javascript.object.GoogleMap" "gmapsfx.MapComponentInitializedListener" "gmapsfx.GoogleMapView" "application.services.RouteService" "application.services.GeneralService" "application.controllers.RouteController" "application.controllers.FetchController")))))

;;; WEATHER DATA (DUKE)
(let ((penv (project-environment-create "~/Java/DukeIntro/Course_2/parsing_weather_data/" "" "bin/")))
  (ert-deftest weather-default-package-p ()
    "'Weather.java' should be in the default package, which should be
the empty string."
    :tags '(duke)
    (should (equal "default"
                   (get-package penv "Weather.java"))))

  (ert-deftest weather-deps ()
    "Examine the dependencies of '~/Java/DukeIntro/Course_2/parsing_weather_data/Weather.java'."
    :tags '(duke)
    (should (equal (list-deps penv "Weather.java")
                   nil)))

  (ert-deftest check-package-table-weather ()
    "Check the (rather simple) package table used by this project."
    :tags '(duke)
    (let ((ptable (package-table-create penv)))
      (should (equal (map-pairs (package-table-table ptable))
                     '(("default" "Weather.java")))))))

;; WORDNGRAM (DUKE)

(let ((penv (project-environment-create "~/Java/DukeIntro/Course_4/week_3/WordNGramStarterProgram/" "" "bin/")))
  (ert-deftest check-package-table-ngram-main ()
    "Check the package table used by this project."
    :tags '(duke)
    (let ((ptable (package-table-create penv)))
      (should (equal (mapcar (lambda (pair) (sort pair #'string<))
                             (map-pairs (package-table-table ptable)))
                     '(("IMarkovModel.java" "Main.java" "MarkovWordOne.java" "MarkovWordTwo.java" "default"))))))

  (ert-deftest wordngram-deps ()
    "Examine the dependencies of '~/Java/DukeIntro/Course_4/week_3/WordNGramStarterProgram/Main.java'."
    :tags '(duke)
    (should (equal (list-deps penv "Main.java")
                   '("MarkovWordTwo" "IMarkovModel")))))

(let ((penv (project-environment-create "~/Java/FakeProject/" "src/" "bin/")))
  (ert-deftest conference-deps ()
    "Examine the dependencies of 'conference/Conference.java'."
    :tags '(me)
    (should (equal (list-deps penv "conference/Conference.java")
                   '("andalucia.Malaga" "andalucia.Sevilla")))))
