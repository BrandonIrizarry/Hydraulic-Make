;; -*- lexical-binding: t; -*-

(require 'h-dependency-graph)
(require 'ert-fixtures)

(defvar ht-ucsd-defs
  (efs-define-fixture
      ((project-root "~/eclipse-workspace2/UCSDGraphs/")
       (package-root "~/eclipse-workspace2/UCSDGraphs/src/")
       (class-root "~/eclipse-workspace2/UCSDGraphs/bin/")
       (example-file "gmapsfx/shapes/CircleOptions.java")
       (example-class "gmapsfx/shapes/CircleOptions.class")
       (example-package-unit "gmapsfx.shapes.CircleOptions")
       (penv (h-project-environment-create project-root "src/" "bin/"))))
  "A UCSD project definition with some auxiliary data provided.")

(efs-use-fixtures get-full-filename (ht-ucsd-defs)
  "Test getting the full filename from the given relative path."
  :tags '(file-format)
  (should (equal (h-get-file penv example-file :type 'full)
                 (concat package-root example-file))))

(efs-use-fixtures get-path-to-class-file (ht-ucsd-defs)
  "Test getting the class file from the given relative path."
  :tags '(file-format)
  (should (equal (h-get-file penv example-file :type 'class)
                 (concat class-root example-class))))

(efs-use-fixtures get-package-unit (ht-ucsd-defs)
  "Test getting the fully qualified name from the given relative
path."
  :tags '(file-format)
  (should (equal (h-get-file penv example-file :type 'package)
                 "gmapsfx.shapes.CircleOptions")))

(efs-use-fixtures get-package (ht-ucsd-defs)
  "Test getting the parent package from the given relative path."
  :tags '(file-format)
  (should (equal (h-get-package penv example-file)
                 "gmapsfx.shapes")))

(efs-use-fixtures get-basename (ht-ucsd-defs)
  "Test getting the basename from the given relative path."
  :tags '(file-format)
  (should (equal (h-get-file penv example-file :type 'basename)
                 "CircleOptions")))


(efs-use-fixtures mapapp-deps (ht-ucsd-defs)
  "Examine the dependencies of 'application/MapApp.java' (UCSD
project)."
  :tags '(duke)
  (should (equal (h-list-deps penv "application/MapApp.java")
                 '("application.MarkerManager" "application.SelectManager" "geography.GeographicPoint" "application.CLabel" "application.DataSet" "gmapsfx.javascript.object.MapTypeIdEnum" "gmapsfx.javascript.object.MapOptions" "gmapsfx.javascript.object.LatLong" "gmapsfx.javascript.object.GoogleMap" "gmapsfx.MapComponentInitializedListener" "gmapsfx.GoogleMapView" "application.services.RouteService" "application.services.GeneralService" "application.controllers.RouteController" "application.controllers.FetchController"))))

;;; WEATHER DATA (DUKE)

(defvar ht-duke-weather-defs
  (efs-define-fixture
      ((penv (h-project-environment-create "~/Java/DukeIntro/Course_2/parsing_weather_data/" "" "bin/")))))


(efs-use-fixtures weather-default-package-p (ht-duke-weather-defs)
  "'Weather.java' should be in the default package, which should be
the empty string."
  :tags '(duke)
  (should (equal "default"
                 (h-get-package penv "Weather.java"))))

(efs-use-fixtures weather-deps (ht-duke-weather-defs)
  "Examine the dependencies of '~/Java/DukeIntro/Course_2/parsing_weather_data/Weather.java'."
  :tags '(duke)
  (should (equal (h-list-deps penv "Weather.java")
                 nil)))

(efs-use-fixtures check-package-table-weather (ht-duke-weather-defs)
  "Check the (rather simple) package table used by this project."
  :tags '(duke)
  (let ((ptable (h-package-table-create penv)))
    (should (equal (map-pairs (h-package-table-table ptable))
                   '(("default" "Weather.java"))))))

;; WORDNGRAM (DUKE)

(defvar ht-duke-wordgram-defs
  (efs-define-fixture
      ((penv (h-project-environment-create "~/Java/DukeIntro/Course_4/week_3/WordNGramStarterProgram/" "" "bin/")))))


(efs-use-fixtures check-package-table-ngram-main (ht-duke-wordgram-defs)
  "Check the package table used by this project."
  :tags '(duke)
  (let ((ptable (h-package-table-create penv)))
    (should (equal (mapcar (lambda (pair) (sort pair #'string<))
                           (map-pairs (h-package-table-table ptable)))
                   '(("IMarkovModel.java" "Main.java" "MarkovWordOne.java" "MarkovWordTwo.java" "default"))))))

(efs-use-fixtures wordngram-deps (ht-duke-wordgram-defs)
  "Examine the dependencies of '~/Java/DukeIntro/Course_4/week_3/WordNGramStarterProgram/Main.java'."
  :tags '(duke deps)
  (should (equal (h-list-deps penv "Main.java")
                 '("default.MarkovWordTwo" "default.IMarkovModel"))))

(defvar ht-fakeproject-defs
  (efs-define-fixture
      ((penv (h-project-environment-create "~/Java/FakeProject/" "src/" "bin/")))))

(efs-use-fixtures conference-deps (ht-fakeproject-defs)
  "Examine the dependencies of 'conference/Conference.java'."
  :tags '(me deps)
  (should (equal (h-list-deps penv "conference/Conference.java")
                 '("asturias.Cangas" "asturias.Gijon" "andaluz.Malaga" "andaluz.Sevilla"))))

;; Dependency graph.
(defvar ht-ucsd-dep-graph (efs-define-fixture ((graph (h-dependency-graph-create penv)))))

(efs-use-fixtures create-dep-graph (ht-ucsd-defs ht-ucsd-dep-graph)
  :tags '(ucsd edebug)
  (h-get-dependencies graph "application/MapApp.java")
  (h-get-dependencies graph "application/RouteVisualization.java"))

(efs-use-fixtures check-modified-deps (ht-ucsd-defs)
  :tags '(ucsd edebug)
  (h-get-modified-dependencies penv "application/MapApp.java"))

;; Local Variables:
;; read-symbol-shorthands: (("efs-" . "ert-fixtures-") ("h-" . "hmake-") ("ht-" . "hmake-tests-"))
;; End:
