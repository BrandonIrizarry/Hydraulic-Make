;; -*- lexical-binding: t; -*-

;;; TESTS FOR 'FILENAME-SELECTOR.EL'

(require 'filename-selector)

(let* ((full-filename "~/eclipse-workspace2/UCSDGraphs/src/gmapsfx/shapes/CircleOptions.java")
       (filename-selector (filename-selector-create full-filename)))

  (ert-deftest full-selector-works ()
    :tags '(filename-selector)
    (should (equal (filename-selector-full filename-selector)
                   full-filename)))

  (ert-deftest simple-selector-works ()
    :tags '(filename-selector)
    (let ((simple-filename "gmapsfx/shapes/CircleOptions.java"))
      (should (equal (filename-selector-simple filename-selector)
                     simple-filename))))

  (ert-deftest class-selector-works ()
    :tags '(filename-selector)
    (let ((class-filename (thread-last
                            full-filename
                            (replace-regexp-in-string "src" "bin")
                            (replace-regexp-in-string "\\.java\\'" ".class"))))
      (should (equal (filename-selector-class filename-selector)
                     class-filename))))

  (ert-deftest package-selector-works ()
    :tags '(filename-selector)
    (let ((package-unit "gmapsfx.shapes.CircleOptions"))
      (should (equal (filename-selector-package filename-selector)
                     package-unit))))

  (ert-deftest get-package-prefix-works ()
    :tags '(filename-selector)
    (should (equal (get-package-prefix filename-selector)
                   "gmapsfx.shapes")))

  (ert-deftest demonstrate-get-lines ()
    :tags '(filename-selector)
    (let ((lines '("package gmapsfx.shapes;" "import gmapsfx.javascript.object.LatLong;" "public class CircleOptions extends FillableMapShapeOptions<CircleOptions> {" "private LatLong center;" "private double radius;" "public CircleOptions center(LatLong center) {" "setProperty(, center);" "this.center = center;" "return this;" "}" "public CircleOptions radius(double radius) {" "setProperty(, radius);" "this.radius = radius;" "return this;" "}" "@Override" "protected CircleOptions getMe() {" "return this;" "}" "}")))
      (cl-flet ((nonempty-string-p (str) (not (string-empty-p str)))
                (quote-character-p (char) (eql char ?\")))
        (should (equal (get-lines filename-selector)
                       lines))
        (should (cl-every #'nonempty-string-p lines))
        (should (cl-notany #'quote-character-p lines )))))

  (ert-deftest find-the-single-import ()
    "The file used by this test suite has only one 'import' statement:
let's see if our code correctly discovers the corresponding package."
    :tags '(filename-selector)
    (should (equal (find-imports filename-selector)
                   '("gmapsfx.javascript.object.LatLong"))))

  (ert-deftest basename-works ()
    (should (equal (filename-selector-basename filename-selector)
                   "CircleOptions"))))

;; Second suite
(let* ((full-filename "/home/demo/Java/DukeIntro/Course_2/parsing_weather_data/Weather.java")
       (filename-selector (filename-selector-create full-filename)))

  (ert-deftest find-the-various-imports ()
    "There are several import statements in this file.

Note there is also no package declaration present."
    :tags '(filename-selector)
    (should (equal (sort (find-imports filename-selector) #'string<)
                   '("edu.duke.*" "java.io.File" "org.apache.commons.csv.*"))))

  (ert-deftest belongs-to-default-package ()
    "That is, it isn't marked as belonging to a package."
    (should-not (filename-selector-package filename-selector))))
