;; -*- lexical-binding: t; -*-

;;; TESTS FOR 'FILENAME-FORMATS.EL'

(require 'filename-formats)

(let* ((full-filename "~/eclipse-workspace2/UCSDGraphs/src/gmapsfx/shapes/CircleOptions.java")
       (filename-selector (filename-selector-create full-filename)))

  (ert-deftest full-selector-works ()
    :tags '(filename-formats)
    (should (equal (filename-selector-full filename-selector)
                   full-filename)))

  (ert-deftest simple-selector-works ()
    :tags '(filename-formats)
    (let ((simple-filename "gmapsfx/shapes/CircleOptions.java"))
      (should (equal (filename-selector-simple filename-selector)
                     simple-filename))))

  (ert-deftest class-selector-works ()
    :tags '(filename-formats)
    (let ((class-filename (thread-last
                            full-filename
                            (replace-regexp-in-string "src" "bin")
                            (replace-regexp-in-string "\\.java\\'" ".class"))))
      (should (equal (filename-selector-class filename-selector)
                     class-filename))))

  (ert-deftest package-selector-works ()
    :tags '(filename-formats)
    (let ((package-unit "gmapsfx.shapes.CircleOptions"))
      (should (equal (filename-selector-package filename-selector)
                     package-unit)))))