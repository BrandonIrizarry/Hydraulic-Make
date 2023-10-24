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
                   "gmapsfx.shapes"))))
