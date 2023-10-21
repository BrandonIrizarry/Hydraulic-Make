;; -*- lexical-binding: t; -*-

(defvar *java-project-root*
  "~/eclipse-workspace2/UCSDGraphs/"
  "The top of the UCSDGraphs project hierarchy. Note that the actual
packages are under 'src'.")

(defvar *java-project-package-root*
  (concat *java-project-root* "src/")
  "The top of the UCSDGraphs package hierarchy.")

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";"))
    (match-string-no-properties 1)))

(defun generate-package-table ()
  "Return a hash table mapping a package to the list of files
it encompasses."
  (let ((known-packages (make-hash-table :test #'equal))
        (all-files (directory-files-recursively *java-project-package-root*
                                                (rx bol (not (any ".#")) (* not-newline) ".java" eol))))
    (dolist (file all-files known-packages)
      (let* ((basic-name
              (progn
                ;; Extract into BASIC-NAME the path that would
                ;; correspond to the fully qualified package name.
                (string-match (rx-to-string `(seq bol ,*java-project-package-root* (group (+ not-newline)) eol))
                              file)
                (match-string-no-properties 1 file)))
             ;; This "opens" the file to discover what the package
             ;; declaration looks like inside it.
             (package-name (find-package-name file))
             (file-list (gethash package-name
                                 known-packages
                                 ;; The default if PACKAGE-NAME
                                 ;; hasn't been added yet
                                 (list))))
        (push basic-name file-list)
        (puthash package-name file-list known-packages)))))




(require 'ert)

(ert-deftest fetch-package-files-for-roadgraph ()
  (let ((files (mapcar (lambda (name) (format "roadgraph/%s.java" name))
                       '("AStarGrader" "CorrectAnswer" "DijkstraGrader" "MapGraph" "SearchGrader")))
        (package-table (generate-package-table)))
    (should (equal (sort (gethash "roadgraph" package-table) #'string<)
                   (sort files #'string<)))))

(ert-deftest mapapp-in-package-application ()
  (should (equal (find-package-name (concat *java-project-package-root* "application/MapApp.java"))
                 "application")))
