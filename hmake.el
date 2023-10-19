;; -*- lexical-binding: t; -*-

(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";"))
    (match-string-no-properties 1)))

(defvar *known-packages* (make-hash-table :test #'equal))

(defvar *java-project-root*
  "~/eclipse-workspace2/UCSDGraphs/src")

(let ((all-files
       (directory-files-recursively *java-project-root* "^[^.#].*\.java$")))
  (cl-loop for file in all-files collect
        (let ((basic-name
               (progn
                 (string-match "^.+/src/\\\(.+\\\)$" file)
                 (match-string-no-properties 1 file)))
              (package-name (find-package-name file)))
          (puthash package-name t *known-packages*)
          (cons basic-name package-name))))
