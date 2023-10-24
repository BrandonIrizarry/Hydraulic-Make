;; -*- lexical-binding: t; -*-

(defvar *java-project-root* "~/eclipse-workspace2/UCSDGraphs/"
    "The top of the UCSDGraphs project hierarchy.")

(defvar *java-project-package-root* (concat *java-project-root* "src/")
  "An immediate subdirectory of the project root, serving as the
top-level package directory.")

(defvar *java-project-class-root* (concat *java-project-root* "bin/")
  "An immediate subdirectory of the project root, where class files
are stored in a manner reflecting the package hierarchy.")

(cl-defstruct (filename-selector (:constructor filename-selector--create))
  "A record summarizing the four different formats of the name of a
Java source file:

1. the full path (full)
2. the relative path corresponding to the package name (simple)
3. the full path to the corresponding class file (class)
4. the fully-qualified package name (package-unit)"
  full simple class package)

;; FIXME: add stripping.
(defun find-package-name (full-filename)
  "Return the package FULL-FILENAME belongs to, as a string."
  (with-temp-buffer
    (insert-file full-filename)
    (goto-char (point-min))
    (re-search-forward (rx "package" (+ space) (group (+ not-newline)) ";") nil t)
    (match-string-no-properties 1)))

;; FIXME: use 'string-remove-prefix'
(defun filename-selector-create (full-filename)
  "Public constructor for FILENAME-SELECTOR objects."
  (let* ((simple-filename (replace-regexp-in-string (rx-to-string `(seq bos ,*java-project-package-root*))
                                                    ""
                                                    full-filename))
         (class-filename (let ((partial-path (concat *java-project-class-root* simple-filename)))
                           (replace-regexp-in-string (rx ".java" eos) ".class" partial-path)))
         (package-unit (format "%s.%s"
                               (find-package-name full-filename)
                               (file-name-base full-filename))))
    (filename-selector--create :full full-filename :simple simple-filename :class class-filename :package package-unit)))

(cl-defmethod get-package-prefix ((this filename-selector))
  "Given a FILENAME-SELECTOR object, return the package prefix
associated with it."
  (let ((package-unit (filename-selector-package this)))
    ;; Weird, but works.
    (mapconcat #'identity
               (butlast (string-split package-unit (rx ".")))
               ".")))

;;; RX definitions to help us strip comments from our working temp
;;; buffer.
;;;
;;; These are defined globally, to ease debugging.

(rx-define java-multi-line-comment
    (: "/*" (*
             (| (not "*")
                (: "*" (not "/"))))
     (+ "*") "/"))

(rx-define java-line-comment
    (: "//" (* not-newline) eol))

(rx-define java-identifier
    (: (any alpha "_") (* (any alnum "_"))))

(rx-define java-string
    (: ?\" (* anything) ?\"))

(cl-defmethod get-lines ((this filename-selector))
  "Return the (non-empty) lines of FULL-FILENAME, after first having
stripped away comments."
  (with-temp-buffer
    (insert-file (filename-selector-full this))
    (let ((content (thread-last
                     (buffer-string)
                     (replace-regexp-in-string (rx java-multi-line-comment) "")
                     (replace-regexp-in-string (rx java-line-comment) "")
                     (replace-regexp-in-string (rx java-string) ""))))
      (thread-last
        (string-split content "\n" t)
        (mapcar #'string-trim)
        (seq-filter (lambda (line) (not (string-empty-p line))))))))

;; FIXME: we should probably refactor "stripping" to its own method,
;; and also do it for the package-finding code above
;;
;; FIXME: verify that glob expansion is really performed by the
;; package-table object.
(cl-defmethod find-imports ((this filename-selector))
  "Extract the contents of all import statements in THIS as a list
of package units.

Note that if the package unit is a glob, it remains unexpanded:
glob expansion is the job of the PACKAGE-TABLE object."
  (with-temp-buffer
    (insert-file (filename-selector-full this))
    (let ((content (thread-last
                     (buffer-string)
                     (replace-regexp-in-string (rx java-multi-line-comment) "")
                     (replace-regexp-in-string (rx java-line-comment) "")
                     (replace-regexp-in-string (rx java-string) ""))))
      (goto-char (point-min))
      (let (package-names)
        (while (re-search-forward (rx "import" (+ space) (group (+ not-newline)) ";") nil t)
          (push (match-string-no-properties 1) package-names))
        package-names))))

(provide 'filename-selector)