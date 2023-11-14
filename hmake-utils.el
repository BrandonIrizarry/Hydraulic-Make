;; -*- lexical-binding: t; -*-

(defun hu-strip-non-code-artefacts ()
  "Strip the current buffer of non-code artefacts."
  (save-excursion
    (rx-let ((java-multi-line-comment
              (: "/*" (*
                       (| (not "*")
                          (: "*" (not "/"))))
                 (+ "*") "/"))
             (java-line-comment
              (: "//" (* not-newline) eol))
             (java-string
              (: ?\" (*? anything) ?\")))
      (while (re-search-forward (rx (or java-multi-line-comment
                                        java-line-comment
                                        java-string))
                                nil t)
        (replace-match "")))))

;; DEPRECATED (use 'get-rest-of-line')
(defun hu-get-current-line ()
  "In the current buffer, return the line POINT is on."
  (string-trim (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position))))

(defun hu-get-rest-of-line ()
  "In the current buffer, return the text on the given line from
point to the end of the line.

Trim the text of surrounding whitespace, and remove the trailing
semicolon if present."
  (thread-last
     (buffer-substring-no-properties (point)
                                     (line-end-position))
     string-trim
     (string-remove-suffix ";")))

;; Matches Java identifiers.
(rx-define hu-java-identifier (: (any alpha "_") (* (any alnum "_"))))

;; Matches either a Java identifier, or a chain of them strung
;; together with ".". This would match the occurrences of the various
;; types visible to the current file.
;;
;; Within a compound identifier, the set of "^(IDENTIFIER.)*", without
;; the final dot, is a _prefix_. The prefix encompassing just before
;; the terminal identifier is a _penultimate prefix_. Given a prefix
;; PREFIX matching "^(IDENTIFIER.)*REST", the part before REST is the
;; _parent prefix_ of PREFIX.
(rx-define hu-java-compound-identifier (: (group (* hu-java-identifier ".")) hu-java-identifier))

(defun hu-get-successive-prefixes (identifier)
  "Return a list of successive prefixes within the compound
identifier IDENTIFIER.

The idea is to later check each one to see if it's a valid
package. The first one that isn't marks where to extract the
parent prefix as the sought-after dependency."
  (with-temp-buffer
    (insert (string-trim identifier))
    (let* ((beginning (goto-char (point-min)))
           (current-pos beginning)
           (prefixes (list)))
      (while (setq current-pos (re-search-forward (rx hu-java-identifier) nil t))
        (push (buffer-substring-no-properties beginning current-pos)
              prefixes))
      (nreverse prefixes))))

;; DEPRECATED (use 'get-successive-prefixes' with 'nth-last')
(defun hu-get-penultimate-prefix (identifier)
  "Return the penultimate prefix of IDENTIFIER."
  (string-match (rx hu-java-compound-identifier) identifier)
  (let ((penultimate-prefix (match-string-no-properties 1 identifier)))
    (string-remove-suffix "." penultimate-prefix)))

(defun hu-nth-last (n lst)
  "Return the N-th element from the end of LST.

Return NIL if out-of-bounds."
  (let ((len (length lst)))
    (when (<= n len)
      (nth (- (length lst) n) lst))))

(provide 'hmake-utils)

;; Local Variables:
;; read-symbol-shorthands: (("hu-" . "hmake-utils-"))
;; End:
