;; -*- lexical-binding: t; -*-

(defun hu-strip-non-code-artefacts ()
  "Strip the current temporary buffer of non-code artefacts."
  (when (string-match-p (rx bos " *temp*") (buffer-name))
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
          (replace-match ""))))))

(defun hu-get-current-line ()
  "Return the line POINT is on in the current temporary buffer."
  (string-trim (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position))))

(provide 'hmake-utils)

;; Local Variables:
;; read-symbol-shorthands: (("hu-" . "hmake-utils-"))
;; End:
