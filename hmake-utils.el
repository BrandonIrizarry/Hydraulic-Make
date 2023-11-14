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

(defun hu-get-current-line (tmp-buffer)
  "In the current buffer, return the line POINT is on."
  (string-trim (buffer-substring-no-properties (line-beginning-position)
                                               (line-end-position))))

(provide 'hmake-utils)

;; Local Variables:
;; read-symbol-shorthands: (("hu-" . "hmake-utils-"))
;; End:
