(mapc #'require '(org cl ob-latex ox-html ox-latex ox-bibtex))

(setq org-confirm-babel-evaluate nil)
(setq org-babel-latex-htlatex "htlatex")

(add-to-list 'org-latex-classes
             `("sigcomm-alternative"
               ,(with-temp-buffer
                  (insert-file-literally "sigcomm-alternative.tex")
                  (buffer-string))
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defmacro by-backend (&rest body)
  `(case (if (boundp (quote backend))
             (org-export-backend-name backend) nil) ,@body))

(defun org-bibtex-group-citations (text backend info)
  "Convert begin/end{verbatim} to begin/end{Verbatim}.
    Allows use of the fancyvrb latex package."
  (with-temp-buffer
    (insert text) (goto-char (point-min))
    (cond
     ((org-export-derived-backend-p backend 'latex)
      (while (re-search-forward
              "\\\\cite{\\([^[:space:]\n\r]+\\)}[[:space:]\n\r]*\\\\cite{"
              nil t)
        (replace-match "\\\\cite{\\1,")
        (goto-char (point-min))))
     ((org-export-derived-backend-p backend 'html)
      (while (re-search-forward
              "\\(#[[:alnum:]]+\">[0-9]+<\/a>\\)\\][[:space:]\n\r]*\\[\\(<a href=\"#[[:alnum:]]+\">[0-9]+<\/a>\\)"
              nil t)
        (replace-match "\\1,\\2")
        (goto-char (point-min)))))
    (buffer-string)))

(add-to-list 'org-export-filter-final-output-functions
             'org-bibtex-group-citations)
