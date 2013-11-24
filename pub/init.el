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
