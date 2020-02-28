;;; init-my-org-latex.el --- my latex settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my|<setup|org>->latex-conf ()
  "Setter function for org-mode latex configurations"
  (setq
   ; Start org buffer with inline imgs displayed.
   org-startup-with-inline-images t
   org-latex-inline-image-rules   '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\|gif\\)\\'"))
   ; Enlarge the inline latex previews.
   org-format-latex-options       (plist-put org-format-latex-options :scale 1.6)
   ;; org-latex-pdf-process          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ;;                                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
   ;;                                  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   org-latex-pdf-process          '("latexmk -shell-escape -bibtex -f -pdf %f"
                                    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                    )
   )
  ;; To use different code block theme styles in export put in your org file header #+LaTeX_HEADER: \usemintedstyle{monokai}
  ;; Full list of styles is given by cmd "pygmentize -L styles"

  (require 'ox-bibtex)

  ;; reftex
  (use-package reftex
    :commands turn-on-reftex
    :init
    (progn
      (setq reftex-default-bibliography '("~/Dropbox/thesis/bibliography/references.bib"))
      (setq reftex-plug-intoAUCTex t))
    )
  (use-package org-ref
    :after org
    :init
    (setq reftex-default-bibliography '("~/Dropbox/thesis/bibliography/references.bib"))
    ;; see org-ref for use of these variables
    (setq org-ref-bibliography-notes "~/Dropbox/thesis/bibliography/notes.org"
          org-ref-default-bibliography '("~/Dropbox/thesis/bibliography/references.bib")
          org-ref-pdf-directory "~/Dropbox/thesis/bibliography/bibtex-pdfs/")
    )

;  (add-to-list 'org-latex-packages-alist '("Lenny" "fncychap" t))
  ;; Enable syntax highlighting in pdf src code blocks
  (add-to-list 'org-latex-packages-alist '("" "minted" t))

  (setq org-latex-listings 'minted)

  (setq bibtex-completion-bibliography "~/Dropbox/thesis/bibliography/references.bib"
        bibtex-completion-library-path "~/Dropbox/thesis/bibliography/bibtex-pdfs"
        bibtex-completion-notes-path "~/Dropbox/thesis/bibliography/helm-bibtex-notes")

  ;; open pdf with system pdf viewer (works on mac)
 ;; (setq bibtex-completion-pdf-open-function
 ;;        (lambda (fpath)
 ;;          (start-process "open" "*open*" "open" fpath)))

  (defun org-export-latex-no-toc (depth)
    (when depth
      (format "%% Org-mode is exporting headings to %s levels.\n"
              depth)))
  (setq org-export-latex-format-toc-function 'org-export-latex-no-toc)

  ;; Add an "ebook style" looking pdf export.
  ;;
  ;; Use #+LaTeX_CLASS: ebook
  ;; (or some other predefined org-latex-class)
  ;; If you want your notes to be formatted to a certain specification.
  (with-eval-after-load 'ox-latex
    (add-to-list 'org-latex-classes '("ebook"
                                      "\\documentclass[11pt, oneside]{memoir}
                                      \\setstocksize{9in}{6in}
                                      \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
                                      \\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
                                      \\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
                                      \\checkandfixthelayout
                                      \\usepackage{times}
                                      \\usepackage[british]{babel}
                                      \\usepackage[raggedright]{sidecap}
                                      \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
                                      \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
                                      \\usepackage[labelformat=empty, font=small]{caption}
                                      \\usepackage{pdfpages}
                                      \\usepackage[unicode=true,
                                      bookmarks=true,bookmarksnumbered=false,bookmarksopen=true,bookmarksopenlevel=1,
                                      breaklinks=true,pdfborder={0 0 0},backref=false,colorlinks=false,pdfborderstyle={/S/U/W .5}, allbordercolors={.8 .8 .8}]
                                      {hyperref}
                                      \\pagestyle{myheadings}
                                      \\setcounter{tocdepth}{0}
                                      \\usepackage{ccicons}
                                      \\OnehalfSpacing
                                      \\usepackage[authoryear]{natbib}"
                                      ("\\chapter{%s}" . "\\chapter*{%s}")
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")

                                      ))

    (add-to-list 'org-latex-classes
                 '("elsarticle"
                   "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


    (add-to-list 'org-latex-classes
                 '("mimosis"
                   "\\documentclass{mimosis}
                    \\usepackage[unicode=true,
                    bookmarks=true,bookmarksnumbered=false,bookmarksopen=true,bookmarksopenlevel=1,
                    breaklinks=true,pdfborder={0 0 0},backref=false,colorlinks=false,pdfborderstyle={/S/U/W .5}, allbordercolors={.8 .8 .8}]
                    {hyperref}
                    \\KOMAoptions{paper=letter}
 \\usepackage{lmodern}

 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\newcommand{\\mboxparagraph}[1]{\\paragraph{#1}\\mbox{}\\\\}
\\newcommand{\\mboxsubparagraph}[1]{\\subparagraph{#1}\\mbox{}\\\\}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                   ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("koma-article" "\\documentclass{scrartcl}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("koma-report" "\\documentclass{scrreprt}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ) ;; end of with-eval-after-load 'ox-latex

  ) ; end my|<setup|org>->latex-conf



(provide 'init-my-org-latex)
