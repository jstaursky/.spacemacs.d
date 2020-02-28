;;; init-my-org.el --- my org settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package visual-fill-column-mode
;;   :init (add-hook 'org-mode-hook '(lambda () (setq word-wrap t truncate-lines nil)))
;;   :hook org-mode
;;   )

(global-set-key (kbd "<f6>") '(lambda () (interactive)
                                (emms-seek +10)))

(global-set-key (kbd "<f4>") '(lambda () (interactive)
                                (emms-seek -10)))

;; Defining with the same key enables toggling behavior.
(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (emms-start)))
(global-set-key (kbd "<f5>") '(lambda () (interactive)
                                (emms-pause)))


;; ---------------------------------------------------------------------------
(use-package org ;; ----------------------------------------------------------
  :init

  ;; Make evil-mode up/down operate in screen lines instead of logical lines.
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  ;; Also in visual mode.
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  (spacemacs/declare-prefix-for-mode 'org-mode "mo" "my org")
  (spacemacs/declare-prefix-for-mode 'org-mode "mot" "time tracking")
  (spacemacs/declare-prefix-for-mode 'org-mode "mos" "scheduling")

  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "op" 'org-set-property
    "oT" 'org-set-tags
    "oss" 'org-schedule               ;; (s)cheduling (s)chedule
    "osd" 'org-deadline               ;; (s)cheduling (d)eadline
    "osw" 'my|adjust-deadline-prewarning

    "ott" 'org-todo
    "otb" 'org-clock-in               ;; (t)ime (b)egin
    "ote" 'org-clock-out              ;; (t)ime (e)nd
    "otc" 'org-clock-cancel           ;; (t)ime (c)ancel
    "otd" 'org-clock-display
    ;; NOTE to remove overlay introduced by org-clock-display type: C-c C-c
    )

  (add-hook 'org-trigger-hook 'save-buffer) ; Save file after toggling a TODO state.

  :config
  (require 'emms)
  (require 'org-emms)


  ;; Replace dashes in org mode with bullet points.
  (add-hook 'org-mode-hook '(lambda ()
                              (font-lock-add-keywords
                               nil
                               '(
                                 ("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
                                 ("NOTE" . font-lock-warning-face) ;; Make NOTE a fontified word in org.
                                 ))
                              ))

  (add-hook 'org-mode-hook 'auto-fill-mode)


  (defmacro η (fnc)
    "Return function that ignores its arguments and invokes FNC."
    `(lambda (&rest _rest)
       (funcall ,fnc)))

  (advice-add 'org-deadline       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-schedule       :after (η #'org-save-all-org-buffers))
  (advice-add 'org-store-log-note :after (η #'org-save-all-org-buffers))
  (advice-add 'org-todo           :after (η #'org-save-all-org-buffers))

  (org-add-link-type
   "attachfile"
   (lambda (link-string) (org-open-file link-string))
   ;; formatting
   (lambda (keyword desc format)
     (cond
      ((eq format 'html) (format "")); no output for html
      ((eq format 'latex)
       ;; write out the latex command
       (format "\\attachfile{%s}" keyword)))))



  (add-to-list 'org-modules 'org-habit t)
  (with-eval-after-load "org-habit"
    (setq org-habit-graph-column               61
          org-habit-show-habits-only-for-today t)
    )

  (setq org-startup-folded         nil  ; Do not start org outline as folded.
        org-startup-indented       t    ; Indent text according to outline structure.
        org-hide-emphasis-markers  t    ; Hide *bold*, _under_, etc, markers.
        org-src-fontify-natively   t    ; fontify code in code blocks.
        )

  (setq-default org-default-notes-file "~/Dropbox/org/Agenda/tasks.org")
  (setq org-directory "~/Dropbox/org/Agenda")
  (setq org-agenda-files (list org-directory))

  ;; (setq org-agenda-files `(,(concat user-home-directory "Dropbox/Org/tasks.org")
  ;;                          ,(concat user-home-directory "Dropbox/School/FALL2019/Fall2019.org")
  ;;                        ; "~/Dropbox/Planning/MalwareJOB/1_ schedule/work.org"
  ;;                          ))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN PROGRESS(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


  ;; ** ORG AGENDA -------------------------------------------------------------
  ;; ---------------------------------------------------------------------------
  (setq ; org-agenda-start-day         "-3d"
        org-agenda-start-on-weekday  nil ; org-agenda view starts today and +7
        org-agenda-span              10
        spaceline-org-clock-p        t   ; Show running clock in modeline.
        org-agenda-default-appointment-duration  60
        org-agenda-timegrid-use-ampm             t
        org-agenda-show-current-time-in-grid     t
        org-agenda-use-time-grid                 t
        )

  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown  (quote repeated-after-deadline))

  (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-start-with-log-mode '(closed clock state))

  (setq org-agenda-sorting-strategy
        '((agenda time-up priority-down category-keep)
          (todo   priority-down category-keep)
          (tags   priority-down category-keep)
          (search category-keep)))

  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t\t% s") ;► 
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))


  (setq org-agenda-deadline-leaders  '("DEADLINE！ "
                                       "DUE IN:%2dD "
                                       "PASTDUE！:%2dD "))
  (setq org-agenda-scheduled-leaders '(""
                                       "x%-2d(RS) " ; # times item has been (R)e(S)cheduled
                                       ))

  (setq org-agenda-time-grid '((remove-match daily today require-timed)
                               (600 800 1000 1200 1400 1600 1800 2000 2200 2400)
                               "......" "----------------"))

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))


  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "")
            (alltodo ""
                     ((org-agenda-skip-function
                       '(or (air-org-skip-subtree-if-priority ?A)
                            (org-agenda-skip-if nil '(scheduled deadline))))))))))


 (defun my|adjust-deadline-prewarning (pom) ;; pom = point or mark
    (interactive "P")
    (let* ((current-prefix-arg '(16))   ; emulates "C-u C-u"
           (input (concat "--" (number-to-string (org-time-stamp-to-now
                                                        (org-entry-get pom "DEADLINE")))))
           )
      (minibuffer-with-setup-hook
          (lambda ()
            (minibuffer-input-provider (list input)))
        (call-interactively 'org-deadline)
        )))

 (advice-add 'org-deadline :after #'my|adjust-deadline-prewarning)

  ;; ** ORG BABEL -------------------------------------------------------------
  ;; ---------------------------------------------------------------------------

  ;; Prevent org-babel from asking to confirm every code eval.
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)      (calc . t)   (C . t)       (emacs-lisp . t)  (haskell . t)
     (gnuplot . t)  (latex . t)  (ledger . t)  (scheme . t)      (perl . t)
     (python . t)   (R . t)      (js . t)      (gnuplot . t)     (shell . t)
     (sql . t)
     ))

  ;; ** ORG LATEX  -------------------------------------------------------------
  ;; ---------------------------------------------------------------------------

  ;; Call Org latex setter function.
  (require 'init-my-org-latex)
  (my|<setup|org>->latex-conf)

  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))


  (org-add-link-type "cite"
                     (defun follow-cite (name)
                       "Open bibliography and jump to appropriate entry.
        The document must contain \bibliography{filename} somewhere
        for this to work"
                       (find-file-other-window
                        (save-excursion
                          (beginning-of-buffer)
                          (save-match-data
                            (re-search-forward "\\\\bibliography{\\([^}]+\\)}")
                            (concat (match-string 1) ".bib"))))

                       (beginning-of-buffer)
                       (search-forward name))
                     (defun export-cite (path desc format)
                       "Export [[cite:cohen93]] as \cite{cohen93} in LaTeX."
                       (if (eq format 'latex)
                           (if (or (not desc) (equal 0 (search "cite:" desc)))
                               (format "\\cite{%s}" path)
                             (format "\\cite[%s]{%s}" desc path)))))

  ;; ** ORG TASK JUGGLER -------------------------------------------------------
  ;; ---------------------------------------------------------------------------
  (setq org-taskjuggler-default-reports
        '("textreport report \"Plan\" {
           formats html
           header '== %title =='
           center -8<-
             [#Plan Plan] | [#Resource_Allocation Resource Allocation]
             ----
             === Plan ===
             <[report id=\"plan\"]>
             ----
             === Resource Allocation ===
             <[report id=\"resourceGraph\"]>
           ->8-
           }
           # A traditional Gantt chart with a project overview.
           taskreport plan \"\" {
             headline \"Project Plan\"
             columns bsi, name, start, end, effort, effortdone, effortleft, complete, chart { scale day width 1000 }
             loadunit shortauto
             hideresource 1
           }
           # A graph showing resource allocation. It identifies whether each
           # resource is under- or over-allocated for.
           resourcereport resourceGraph \"\" {
             headline \"Resource Allocation Graph\"
             columns no, name, effort, daily { width 1000 }
             loadunit shortauto
             hidetask ~(isleaf() & isleaf_())
             sorttasks plan.start.up
           }")
        )




  (add-hook 'org-src-mode-hook 'rainbow-delimiters-mode-disable)

  :bind (:map org-mode-map
              ("C-c f" . my|org-hide-emphasis-markers)
              ;; Prevent the accidental adding and removing of files to 'org-agenda-files variable.
              ("C-c [" . nil)
              ("C-c ]" . nil)
              )
  ) ;; end of use-package org.................................................
;; ...........................................................................
;;             _                    __                  _   _
;;   /\  /\___| |_ __   ___ _ __   / _|_   _ _ __   ___| |_(_) ___  _ __  ___
;;  / /_/ / _ \ | '_ \ / _ \ '__| | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;; / __  /  __/ | |_) |  __/ |    |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;; \/ /_/ \___|_| .__/ \___|_|    |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;              |_|
;; Helper Functions
;;


(defun my|org-hide-emphasis-markers ()
  "Toggle whether or not the emphasis markers ~, =, *, _ are displayed"
  (interactive)
  (if (bound-and-true-p org-hide-emphasis-markers)
      (setq-local org-hide-emphasis-markers nil)
    (setq-local org-hide-emphasis-markers t))
  (font-lock-fontify-buffer)
  )

;; Example use:
;; * CS class 9:00am-5:00pm
;; <%%(my|org-class-days '(1 2) (org-block 2019 7 13 2019 7 19))>
(defun my|org-class-days (days rng)
  (and (memq (calendar-day-of-week date) days) rng)
  )


(provide 'init-my-org)
