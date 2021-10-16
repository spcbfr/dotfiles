(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "Cascadia Code" :size 13)
      doom-big-font (font-spec :family "Operator Mono" :size 19))

(setq display-line-numbers-type nil)

(setq org-directory "~/docs/org/")

(setq org-roam-directory "~/docs/roam")

(setq org-roam-dailies-directory "~/docs/roam/daily")

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(xterm-mouse-mode 1)

(setq org-clock-sound "~/music/ding.wav")

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(map! :leader :desc "Insert node immediatly" :n "n r I" #'org-roam-node-insert-immediate)

(after! elfeed
  (setq elfeed-search-filter "@1-week-ago"))

(add-hook! 'elfeed-search-mode-hook 'elfeed-update)

(map! :leader :desc "Open elfeed" :n "r" #'elfeed)
