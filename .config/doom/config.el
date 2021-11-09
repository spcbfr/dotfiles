(setq user-full-name "Youssef Bouzekri"
      user-mail-address "youssefbouzekri@protonmail.com")

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "q" #'save-buffers-kill-terminal)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

(setq org-clock-sound "~/docs/ding.wav")

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

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(setq doom-theme 'doom-one)

(setq doom-font (font-spec :family "monospace" :size 13)
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      doom-variable-pitch-font (font-spec :family "Overpass" :size 13)
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))

(setq display-line-numbers-type nil)

(setq org-directory "~/docs/org/")

(setq org-roam-directory "~/docs/roam")

(setq org-roam-dailies-directory "~/docs/roam/daily")

(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
