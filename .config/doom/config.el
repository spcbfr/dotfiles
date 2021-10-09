;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "youssef bouzekri"
      user-mail-address "youssefbouzekri@protonmail.com")

(setq doom-theme 'doom-one)

;; line numbers are slow and they aren't really needed
(setq display-line-numbers-type nil)

(setq org-directory "~/docs/org")
(setq org-roam-directory "~/docs/roam/")
(setq org-id-locations-file "~/docs/roam/.orgids")

;; open pdf documents in dark mode by default
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

;; preview latex fragments automatically
(add-hook 'org-mode-hook 'org-fragtog-mode)

;; org-roam-ui
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; inserts an org-roam link immediately,
;; without invoking the capture buffer
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))
(map! :leader :desc "Insert link immediately"  :n "nrI" #'org-roam-node-insert-immediate)

;; adds a timestamp to every org-roam-dailies entry
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

;; play a ding sound when an org-timer reaches zero
(setq org-clock-sound "~/music/ding.wav")
