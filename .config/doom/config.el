;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "spcbfr"
      user-mail-address "youssefbouzekri@protonmail.com")

;; load the default theme
;; it can also be changed for the current session using SPC h t
(setq doom-theme 'doom-one)

;; setting fonts for different use cases
;; and using the italics variant for comments and such
(setq doom-font (font-spec :family "IBM plex mono" :size 13)
      doom-variable-pitch-font (font-spec :family "Jetbrains Mono" :size 15)
      doom-big-font (font-spec :family "Source Code Pro" :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))


(setq org-directory "~/docs/org/")

;; set to `relative' if you want relative line numbers
;; I prefer to disable them all together
(setq display-line-numbers-type nil)

;; set the window name to the buffer name only
;; and to the project folder when applicable
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))


;; make dashboards keybindings easier to type
(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "p" #'doom/open-private-config
      :ne "q" #'save-buffers-kill-terminal)

;; make raibow mode a global minor mode so it runs in every buffer
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(global-rainbow-mode 1 )

;; make which-key show up faster
(setq which-key-idle-delay 0.5) ;; I need the help, I really do

;; having the `evil-' prefix on every command is a bit too verbose
;; let's make it simpler
(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

;; start pdf-tools in midnight-mode (dark-mode)
(add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)
