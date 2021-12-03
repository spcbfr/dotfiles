;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "spcbfr"
      user-mail-address "spcbfr@protonmail.com")

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

(setq doom-font (font-spec :family "IBM Plex Mono" :size 13) ;; the default general doom font
      doom-unicode-font (font-spec :family "Fira Code" :size 13) ;; Fallback unicode font for icons, glyphs, etc...
      doom-variable-pitch-font (font-spec :family "IBM Plex Mono" :size 15) ;; the font to use for variable-pitch text
      doom-big-font (font-spec :family "Jetbrains Mono" :size 24)) ;; font used in big-font-mode

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "v" #'vterm/here
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "q" #'save-buffers-kill-terminal)

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
