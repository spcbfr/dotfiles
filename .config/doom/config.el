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
      doom-unicode-font (font-spec :family "Fira Code" :size 12) ;; Fallback unicode font for icons, glyphs, etc...
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 15) ;; the font to use for variable-pitch text
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

(setq display-line-numbers-type nil)

(add-hook 'pdf-view-mode-hook (lambda ()
                                (pdf-view-midnight-minor-mode))) ; automatically turns on midnight-mode for pdfs

(add-hook! '(text-mode-hook prog-mode-hook conf-mode-hook) #'rainbow-mode)

(remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

(defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
  :override #'emacs-everywhere-set-frame-position
  (cl-destructuring-bind (x y width height)
      (emacs-everywhere-window-geometry window-info)
    (set-frame-position frame
                        (+ x (/ width 2) (- (/ width 2)))
                        (+ y (/ height 2)))))

(setq emacs-everywhere-frame-name-format "emacs-everywhere")

(setq org-directory "~/org")

(setq org-roam-dailies-directory "daily")
(setq org-roam-directory "~/org/roam")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

(setq which-key-idle-delay 0.5) ;; I need the help, I really do

(setq which-key-allow-multiple-replacements t)
(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))
