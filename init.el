;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(package-initialize)
(evil-mode 1)
;; enable evil-mode(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

;; Keys
;; C-u C-<spc> ;; jump back in mark ring
(blink-cursor-mode -1) ;;Disable the blinking cursor
(electric-indent-mode 1) ;; Not a hundred % sure what it does yet
(electric-pair-mode 1) ;; automatically match braces and parenthesis
(tool-bar-mode -1) ;; turn off the tool-bar

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

;; Devilry-mode
(add-to-list 'load-path "~/.emacs.d/plugins/devilry-mode/")
(require 'devilry-mode)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("05c3bc4eb1219953a4f182e10de1f7466d28987f48d647c01f1f0037ff35ab9a" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Disable the scroll bars
(setq scroll-bar-mode nil) ;; Not sure if right way

;; define and bind the back to indentation to be back-to-indentation() and move-beggining of line every other time

;; Your theme
(custom-set-variables
 '(custom-enabled-themes (quote (tango-dark))))

;; Change comment color to violet
(set-face-foreground 'font-lock-comment-face "violet")

;;; If using evil and wanting regular emacs bindings on insert
;; (setcdr evil-insert-state-map nil)
;; (define-key evil-insert-state-map
;;             (read-kbd-macro evil-toggle-key) 'evil-normal-state)
;; (define-key evil-insert-state-map [escape] 'evil-normal-state)
;(cond ((member "Einstein Grand" (font-family-list))
;       (set-face-attribute 'default nil :font "Einstein Grand-1"))
;      ((member "Inconsolata" (font-family-list))
					;       (set-face-attribute 'default nil :font "Inconsolata-14")))

(cond ((member "Source Code Pro" (font-family-list))
       (set-face-attribute 'default nil :font "Source Code pro-13"))
      ((member "Inconsolata" (font-family-list))
       (set-face-attribute 'default nil :font "Inconsolata-14")))

;; Org-mode
;; The following lines are always needed.  Choose your own keys.
 (global-set-key "\C-cl" 'org-store-link)
 (global-set-key "\C-ca" 'org-agenda)
 (global-set-key "\C-cc" 'org-capture)
 (global-set-key "\C-cb" 'org-iswitchb)	
