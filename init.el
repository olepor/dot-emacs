(load-theme 'monokai)
(blink-cursor-mode -1) ;;Disable the blinking cursor
(electric-indent-mode 1) ;; Not a hundred % sure what it does yet
(electric-pair-mode 1) ;; automatically match braces and parenthesis
(tool-bar-mode -1) ;; turn off the tool-bar

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

