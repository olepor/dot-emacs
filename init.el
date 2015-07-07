
;; Meta

;;    Emacs can only load =.el=-files. We can use =C-c C-v t= to run
;;    =org-babel-tangle=, which extracts the code blocks from the current =.org= file
;;    into a =.el= file.

;;    In order to avoid doing this each time over, every time a change is made, we can add a
;;    function to the =after-save-hook= ensuring that we always tangle and byte-compile 
;;    the =.org= document.

;;    (Courtesy of Lars Tveito)

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
    tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

;; Packages
   
;;    Managing extensions for Emacs is simplified using =package= which is built in to
;;    Emacs 24 and newer. To load downloaded packages we need to initialise =package=.
;;    =cl= is a libarary that contains many functions from Common Lisp, and comes in handy
;;    quite often, so we want to make sure it's loaded, along with =package=, which is obviously 
;;    needed.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("MELPA" . "http://melpa.milkbox.net/packages/")))

;; #+RESULTS:

(require 'cl)
 (require 'package)
 (setq package-enable-at-startup)
 (package-initialize)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0) ;; Byte
 ;; compiles everything in my emacs.d directory, slow, but needed to get rid
 ;; of the swift-mode-error

;; Define whether or not the newest version of a package is installed

(let* ((packages '(auto-complete
                   auto-compile ;; Automatically compiles an elisp file
                   ido-vertical-mode
                   monokai-theme ;; A fruity dark theme, originally from sublime ( I think )...
                   leuven-theme ;; A nice light theme for daytime coding
                   anchored-transpose ;; Fancy text editing
                   expand-region ;; Expands a region based on ( " ...
                   undo-tree ;; visually represents your undo's
                   autopair ;; Highlights matching parantheses
                   auto-package-update ;; Automatically update packages at a certain frequency. e.g once a week
                   centered-window-mode ;; Centers the text if only one
                   ;; window is showing
                   jedi ;; Python auto-completion for emacs
                   magit ;; Control git from emacs
                   org ;;Outline based notes and management organizer
                   swift-mode ;; Major mode for Apple's swift
                   ;; programming language
                   ;;  flycheck ;; On the fly spell checking for emacs ;; 
                   ))
       (packages (remove-if 'package-installed-p packages)))
  (when packages
    (package-refresh-contents)
    (mapc 'package-install packages)))

;; Add theme list from elpa directory

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Enable ido, which changes the way files are selected in the minibuffer, everywhere
;;    and at last show it vertically

(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)

;; Enable the expand region package

(global-set-key (kbd "C-=") 'er/expand-region)

;; Set the undo-tree-mode globally

(global-undo-tree-mode 1)

;; emacs-lisp
   
;;    Set the autopair parenthesis on

(require 'autopair)
(defvar autopair-modes '(r-mode java-mode))
(defun turn-on-autopair-mode ()
  (autopair-mode 1))
(dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

;; Set the auto-update-packages interval to 7 days. Add =(auto-package-update-maybe)= for å skru på

(require 'auto-package-update)
(setq auto-package-update-interval 7)

;; #+RESULTS:

(require 'centered-window-mode)
(centered-window-mode t)
(visual-line-mode t)

;; Simple Emacs setup

;;    Loops a list of everything we wish to enable (e.g set to 1)

(dolist (mode '(show-paren-mode ;; Shows matching parenthesis
                show-column-number ;; Show column number in mode line / Genius
                delete-selection-mode ;; Deletes marked text
                global-undo-tree-mode ;; Sets the undo tree mode to global
                auto-compile-on-load-mode ;; Auto compiles elisp on load
                )) 
  (when (fboundp mode)
    (funcall mode 1)))

(dolist (mode '(blink-cursor-mode ;; Disables the blinking cursor
                menu-bar-mode ;; Removes the toolbar
                tool-bar-mode ;; Turns the toolbar off
                scroll-bar-mode ;; NO SCROLLBARS
                ))
    (funcall mode 0))

;; Answer with y/n instead of yes or no

(fset 'yes-or-no-p 'y-or-n-p)

;; Make a keybind to kill the current buffer, and bind it to =C-x C-k=

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; Comment or uncomment a region

(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; Add all autosaves in one directory, in order to keep everything clean

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a directory
called autosaves located wherever your .emacs.d/ is located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms `((".*" ,emacs-autosave-directory t)))

;; Set the limitations for line length and tabs vs spaces etc

(setq-default fill-column 76                    ; Maximum line width.
              indent-tabs-mode nil              ; Use spaces instead of tabs.
              split-width-threshold 100         ; Split verticly by default.
              auto-fill-function 'do-auto-fill) ; Auto-fill-mode everywhere.

;; Os Specifics
;;    Use the Command key as our Meta

(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t)
  (exec-path-from-shell-initialize))

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

;; Defaults
   
;;    Set the default encoding to UTF-8

(set-language-environment "UTF-8")

;; Use setq to set default startup variables to whatever we like

(setq inhibit-startup-message t
      initial-scratch-message nil
      )

;; Ido
;;    The ido specifics

(dolist (mode
         '(ido-mode                   ; Interactivly do.
           ido-everywhere             ; Use Ido for all buffer/file reading.
           ido-vertical-mode          ; ido vertical
           ))
  (funcall mode 1))

;; Sort the files shown in ido in prioritized order

(setq ido-file-extension-order
      '(".java" ".c" ".h" ".el" ".org"))

;; Auto Complete

;;     Enable the auto-complete that we downloaded with the package manager

(require 'auto-complete-config)
(ac-config-default)

;; General code hooks
     
;;    For folding code we use the commands

(defun hideshow-on ()
  (local-set-key (kbd "C-c <right>") 'hs-show-block)
  (local-set-key (kbd "C-c <left>") 'hs-hide-block)
  (local-set-key (kbd "C-c <up>") 'hs-hide-level)
  (local-set-key (kbd "C-c <down>") 'hs-show-all)
  (hs-minor-mode t))

;; And then add the hook to all C-like languages

(add-hook 'c-mode-common-hook 'hideshow-on)

;; The tidy function is an absolute gem. It Indents everything properly, and removes wasted
;;    whitespace. Couldn't live without it

(defun tidy ()
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (whitespace-cleanup)
    (indent-region beg end nil)
    (untabify beg end)))

;; Now bind the tidy function to =<C-tab>=

(global-set-key (kbd "<C-tab>") 'tidy)

;; Enable multiple cursors

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Does not show the compilation buffer, unless there is an error

(require 'cl)


(defun brian-compile-finish (buffer outstr)
  (unless (string-match "finished" outstr)
    (switch-to-buffer-other-window buffer))
  t)

(setq compilation-finish-functions 'brian-compile-finish)


(defadvice compilation-start
    (around inhibit-display
            (command &optional mode name-function highlight-regexp)) 
  (if (not (string-match "^\\(find\\|grep\\)" command))
      (flet ((display-buffer)
             (set-window-point)
             (goto-char)) 
        (fset 'display-buffer 'ignore)
        (fset 'goto-char 'ignore)
        (fset 'set-window-point 'ignore)
        (save-window-excursion 
          ad-do-it))
    ad-do-it))

(ad-activate 'compilation-start)

;; Add a function to bind the revert-buffer function

(global-set-key (kbd "C-x C->") 'revert-buffer)

;; Compilation
;;    Add a compile hook for all c-like languages

(defun c-setup()
  (local-set-key (kbd "C-c C-c") 'compile))

;; And now add the hook to all c-modes

(add-hook 'c-mode-common-hook 'c-setup)

;; Java-mode specifics

;;    Defines the shortcuts used in java

(defun java-shortcuts ()
  (define-abbrev-table 'java-mode-abbrev-table
    '(("psvm" "public static void main(String[] args) {" nil 0)
      ("sin" "Scanner myScanner = new Scanner(" nil 0)
      ("sop" "System.out.printf" nil 0)
      ("sopl" "System.out.println" nil 0)))
  (abbrev-mode t))

;; Now we add the hook to be used in java only

(add-hook 'java-mode-hook 'java-shortcuts)

;; Defines a function that compiles java files, and binds it to =C-c C-c=

(defun java-setup ()
  (set (make-variable-buffer-local 'compile-command)
       (concat "javac " (buffer-name)))
  (local-set-key (kbd "C-c C-c") 'compile))

;; Then we add the java hook

(add-hook 'java-mode-hook 'java-setup)

;; My own Java-hooks
    
;;     Runs the current java buffer in the emacs terminal

(defun run-java-buffer ()
  (interactive)
  (eshell-command (concat "java " (substring (buffer-name) 0 -5)))
  (local-set-key (kbd "<f6>") 'run-java-buffer))

;; A function that binds replace-string to a keybind =undecided=

(defun java-string-replace ()
  (local-set-key (kbd "C-r") 'java-string-replace))

(add-hook 'java-mode-hook 'java-string-replace)

;; C
;;    As we have already made sure that the yasnippet and auto-complete
;;    packages are loaded,

(add-hook 'c-mode--hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Python
   
;;    Set the version to use, currently 3.4

(setq python-shell-interpreter "/usr/local/bin/python3.4")

;; Setup jedi

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;; Org Mode

;;    Setup the org mode so that the source code is themed as they would in
;;    their native mode

(setq org-src-fontify-natively t
   org-confirm-babel-evaluate nil)

;; Add support for java in org-mode

(org-babel-do-load-languages
    'org-babel-load-languages '((python . t) (java . t)))

;; Setup agenda and org-links, copy with =C-c l= and
;;    paste with =C-c C-l=
;;    Also note the handy =C-u C-c C-l=

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Magit
;;    magit bind magit-status to =C-x g=

(global-set-key (kbd "C-x g") 'magit-status)

;; Swift

;;    Add flycheck to the swift mode

(require 'flycheck)
(add-to-list 'flycheck-checkers 'swift)

;; Add an abbrev table for auto-complete (?)

;;    You can run the command `swift-mode-run-repl' with =C-c C-z=

(add-hook 'swift-mode-hook 'autopair-on)

;; Add a the swift language to the auto-complete-list (Auto-Complete
;;    package)

(add-to-list 'ac-modes 'swift-mode)

;; XCode Integration

;;    Add a method from
;;    http://bretthutley.com/programming/emacs/integrating-emacs-and-xcode/
;;    to integrate compilation with xCode build

(defun bh-compile ()
  (interactive)
  (let ((df (directory-files "."))
        (has-proj-file nil)
        )
    (while (and df (not has-proj-file))
      (let ((fn (car df)))
        (if (> (length fn) 10)
            (if (string-equal (substring fn -10) ".xcodeproj")
                (setq has-proj-file t)
              )
          )
        )
      (setq df (cdr df))
      )
    (if has-proj-file
        (compile "xcodebuild -configuration Debug")
      (compile "make")
      )
    )
  )

;; Now add it to the swift mode, and keybind it to our usual compile =C-c
;;    C-c=

(defun swift-xcode-compile()
  (local-set-key (kbd "C-c C-c") 'bh-compile))

(add-hook 'swift-mode-hook 'swift-xcode-compile)

;; Themes
;;      Now choose a colour theme - Monokai for now

(load-theme 'monokai t)

;; Disables themes that are selected using =M-x load-theme=, so that there
;;      is litter left hanging from the old theme

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate) 
  (mapc 'disable-theme custom-enabled-themes))

;; Fonts  

;;    Changes the default font

(if (member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil :font "Source Code Pro-13")
(set-frame-parameter nil 'font "DejaVu Sans Mono-12"))
