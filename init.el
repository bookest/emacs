;;; init.el -- Emacs customizations
;;
;; Author: Chris Grim
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bootstrap cask
(eval-and-compile
  (require 'cask (expand-file-name "~/.cask/cask.el"))
  (cask-initialize (getenv "TRAVIS_BUILD_DIR")))

(eval-and-compile
  (require 'use-package))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

(use-package pallet
  :config
  (pallet-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general config
(column-number-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(which-function-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(unless (eq window-system 'ns)
  (menu-bar-mode -1))

(setq visible-bell t
      inhibit-startup-message t
      custom-file "~/.emacs.d/custom.el"
      custom-buffer-done-function 'kill-buffer)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.backups"))
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6
      make-backup-files t
      version-control t)

(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macosx specific config
(defun osxp ()
  "Return true if this Emacs is running on MacOS X."
  (string= system-type "darwin"))

(setq ns-command-modifier 'meta)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global keybindings
(bind-keys ("C-c o" . occur)
           ("C-c g" . goto-line)
           ("C-c #" . comment-or-uncomment-region)
           ("C-c cc" . compile)
           ("RET" . newline-and-indent)
           ("C-x C-m" . execute-extended-command)
           ("C-c l" . org-store-link)
           ("C-c a" . org-agenda)
           ("C-S-y" . clipboard-yank)
           ("C-S-w" . clipboard-kill-region)
           ("M-W" . clipboard-kill-ring-save)
           ("<f9>" . deft)
           ("C-x a r" . align-regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general functions
(defun cjg-create-scratch-buffer ()
  "Recreate a killed scratch buffer, complete with banner.

Stolen from emacswiki."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert initial-scratch-message)
  (lisp-interaction-mode))

(defun cjg-add-mode-directive nil
  "Insert a mode directive for the current major-mode at the end of
the first line of the buffer"
;;TODO: this should be a little smarter, like checking the first line
;;TODO: begins with a comment character and not a shebang
  (interactive)
  (let ((mode (symbol-name major-mode)))
    (when (string-match "-mode$" mode)
      (replace-match "" t t mode))
    (save-excursion
      (goto-char (point-min))
      (end-of-line)
      (insert " -*- Mode: " mode " -*-"))))

(defun cjg-electric-pair ()
  "Insert character pair without surrounding spaces.

Stolen from http://www.emacswiki.org/cgi-bin/wiki/PythonMode#toc6"
  (interactive)
  (let (parens-require-spaces)
    (expand-abbrev)
    (insert-pair)))

(defun cjg-insert-trailing-space (n)
  "Insert a trailing space after the character."
  (interactive "p")
  (self-insert-command n)
  (insert " "))

(defun cjg-move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR.

Stolen and modified from the original version found at
<http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html>."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                  (substring dir 0 -1)
                dir))
	 (newname (concat dir "/" name)))

    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

(defun cjg-rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME.

Stolen and modified from the original version found at
<http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html>."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (cond ((not filename)
           (message "Buffer '%s' is not visiting a file!" name))
          ((get-buffer new-name)
           (message "A buffer named '%s' already exists!" new-name))
          (t
           (rename-file name new-name 1)
           (rename-buffer new-name)
           (set-visited-file-name new-name)
           (set-buffer-modified-p nil)))))

(defmacro cjg-define-abbrevs (abbrev-table &rest abbrevs)
  "Define one or more abbreviations."
  (declare (indent defun))
  (let ((table abbrev-table))
    `(progn
       ,@(mapcar (lambda (elt)
                   `(define-abbrev ,table ,@elt))
                 abbrevs))))

(defmacro cjg-add-hook (hook &rest body)
  (declare (indent defun))
  (let ((fun (intern (concat "cjg-"
                             (symbol-name hook)))))
    `(progn
       (defun ,fun ()
         (progn ,@body))
       (add-hook ',hook ',fun))))

(defun cjg-term ()
  "Spawn a new terminal.

If running under a GUI the terminal will be created in a new
maximized frame."
  (interactive)
  (when window-system
    (let ((frame (make-frame)))
      (select-frame frame)
      (cjg-maximize-frame)))
  (cd (getenv "HOME"))
  (term "/bin/bash"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; window and frame manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cjg-swap-window-buffers ()
  "Swap the buffers displayed in two windows of the current frame.
There must be exactly two windows open."
  (interactive)
  (let ((windows (window-list)))
    (if (not (equal (length windows) 2))
	(error "Exactly two windows may be present when swapping.")
      (let* ((window1 (car windows))
	     (window1-buffer (window-buffer window1))
	     (window2 (cadr windows))
	     (window2-buffer (window-buffer window2)))
	  (set-window-buffer window1 window2-buffer)
	  (set-window-buffer window2 window1-buffer)
	  (select-window (next-window))))))

(defun cjg-set-frame-position (frame left top)
  "Set the frame position.

This is the same as the SET-FRAME-POSITION function, but if FRAME
is NIL it defaults to the currently selected frame."
  (when (null frame)
    (setq frame (selected-frame)))
  (set-frame-position frame left top))

(defun cjg-move-frame-upper-left (&optional frame)
  "Move FRAME to the upper left hand corner of the screen.
The current selected frame is moved if FRAME is NIL."
  (interactive)
  (cjg-set-frame-position frame 0 0))

(defun cjg-move-frame-upper-right (&optional frame)
    "Move FRAME to the upper right hand corner of the screen.
The current selected frame is moved if FRAME is NIL."
  (interactive)
  (cjg-set-frame-position frame -1 0))

(defun cjg-display-char-height ()
  "Return the height of the display in characters."
  (- (/ (display-pixel-height) (frame-char-height)) 3))

(defun cjg-display-char-width ()
  "Return the width of the display in characters."
  (/ (display-pixel-width) (frame-char-width)))

(defun cjg-maximize-frame ()
  "Maximize the current frame."
  (interactive)
  (cjg-move-frame-upper-left)
  (set-frame-size (selected-frame)
                  (cjg-display-char-width)
                  (cjg-display-char-height)))

(defun cjg-half-screen-frame ()
  "Make the current frame half the size of the display."
  (interactive)
  (set-frame-size (selected-frame)
                  (/ (cjg-display-char-width) 2)
                  (cjg-display-char-height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unicode prettiness
(defvar unicode-symbol-alist
  '((left-arrow . 8592)
    (up-arrow . 8593)
    (right-arrow . 8594)
    (down-arrow . 8595)
    (double-vertical-bar . #X2551)
    (equal . #X003d)
    (not-equal . #X2260)
    (identical . #X2261)
    (not-identical . #X2262)
    (less-than . #X003c)
    (greater-than . #X003e)
    (less-than-or-equal-to . #X2264)
    (greater-than-or-equal-to . #X2265)
    (logical-and . #X2227)
    (logical-or . #X2228)
    (logical-neg . #X00AC)
    (nil . #X2205)
    (horizontal-ellipsis . #X2026)
    (double-exclamation . #X203C)
    (prime . #X2032)
    (double-prime . #X2033)
    (for-all . #X2200)
    (there-exists . #X2203)
    (element-of . #X2208)
    (square-root . #X221A)
    (squared . #X00B2)
    (cubed . #X00B3)
    (lambda . #X03BB)
    (alpha . #X03B1)
    (beta . #X03B2)
    (gamma . #X03B3)
    (delta . #X03B4)))

(defun unicode-symbol (name)
  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
or GREATER-THAN into an actual Unicode character code. "
  (let ((char-code (cdr (assoc name unicode-symbol-alist))))
    (if (null char-code)
        (error "Unknown character name: '%s'" name)
      (decode-char 'ucs char-code))))


(defun substitute-pattern-with-unicode (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the
Unicode symbol SYMBOL."
  (interactive)
  (when window-system
    (font-lock-add-keywords
     nil
     `((,pattern (0 (progn
                      (compose-region (match-beginning 1)
                                      (match-end 1)
                                      ,(unicode-symbol symbol))
                      nil)))))))

(defun substitute-patterns-with-unicode (patterns)
  "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode (car x)
                                               (cdr x)))
          patterns))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; color-theme
(use-package color-theme
  :config
  (load-theme 'solarized t))

(defun bookest-toggle-solarized-mode ()
  "Toggle between solarized light and dark modes."
  (interactive)
  (let ((mode (if (equal (frame-parameter nil 'background-mode) 'light)
                  'dark
                'light)))
    (set-frame-parameter nil 'background-mode mode)
    (set-terminal-parameter nil 'background-mode mode))
  (enable-theme 'solarized))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode specific configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the island of lost modes
(use-package executable
  :config
  (setq executable-magicless-file-regexp
        (concat executable-magicless-file-regexp "\\|\\.pm$"))
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p))

(use-package generic-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-mode
(require 'compile)

(defmacro cjg-define-compile-command (name &rest body)
  "Define NAME as a function that sets COMPILE-COMMAND.

COMPILE-COMMAND is set to the results of evaluating
BODY. COMPILE-COMMAND will be the default if a file named
Makefile or makefile exist in the current directory."
  (declare (indent defun))
  `(defun ,name ()
     (unless (or (null buffer-file-name)
		 (file-exists-p "Makefile")
		 (file-exists-p "makefile"))
       (set (make-local-variable 'compile-command)
	    (progn ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-mode
(use-package font-lock
  :config
  (setq-default font-lock-maximum-decoration t
                font-lock-maximum-size nil)
  (global-font-lock-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-insert-mode
(use-package autoinsert
  :config
  (setq-default auto-insert t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prog-mode
(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  (add-hook 'prog-mode-hook #'(lambda () (setq show-trailing-whitespace t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cperl-mode
(use-package cperl-mode
  :commands (cjg-perl-insert-no-critic
             cjg-perl-toggle-test-plan
             cjg-perl-increment-test-plan
             cjg-perl-set-test-plan)
  :mode ("\\.t$" "\\.pl$" "\\.pm$")
  :init
  (defalias 'perl-mode 'cperl-mode)
  :config
  (setq cperl-electric-keywords nil
        cperl-electric-parens nil
        cperl-invalid-face nil
        cperl-under-as-char t
        cperl-indent-level 2
        cperl-indent-parens-as-block t
        cperl-close-paren-offset -2
        cperl-label-offset 0)

  (cjg-define-compile-command cjg:cperl-set-compile-command
    (concat "perl -cw "
            (file-name-nondirectory buffer-file-name)))

  (cjg-add-hook cperl-mode-hook
    (cjg:cperl-set-compile-command)
    (cjg-enable 'auto-insert-mode
                'abbrev-mode)
    (eldoc-mode)

    (set (make-local-variable 'eldoc-documentation-function)
         'cjg-cperl-eldoc-documentation-function)

    (cjg-define-abbrevs local-abbrev-table
      ("__p" "__PACKAGE__")
      ("__d" "__DATA__")
      ("__e" "__END__")
      ("dbg" "" 'perl-debug-skeleton)
      ("subm" "" 'perl-method-skeleton)
      ("hasm" "" 'moose-has-skeleton)))

  (define-skeleton perl-module-skeleton
    "Inserts a skeleton Perl module into the current buffer."
    "Package name: "
    "package " str ";\n"
    "use strict;\n"
    "use warnings;\n"
    "\n" _ "\n\n"
    "1;\n\n"
    "__END__\n")
  (define-auto-insert "\\.pm$" 'perl-module-skeleton)

  (define-skeleton perl-test-skeleton
    "Inserts a skeleton Perl test file into the current buffer."
    nil
    "use strict;\n"
    "use warnings;\n"
    "\n"
    "use Test::More qw(no_plan);\n"
    "\n" _ "\n"
    "\n"
    "__END__\n")
  (define-auto-insert "\\.t$" 'perl-test-skeleton)

  (define-skeleton perl-script-skeleton
    "Inserts a skeleton Perl script into the current buffer."
    nil
    "#!/usr/bin/perl\n"
    "use strict;\n"
    "use warnings;\n"
    "\n" _ "\n"
    "\n"
    "__END__\n")
  (define-auto-insert 'cperl-mode 'perl-script-skeleton t)

  (define-skeleton perl-debug-skeleton
    "Inserts a debug statement."
    nil
    "use Data::Dumper; warn Dumper(" _ ");")

  (define-skeleton perl-method-skeleton
    "Inserts a skeleton method."
    nil
    "sub " _ " {\n"
    > "my ($self) = @_;\n"
    "}\n")

  (define-skeleton moose-has-skeleton
    "Inserts a skeleton has statement"
    nil
    "has '" _ "' => (\n"
    > "is  => 'ro',\n"
    > "isa => 'Str',\n"
    ");\n")

  (defun cjg-cperl-eldoc-documentation-function ()
    "Return doc string for `eldoc-mode'."
    (let ((cperl-message-on-help-error nil))
      (car (cperl-get-help))))

  (defun cjg-perl-insert-no-critic ()
    "Insert the magic comment to disable perlcritic checks for the current line."
    (interactive)
    (save-excursion
      (let ((comment-start "##"))
        (indent-for-comment)
        (insert " no critic")
        (indent-according-to-mode))))

  (defun cjg-perl-toggle-test-plan ()
    "Toggle the Test::More plan in the current buffer.

This is a modified version of something I stole from perlmonks."
    (interactive)
    (let ((plan-pos))
      (save-excursion
        (save-match-data
          (goto-char (point-min))
          (cond ((re-search-forward "More[ \t]+tests[ \t]*=>[ \t]*" nil t)
                 (replace-match "More qw(no_plan); # tests => " t t))
                ((re-search-forward "More[ \t]+qw(no_plan);[ \t]*#[ \t]*" nil t)
                 (replace-match "More " t t)
                 (setq plan-pos (or (re-search-forward "[0-9]+" nil t)
                                    (point)))))))
      (when plan-pos (goto-char plan-pos))))

  (defun cjg-perl-find-test-plan ()
    (goto-char (point-min))
    (re-search-forward "More[ \t]+tests[ \t]*=>[ \t]*\\([0-9]+\\)[ \t]*;" nil t))

  (defun cjg-perl-increment-test-plan (arg)
    (interactive "p")
    (save-excursion
      (save-match-data
        (if (cjg-perl-find-test-plan)
            (let* ((plan (string-to-number (match-string 1))))
              (replace-match (number-to-string (+ plan arg)) nil nil nil 1))
          (message "no plan")))))

  (defun cjg-perl-set-test-plan (arg)
    (interactive "Nplan: ")
    (when (< arg 0)
      (error "plan must be greater than or equal to one."))
    (save-excursion
      (save-match-data
        (if (cjg-perl-find-test-plan)
            (replace-match (number-to-string arg) nil nil nil 1)
          (message "no plan")))))

  (bind-keys :map cperl-mode-map
             ("C-c nc" . cjg-perl-insert-no-critic)
             ("C-c tp" . cjg-perl-toggle-test-plan)
             ("C-c ip" . cjg-perl-increment-test-plan)
             ("C-c sp" . cjg-perl-set-test-plan)))

(defun perldoc (args)
  "Like man, but use perldoc instead."
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man args)))

(defun perl-eval-region (start end)
  "Evaluate Perl code in the current region."
  (interactive "r")
  (shell-command-on-region start end "perl " "*Perl Output*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tt-mode
(use-package tt-mode :mode "\\.tt$")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
(use-package emacs-lisp-mode
  :mode "Cask$"
  :config
  (cjg-add-hook emacs-lisp-mode-hook
    (eldoc-mode)
    (substitute-pattern-with-unicode "\\<(\\(lambda\\>\\)" 'lambda))

  (add-hook 'lisp-interaction-mode-hook 'cjg-emacs-lisp-mode-hook)
  (add-hook 'ielm-mode-hook 'cjg-emacs-lisp-mode-hook)

  (defun cjg-unintern-symbol-at-point ()
    "Unintern the symbol at point."
    (interactive)
    (let ((sym (symbol-at-point)))
      (unintern sym obarray)))

  (defmacro cjg-macroexpand (form)
    "Pretty print the macro expansion of `FORM'."
    `(pp (macroexpand-all ',form))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sh-mode
(use-package sh-script
  :config
  (setq sh-basic-offset 4)

  (cjg-define-compile-command cjg:sh-set-compile-command
    (concat "bash -n "
            (file-name-nondirectory buffer-file-name)))

  (cjg-add-hook sh-mode-hook
    (cjg:sh-set-compile-command)
    (cjg-enable 'abbrev-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-mode
(use-package cc-mode
  :mode ("\\.m$" . objc-mode)
  :config
  (use-package find-file)
  ;; setup ff-find-other-file to work with objc files
  (add-to-list 'cc-other-file-alist
               `("\\.h\\'" (,@(cadr (assoc "\\.h\\'" cc-other-file-alist)) ".m")))
  (add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))

  (bind-key "C-c C-o" ff-find-other-file cc-mode-map)

  (defun cjg-guess-c-header-mode ()
    "Guess the proper CC-mode for header files."
    (save-match-data
      (let ((name (buffer-file-name)))
        (when (and (string-match "\\.h$" name)
                   (not (eq major-mode 'objc-mode)))
          (when (replace-match ".m" t t name)
            (objc-mode))))))

  (cjg-add-hook c-mode-common-hook
    (setq c-basic-offset 4)
    (cjg-enable 'abbrev-mode)
    (cjg-guess-c-header-mode))

  (cjg-define-compile-command cjg:c-set-compile-command
    (let ((file (file-name-nondirectory buffer-file-name)))
      (concat "gcc -g -Wall -o "
              (file-name-sans-extension file)
              " "
              file)))

  (cjg-add-hook c-mode-hook
    (setq c-basic-offset 8)
    (cjg:c-set-compile-command))

  (cjg-define-compile-command cjg:c++-set-compile-command
    (let ((file (file-name-nondirectory buffer-file-name)))
      (concat "g++ -g -Wall -o "
              (file-name-sans-extension file)
              " "
              file)))

  (cjg-add-hook c++-mode-hook (cjg:c++-set-compile-command))

  (cjg-add-hook java-mode-hook
    ;; fix indentation for anonymous classes.
    (c-set-offset 'inexpr-class 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode
(use-package ruby-mode
  :mode ("\\.rb$"
         "\\.rake$"
         "\\.gemspec$"
         "Gemfile$"
         "Guardfile$"
         "Puppetfile$"
         "Rakefile$"
         "Vagrantfile$"
         "Berksfile$")
  :interpreter "ruby"
  :bind ("RET" . reindent-then-newline-and-indent)
  :init
  (defalias 'irb 'run-ruby)
  :config
  (cjg-add-hook ruby-mode-hook
    (cjg-enable 'ruby-electric-mode))

  (setq ruby-insert-encoding-magic-comment nil
        ruby-deep-indent-paren nil
        ruby-deep-arglist nil)

  (defun xmp ()
    (interactive)
    (let ((line (current-line))
          (col  (current-column)))
      (shell-command-on-region 1 (point-max) (xmp-command) t t)
      (goto-line line)
      (move-to-column col)))

  (defun xmp-command ()
    (cond ((save-excursion
             (goto-char 1)
             (search-forward "< Test::Unit::TestCase" nil t))
           "ruby -S xmpfilter.rb --unittest")
          ((save-excursion
             (goto-char 1)
             (re-search-forward "^context.+do$" nil t))
           "ruby -S xmpfilter.rb --spec")
          (t
           "ruby -S xmpfilter.rb"))))

(use-package inf-ruby :init (defalias 'irb 'rub-ruby))
(use-package ruby-electric
  :config
  ;; FIXME: ruby-electric uses the obsoleted last-command-char, which
  ;; has been removed as of 24.3.1. This restores the alias until I
  ;; can get an updated version of ruby-electric.
  (when (not (boundp 'last-command-char))
    (defvaralias 'last-command-char 'last-command-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-mode
(use-package python
  :config
  (add-hook 'python-mode-hook #'abbrev-mode)
  (add-hook 'python-mode-hook #'outline-minor-mode)
  (add-hook 'python-mode-hook #'eldoc-mode)
  (add-hook 'before-save-hook #'delete-trailing-whitespace t t)

  (defun pylint ()
    "Run pylint against the file visited by the current buffer.
Checks if unsaved buffers need to be saved."
    (interactive)
    (let ((command (concat "pylint --parseable=y \""
                           (buffer-file-name (current-buffer))
                           "\"")))
      (save-some-buffers (not compilation-ask-about-save) nil)
      (compilation-start command)))

  (defun cjg-python-electric-dot (n)
    (interactive "p")
    (let ((prev (char-before)))
      (when (not (or (null prev)
                     (python-syntax-comment-or-string-p)
                     (member (char-syntax prev) '(?w ?\" ?\)))))
        (insert "self")))
    (self-insert-command n))

  (font-lock-add-keywords 'python-mode
                          '(("\\<\\(self\\)\\>" 1 'italic)))

  (bind-key "." 'cjg-python-electric-dot python-mode-map)

  (define-skeleton python-def-skeleton
    "Insert a def statement."
    nil
    "def " _ "():")

  (define-skeleton python-def-method-skeleton
    "Insert a def statement for a method."
    nil
    "def " _ "(self):")

  (define-skeleton python-class-skeleton
    "Insert a class definition."
    nil
    "class " _ ":")

  (cjg-define-abbrevs python-mode-abbrev-table
    ("def" "" 'python-def-skeleton)
    ("defm" "" 'python-def-method-skeleton)
    ("class" "" 'python-class-skeleton)
    ("__i" "__init__")
    ("__m" "__main__")
    ("__v" "__version__")
    ("__s" "__str__")
    ("__n" "__name__")
    ("__m" "__main__")
    ("ifm" "if __name__ == '__main__':")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; haskell-mode
(use-package haskell-mode
  :config
  (bind-keys :map haskell-mode-map
             ("C-`"     . haskell-interactive-bring)
             ("C-c C-k" . haskell-process-clear)
             ("C-c C-z" . haskell-interactive-switch)
             ("C-c C-l" . haskell-process-load-or-reload)
             ("C-c C-t" . haskell-process-do-type)
             ("C-c C-i" . haskell-process-do-info)
             ("SPC"     . haskell-mode-contextual-space))

  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'haskell-doc-mode)
  (add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template))

(use-package haskell-cabal
  :config
  (bind-keys :map haskell-cabal-mode-map
             ("C-`" . haskell-interactive-bring)
             ("C-c C-z" . haskell-interactive-switch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; puppet-mode
(use-package puppet-mode
  :config
  (setq puppet-imenu-generic-expression
        '((nil "^\\s-*\\(?:class\\|define\\|node\\)\\s-+\\(\\(?:\\sw\\|\\s_\\)+\\(?:::\\(?:\\sw\\|\\s_\\)+\\)*\\)" 1)))

  (cjg-add-hook puppet-mode-hook
    (set (make-local-variable 'imenu-generic-expression)
          puppet-imenu-generic-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outline-mode
(use-package outline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-mode
(use-package ansi-color)
(use-package shell
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell
(use-package eshell
  :config
  (setq eshell-prompt-function 'cjg-eshell-mildly-fancy-prompt
        eshell-prompt-regexp "^[^#>\n]* [#>] "
        eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-rm-interactive-query t
        eshell-mv-overwrite-files nil
        eshell-cmpl-cycle-completions nil
        eshell-scroll-show-maximum-output t
        eshell-scroll-to-bottom-on-output nil)

  (defun cjg-eshell-simple-prompt ()
    "A simple [user@host] pwd > prompt for eshell."
    (concat "[" user-login-name "@"
            (car (split-string system-name "\\.")) "] "
            (eshell/pwd)
            (if (= (user-uid) 0)
                " # "
              " > " )))

  (defun cjg-eshell-mildly-fancy-prompt ()
    "A prompt that is a little fancier than the simple prompt.
Adds the display of the current time in 24 hour format."
    (concat (format-time-string "{%T}") (cjg-eshell-simple-prompt)))

  (cjg-add-hook eshell-mode-hook
    (add-to-list 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom))

  ;; handle ASCII control codes
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)

  (defun eshell/clear nil
    "Emulate the shell command clear in lisp"
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)))

  (defun eshell/perldoc (&rest args)
    "Browse Perl documentation in Pod format. Similar to
eshell/man. Taken from EmacsWiki."
    (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))

  (defun eshell/e (&rest args)
    "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (find-file file)
            (goto-line line))
        (find-file (pop args)))))

  ;; this overrides the standard eshell/basename, so we have to be
  ;; careful about when it is loaded
  (use-package em-unix
    :config
    (defun eshell/basename (filename &optional ext)
      "Return FILENAME sans the directory, if EXT is provided remove
the extension EXT from the end of the filename.

Overrides the default eshell/basename with an implementation that
is closer to GNU basename."
      (save-match-data
        (let ((file (file-name-nondirectory filename))
              regex)
          (if (and ext
                   (setq regex (concat (regexp-quote ext) "$"))
                   (string-match regex file))
              (substring file 0 (match-beginning 0))
            file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3m-mode
(use-package w3m
  :config
  (use-package w3m-search
    :config
    (add-to-list 'w3m-search-engine-alist
                 '("search-cpan"
                   "http://search.cpan.org/search?query=%s&mode=all"))
    (add-to-list 'w3m-search-engine-alist
                 '("google-groups-clpm"
                   "http://groups.google.com/groups?hl=en&lr=&ie=ISO-8859-1&q=foo&btnG=Google+Search&meta=group%3Dcomp.lang.perl.misc")))
  (add-to-list 'w3m-uri-replace-alist
               '("\\`cpan:" w3m-search-uri-replace "search-cpan"))

  (cjg-add-hook w3m-mode-hook
    (w3m-toggle-inline-images t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; table-mode
(use-package table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar-mode
(use-package calendar
  :config
  (add-hook 'calendar-initial-window-hook 'calendar-mark-holidays)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (setq diary-file "~/.diary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diary-mode
(use-package diary-lib
  :config
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'diary-hook 'appt-make-list)
  (setq diary-file "~/.diary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  ;; shut off backups for remote files
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remember
(use-package remember
  :config
  (setq remember-annotation-functions '(org-remember-annotation)
        remember-handler-functions '(org-remember-handler))
  (cjg-add-hook remember-mode-hook
    (cjg-enable 'flyspell-mode)
    (org-remember-apply-template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb
(use-package bbdb
  :config
  (bbdb-initialize 'gnus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cfengine
(use-package cfengine
  :config
  (setq cfengine-indent 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erc
(use-package erc
  :config
  (use-package erc-spelling)
  (setq erc-auto-query 'window-noselect))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; message-mode
(use-package message
  :config
  (cjg-add-hook message-mode-hook
    (cjg-enable 'flyspell-mode
                'footnote-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-mode
(use-package text-mode
  :disabled t
  :config
  (cjg-add-hook text-mode-hook
    (cjg-enable 'flyspell-mode
                'footnote-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css-mode-simple
(use-package css-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse-avoidance-mode
(when (display-mouse-p)
  (when (not (osxp))
    (defun mouse-avoidance-banish-destination ()
      (let ((edges (window-edges)))
        (cons (nth 2 edges)
              (nth 3 edges)))))
  (mouse-avoidance-mode 'banish))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ido
(use-package ido
  :config
  (setq ido-slow-ftp-host-regexps '(".*")
        ido-enable-flex-matching t)
  (ido-mode 1)
  (ido-everywhere 1))

(use-package projectile
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (projectile-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
(use-package org
  :defer t
  :config
  (setq org-log-done t
        org-mode-hide-leading-stars t
        org-mode-odd-levels-only t
        org-return-follows-link t
        org-directory "~/Documents/org/"
        org-default-notes-file (concat org-directory "notes.org"))
  (setq org-link-abbrev-alist
        '(("org" . "file:~/Documents/org/")
          ("rt" . "https://rt3.be-md.ncbi.nlm.nih.gov/rt3/Ticket/Display.html?id=%s")
          ("cpan" . "http://search.cpan.org/perldoc?%s")
          ("jira" . "http://jira.be-md.ncbi.nlm.nih.gov/browse/%s"))))

(use-package deft
  :defer t
  :config
  (setq  deft-extensions '("org" "md" "txt")
         deft-use-filename-as-title t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fit-frame
(use-package fit-frame
  :config
  ;; disable fit-frame
  (setq fit-frame-inhibit-fitting-flag t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (require 'ibuf-ext nil t)
  (when (featurep 'ibuf-ext)
    (setq ibuffer-saved-filter-groups '(("default"
                                         ("org" (mode . org-mode))
                                         ("gnus" (or
                                                  (mode . message-mode)
                                                  (mode . mail-mode)
                                                  (mode . gnus-group-mode)
                                                  (mode . gnus-summary-mode)
                                                  (mode . gnus-article-mode))))))
    (cjg-add-hook ibuffer-mode-hook
      (ibuffer-switch-to-saved-filter-groups "default"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yaml
(use-package yaml-mode
  :mode "\\.ya?ml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git-gutter
(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode t))


(server-start)

;;; init.el ends here
