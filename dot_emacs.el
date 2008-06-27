;;; dot_emacs.el -- Emacs customizations 
;;
;; Author: Chris Grim
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load-path config
(defun cjg-add-to-load-path (paths)
  "Add the list of directories to load-path if they exist."
  (dolist (dir paths)
    (let ((expanded-dir (expand-file-name dir))) 
      (when (file-directory-p expanded-dir)
	(message "Adding %S to load path..." expanded-dir)
	(add-to-list 'load-path expanded-dir)
        (save-current-buffer
          ;;FIXME: unfortunately, this appends directories to loadpath
          ;; so it doesn't DWIM if I want to use an alterante version
          ;; of something distributed with emacs (Gnus for example)
          (cd dir)
          (normal-top-level-add-subdirs-to-load-path))))))

(defun cjg-load-file-if-exists (file)
  (when (file-exists-p file)
    (message "loading %s..." file)
    (load file)))

(defvar *cjg-lisp-dirs*
  '("~/lib/emacs" "~/lib/emacs/site-lisp")
  "Directories to check for locally installed lisp")

(cjg-add-to-load-path *cjg-lisp-dirs*)

(defvar *cjg-local-config-file-name*
  (concat (getenv "HOME")
	  "/lisp/site-lisp/site-"
	  (car (split-string system-name "\\."))
	  ".el"))

(cjg-load-file-if-exists "/usr/share/emacs/site-lisp/site-gentoo.el")
(cjg-load-file-if-exists *cjg-local-config-file-name*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general predicates

(defmacro cjg-define-domain-predicate (name re)
  "Defines a predicate that returns true if `system-name' matches RE.

The predicate will be called cjg-at-LCNAME-p. Where LCNAME is
NAME converted to lowercase."
  `(defun ,(intern (concat "cjg-at-" (downcase name) "-p")) ()
     ,(concat "Returns true if running Emacs at " name ".")
     (save-match-data
       (if (string-match ,re (system-name))
           t
         nil))))

(cjg-define-domain-predicate "NCBI" "\\.ncbi\\.nlm\\.nih\\.gov$")
(cjg-define-domain-predicate "NYU" "\\.nyu\\.edu")

(defalias 'at-work-p 'cjg-at-ncbi-p)

(defun cjg-at-home-p ()
  "Returns true if running Emacs at home."
  (not (at-work-p)))

(defun osxp ()
  "Return true if this Emacs is running on MacOS X."
  (string= system-type "darwin"))

(defun carbonp ()
  "Returns true if running under Carbon."
  (string= window-system "mac"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general config

(defun cjg-toggle (arg lst)
  (dolist (fn lst)
    (when (fboundp fn)
      (funcall fn arg))))

(defun cjg-disable (&rest lst)
  (cjg-toggle -1 lst))

(defun cjg-enable (&rest lst)
  (cjg-toggle 1 lst))

(cjg-enable 'column-number-mode
            'line-number-mode
            'show-paren-mode
            'ido-mode
            'ido-everywhere
            'which-function-mode
            'partial-completion-mode)

(cjg-disable 'scroll-bar-mode
             'tool-bar-mode
             'menu-bar-mode)

(display-time)

(setq visible-bell t			
      inhibit-startup-message t
      custom-file "~/lib/emacs/custom.el"
      custom-buffer-done-function 'kill-buffer)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.backups"))
      delete-old-versions t
      kept-old-versions 2
      kept-new-versions 6
      make-backup-files t
      version-control t)

(setq-default indent-tabs-mode nil)

(when (boundp 'safe-local-variable-values)
  (add-to-list 'safe-local-variable-values '(auto-recompile . t)))

(defvar *cjg-work-email-address* "grimc@ncbi.nlm.nih.gov")

(when (at-work-p)
  (setq user-mail-address *cjg-work-email-address*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macosx specific config
(when (osxp)
  (setq mac-command-modifier 'meta)

  (when (carbonp)			; set color scheme for Carbon
    (dolist (elm '((foreground-color . "white")
		   (background-color . "black")
		   (cursor-color . "coral")))
      (add-to-list 'default-frame-alist elm)))
  
  (when (featurep 'aquamacs)
    (setq default-major-mode 'fundamental-mode
          initial-major-mode 'lisp-interaction-mode
          obof-other-frame-regexps nil
          special-display-regexps nil)
    
    (add-to-list 'obof-same-frame-regexps "\\*Help\\*")
    (add-to-list 'obof-same-frame-switching-regexps "\\*Help\\*")

    (cjg-disable 'cua-mode
                 'osx-key-mode
                 'one-buffer-one-frame-mode
                 'aquamacs-styles-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global keybindings

(defmacro cjg-define-global-keys (&rest bindings)
  "Define one or more keybindings in the global map."
  `(cjg-define-keys (current-global-map) ,@bindings))
(put 'cjg-define-global-keys 'lisp-indent-function 'defun)

(defmacro cjg-define-keys (map &rest bindings)
  "Define one or more key bindings in MAP."
  ;;FIXME: keymap should be an uninterned tempvar to avoid name conflict.
  (let ((keymap map))
    `(progn
       ,@(mapcar (lambda (elt)
                   `(define-key ,keymap (kbd ,(car elt)) ,(cdr elt)))
                 bindings))))
(put 'cjg-define-keys 'lisp-indent-function 'defun)

(cjg-define-global-keys
  ("%" . 'cjg-match-paren)
  ("C-c o" .  'occur)
  ("C-c g" . 'goto-line)
  ("C-c #" . 'comment-or-uncomment-region)
  ("C-c cc" . 'compile)
  ("C-x C-b" . 'cjg-buffer-list)
  ("RET" . 'newline-and-indent)
  ("C-x C-m" . 'execute-extended-command)
  ("C-c l" . 'org-store-link)
  ("C-c a" . 'org-agenda)
  ("C-S-y" . 'clipboard-yank)
  ("C-S-w" . 'clipboard-kill-region)
  ("M-W"   . 'clipboard-kill-ring-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general functions
(defun cjg-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %.
Very blasphemous."
  (interactive "p")
  (let ((current-syntax (char-syntax (char-after (point)))))
    (cond ((equal current-syntax ?\()
           (forward-list 1)
           (backward-char 1))
          ((equal current-syntax ?\))
           (forward-char 1)
           (backward-list 1))
          (t (self-insert-command (or arg 1))))))
  
(defun cjg-buffer-list (&optional files-only)
  (interactive "P")
  (let ((b (list-buffers-noselect files-only)))
    (display-buffer b)
    (pop-to-buffer b)))

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
  (let ((table abbrev-table))
    `(progn
       ,@(mapcar (lambda (elt)
                   `(define-abbrev ,table ,@elt))
                 abbrevs))))
(put 'cjg-define-abbrevs 'lisp-indent-function 1)

(defmacro cjg-add-hook (hook &rest body)
  (let ((fun (intern (concat "cjg-"
                             (symbol-name hook)))))
    `(progn
       (defun ,fun ()
         (progn ,@body))
       (add-hook ',hook ',fun))))
(put 'cjg-add-hook 'lisp-indent-function 1)

(defmacro cjg-eval-after-load (file &rest body)
  "Evaluates `BODY' after `FILE' has been loaded.
See `eval-after-load'."
  `(eval-after-load ,file
     '(progn ,@body)))
(put 'cjg-eval-after-load 'lisp-indent-function 1)

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
;;; mode specific configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the island of lost modes
(require 'executable)
(setq executable-magicless-file-regexp 
      (concat executable-magicless-file-regexp "\\|\\.pm$"))

(add-hook 'after-save-hook 
	  'executable-make-buffer-file-executable-if-script-p)

(require 'generic-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compile-mode
(require 'compile)

(defmacro cjg-define-compile-command (name &rest body)
  "Define NAME as a function that sets COMPILE-COMMAND.

COMPILE-COMMAND is set to the results of evaluating
BODY. COMPILE-COMMAND will be the default if a file named
Makefile or makefile exist in the current directory."
  `(defun ,name ()
     (unless (or (null buffer-file-name)
		 (file-exists-p "Makefile")
		 (file-exists-p "makefile"))
       (set (make-local-variable 'compile-command) 
	    (progn ,@body)))))

(put 'cjg-define-compile-command 'lisp-indent-function 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flymake

(defmacro cjg-with-flymake-tempfile (var &rest body)
  "Evaluate BODY with VAR bound to a tempfile suitable for use
with flymake."
  `(let ((,var (file-relative-name (flymake-init-create-temp-buffer-copy
                                    'flymake-create-temp-inplace)
                                   (file-name-directory buffer-file-name))))
     ,@body))

(put 'cjg-with-flymake-tempfile 'lisp-indent-function 1)

(cjg-eval-after-load "flymake"
  (face-spec-set 'flymake-errline '((t (:underline "OrangeRed")))  nil)
  (face-spec-set 'flymake-warnline '((t (:underline "yellow")))  nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-mode
(require 'font-lock)
(setq-default font-lock-maximum-decoration t
              font-lock-maximum-size nil)
(global-font-lock-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-insert-mode
(require 'autoinsert)
(setq-default auto-insert t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cperl-mode
(defalias 'perl-mode 'cperl-mode) 
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

(cjg-eval-after-load "cperl-mode"
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

    (flyspell-prog-mode)
    (turn-on-eldoc-mode)

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

  (cjg-define-keys cperl-mode-map
    ("C-c nc" . 'cjg-perl-insert-no-critic)
    ("C-c tp" . 'cjg-perl-toggle-test-plan)
    ("C-c ip" . 'cjg-perl-increment-test-plan)
    ("C-c sp" . 'cjg-perl-set-test-plan)))

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

(autoload 'perl-lint "perl-lint-mode" nil t)
(autoload 'perl-lint-mode "perl-lint-mode" nil t)
(autoload 'perltidy "perltidy-mode" nil t)
(autoload 'perltidy-mode "perltidy-mode" nil t)
(autoload 'perlcritic "perlcritic" nil t)
(autoload 'perlcritic-region "perlcritic" nil t)
(autoload 'perlcritic-mode "perlcritic" nil t)

(autoload 'sepia-init "sepia" nil t)
(defalias 'run-perl 'sepia-init)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tt-mode
(autoload 'tt-mode "tt-mode")
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
(autoload 'turn-on-eldoc-mode "eldoc" nil t) 

(cjg-add-hook emacs-lisp-mode-hook
  (turn-on-eldoc-mode)
  (flyspell-prog-mode)
  (substitute-pattern-with-unicode "\\<(\\(lambda\\>\\)" 'lambda))

(add-hook 'lisp-interaction-mode-hook 'cjg-emacs-lisp-mode-hook)
(add-hook 'ielm-mode-hook 'cjg-emacs-lisp-mode-hook)

;; these are from cliki:EditingLispCodeWithEmacs
(cjg-define-keys emacs-lisp-mode-map
  ("C-t" . 'transpose-sexps)
  ("C-M-t" . 'transpose-chars)
  ("C-b" . 'backward-sexp)
  ("C-M-b" . 'backward-char)
  ("C-f" . 'forward-sexp)
  ("C-M-f" . 'forward-char)
  ("(" . 'insert-parentheses)
  (")" . 'move-past-close-and-reindent))

(defun cjg-unintern-symbol-at-point ()
  "Unintern the symbol at point."
  (interactive)
  (let ((sym (symbol-at-point)))
    (unintern sym)))

(defmacro cjg-macroexpand (form)
  "Pretty print the macro expansion of `FORM'."
  `(pp (macroexpand-all ',form)))

(require 'auto-recomp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sh-mode
(cjg-eval-after-load "sh-script"
  (setq sh-basic-offset 4)

  (cjg-define-compile-command cjg:sh-set-compile-command
    (concat "bash -n "
            (file-name-nondirectory buffer-file-name)))

  (when (require 'flymake nil t)
    (defun cjg-shell-flymake-init ()
      (cjg-with-flymake-tempfile local-file
        `("bash" ("-n" ,local-file))))
    
    (defvar cjg-flymake-shell-err-line-pattern
      '("^\\(.+\\): line \\([0-9]+\\): \\(.+\\)$" 1 2 nil 3))
    
    (push cjg-flymake-shell-err-line-pattern flymake-err-line-patterns)
  
    (push '(".+\\.sh$" cjg-shell-flymake-init) flymake-allowed-file-name-masks)
    (push '("bashrc$" cjg-shell-flymake-init) flymake-allowed-file-name-masks)
    (push '("bash_profile$" cjg-shell-flymake-init) flymake-allowed-file-name-masks)
    (push '("bash_logout$" cjg-shell-flymake-init) flymake-allowed-file-name-masks))
    
  (cjg-add-hook sh-mode-hook
    (cjg:sh-set-compile-command)
    (cjg-enable 'abbrev-mode)
    (flyspell-prog-mode)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-mode
(setq c-basic-offset 8)

(cjg-define-compile-command cjg:c-set-compile-command
  (let ((file (file-name-nondirectory buffer-file-name)))
    (concat "gcc -g -Wall -o " 
	    (file-name-sans-extension file)
	    " "
	    file)))

(add-hook 'c-mode-hook 'cjg:c-set-compile-command)
(add-hook 'c-mode-hook 'flyspell-prog-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++-mode

(cjg-define-compile-command cjg:c++-set-compile-command
  (let ((file (file-name-nondirectory buffer-file-name)))
    (concat "g++ -g -Wall -o " 
	    (file-name-sans-extension file)
	    " "
	    file)))

(add-hook 'c++-mode-hook 'cjg:c++-set-compile-command)
(add-hook 'c++-mode-hook 'flyspell-prog-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode
(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'run-ruby "inf-ruby" nil t)
(autoload 'inf-ruby-keys "inf-ruby" nil t)
(autoload 'ruby-electric-mode "ruby-electric" nil t)
(autoload 'ri "ri-ruby" nil t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(cjg-eval-after-load "ruby-mode"
  (cjg-add-hook ruby-mode-hook
    (cjg-enable 'ruby-electric-mode)
    (flyspell-prog-mode))
     
  (cjg-define-keys ruby-mode-map
    ("RET" . 'ruby-reindent-then-newline-and-indent))

  (inf-ruby-keys)
  (add-to-list 'which-func-modes 'ruby-mode)

  (modify-syntax-entry ?_ "w" ruby-mode-syntax-table)

  (when (require 'flymake nil t)
    (defun flymake-ruby-init ()
      (cjg-with-flymake-tempfile local-file
        `("ruby" ("-c" ,local-file))))

    (defvar cjg-ruby-flymake-err-line-patterns
      '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3))
    
    (push cjg-ruby-flymake-err-line-patterns flymake-err-line-patterns)
    
    (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
    (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks))
  
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

(cjg-eval-after-load "ri-ruby"
  (setq ri-ruby-script (expand-file-name "~/lib/emacs/bin/ri-emacs.rb")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-mode
(cjg-eval-after-load "python"
  (cjg-add-hook python-mode-hook
    (cjg-enable 'abbrev-mode
                'outline-minor-mode)
    (turn-on-eldoc-mode)
    (flyspell-prog-mode)
    (substitute-pattern-with-unicode "\\<\\(lambda\\>\\)" 'lambda))
  
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
                     (python-in-string/comment)
                     (let ((prev-syntax (char-syntax prev)))
                       (or (equal prev-syntax ?w)
                           (equal prev-syntax ?\")
                           (equal prev-syntax ?\))))))
        (insert "self")))
    (self-insert-command n))                     
  
  (modify-syntax-entry ?_ "w" python-mode-syntax-table)

  (font-lock-add-keywords 'python-mode
                          '(("\\<\\(self\\)\\>" 1 'italic)))

  (add-to-list 'which-func-modes 'python-mode)
  (cjg-define-keys python-mode-map
    ("."  . 'cjg-python-electric-dot)
    ("\"" . 'cjg-electric-pair)
    ("\'" . 'cjg-electric-pair)
    ("("  . 'cjg-electric-pair)
    ("["  . 'cjg-electric-pair)
    ("{"  . 'cjg-electric-pair)
    (","  . 'cjg-insert-trailing-space))
  
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

(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; haskell-mode
(autoload 'haskell-mode "haskell-mode" "Major mode for editing Haskell." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell." t)

(add-to-list 'auto-mode-alist '("\\.[hg]s$"  . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hi$"     . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.l[hg]s$" . literate-haskell-mode))

(cjg-eval-after-load "haskell-mode"
  (cjg-add-hook haskell-mode-hook
    (turn-on-haskell-decl-scan)
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    (flyspell-prog-mode))
  
  (add-to-list 'which-func-modes 'haskell-mode)
  (add-to-list 'which-func-modes 'literate-haskell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; viper-mode
;; this should be moved to dot_viper.el
;(require 'viper)
;(setq-default viper-auto-indent t)
;(setq viper-search-wrap-around t)
;(setq viper-expert-level 2)
;(setq viper-inhibit-startup-message t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outline-mode
(autoload 'outline-minor-mode "outline" "Toggle Outline minor mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) 

;; this will clear the shell buffer - kept here for reference
;;(let ((comint-buffer-maximum-size 0))
;;  (comint-truncate-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell
(cjg-eval-after-load "eshell"
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
  
  (setq eshell-prompt-function 'cjg-eshell-mildly-fancy-prompt)
  (setq eshell-prompt-regexp "^[^#>\n]* [#>] ")
  (setq eshell-cp-interactive-query t
        eshell-ln-interactive-query t
        eshell-mv-interactive-query t
        eshell-rm-interactive-query t
        eshell-mv-overwrite-files nil)
  
  (setq eshell-cmpl-cycle-completions nil) 
  
  (setq eshell-scroll-show-maximum-output t
        eshell-scroll-to-bottom-on-output nil)
  
  (cjg-add-hook eshell-mode-hook
    (add-to-list 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom))
  
  ;; handle ASCII control codes
  (require 'ansi-color)
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-apply)
  
  (defun eshell/clear nil
    "Emulate the shell command clear in lisp"
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer)))
  
  (defun eshell/perldoc (&rest args)
    "Browse Perl documentation in Pod format. Similar to
eshell/man. Taken from EmacsWiki."
    (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))
  
  (defun eshell/vi (&rest args)
    "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (find-file file)
            (goto-line line))
        (find-file (pop args)))))
  
  (defun eshell/view (&rest args)
    "Invoke `view-file' on the file.  
\"view +42 foo\" also goes to line 42 in the buffer."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (view-file file)
            (goto-line line))
        (view-file (pop args))))))

;; this overrides the standard eshell/basename, so we have to be
;; careful about when it is loaded
(cjg-eval-after-load "em-unix"
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
          file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; w3m-mode
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(cjg-eval-after-load "w3m"
  (require 'w3m-search)
  (add-to-list 'w3m-search-engine-alist 
               '("search-cpan" 
                 "http://search.cpan.org/search?query=%s&mode=all"))
  (add-to-list 'w3m-search-engine-alist 
               '("google-groups-clpm" 
                 "http://groups.google.com/groups?hl=en&lr=&ie=ISO-8859-1&q=foo&btnG=Google+Search&meta=group%3Dcomp.lang.perl.misc"))
  (add-to-list 'w3m-uri-replace-alist 
               '("\\`cpan:" w3m-search-uri-replace "search-cpan"))
  
  (cjg-add-hook w3m-mode-hook
    (w3m-toggle-inline-images t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; table-mode
(autoload 'table-insert "table" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar-mode
(cjg-eval-after-load "calendar"
  (add-hook 'initial-calendar-window-hook 'mark-calendar-holidays)
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (setq diary-file "~/.diary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diary-mode
(cjg-eval-after-load "diary"
  (add-hook 'diary-display-hook 'fancy-diary-display)
  (add-hook 'diary-hook 'appt-make-list)
  (setq diary-file "~/.diary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP
(cjg-eval-after-load "tramp" 
  (require 'tramp-util)              ;enable tramp-compile
  (setq tramp-default-method "ssh")
  
  (add-to-list 'tramp-default-method-alist
               '("\\`localhost\\'" "\\`root\\'" "sudo"))
  
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "~/.ssh/config")
                                   (tramp-parse-shosts 
                                    "/etc/ssh/ssh_known_hosts")
                                   (tramp-parse-shosts 
                                    "/etc/ssh/ssh_known_hosts2")
                                   (tramp-parse-shosts "~/.ssh/known_hosts2")
                                   (tramp-parse-shosts "~/.ssh/known_hosts")))
  
  ;; shut off backups for remote files
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                                               
;;; jabber-mode                                                                                                                                                
(autoload 'jabber-connect "jabber" "connect to a jabber server" t)
(eval-after-load "jabber"
  '(progn
     (defface jabber-roster-user-xa
       '((t (:foreground "pink" :weight normal :slant italic)))
       "face for displaying extended away users"
       :group 'jabber-faces)
     (setq jabber-server "jive.home.nyu.edu"
           jabber-username "cjg5"
           jabber-nickname "grim"
           jabber-connection-type 'ssl)
     (jabber-activity-mode 1)
     (jabber-mode-line-mode 1)
     (add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)
     (add-hook 'jabber-chat-mode-hook 'flyspell-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-wiki
(autoload 'emacs-wiki-find-file "emacs-wiki" "Visit an Emacs wiki page" t)
(cjg-eval-after-load "emacs-wiki"
  (require 'emacs-wiki-srctag)
  (add-to-list 'emacs-wiki-src-tag-modes-alist 
               '("perl" . cperl-mode))
  (add-to-list 'emacs-wiki-src-tag-modes-alist 
               '("sh" . shell-script-mode))
  (add-to-list 'emacs-wiki-interwiki-names '("Wiki" . "~/Wiki/default/"))
  (add-to-list 'emacs-wiki-interwiki-names '("Planner" . "~/Plans/"))
  (add-to-list 'emacs-wiki-interwiki-names 
               ;; this should be a function that URL-escapes
               ;; the passed in tag, so things like
               ;; [[CPAN#Foo::Bar][Foo::Bar]] work properly.
               '("CPAN" . "http://search.cpan.org/perldoc?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; muse
(cjg-eval-after-load "muse"
  (require 'muse-wiki)
  (setq muse-project-alist '(("WikiPlanner" ("~/Plans"
                                             :default "TaskPool"
                                             :major-mode planner-mode
                                             :visit-link planner-visit-link)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; planner-mode
(autoload 'plan "planner" "Start planning the day." t)
(autoload 'planner-create-task-from-buffer "planner" 
  "Create a task based on current buffer" t)
(autoload 'planner-create-note "planner" 
  "Create a note on the current days plan" t)
(cjg-eval-after-load "planner"
  (require 'planner-diary)
  (require 'planner-lisp)
  (require 'planner-gnus)
  (require 'planner-bbdb)
  (require 'planner-psvn)
  (require 'planner-cyclic)
  (require 'remember-planner)
  (require 'muse)
  
  (setq planner-carry-tasks-forward t
        planner-use-task-numbers t
        planner-default-task-priority "B"
        planner-diary-use-diary t
        planner-psvn-log-edit-notice-commit-function t
        planner-psvn-log-edit-include-files-flag nil)
  
  (setq planner-day-page-template 
        "* Tasks\n\n\n* Schedule\n\n\n* Diary\n\n\n* Notes\n\n\n")
  
  (planner-diary-insinuate)
  (planner-calendar-insinuate)
  (planner-gnus-insinuate)
    
  (cjg-add-hook planner-mode-hook
    (cjg-disable 'flyspell-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remember
(autoload 'remember "remember" nil t)
(cjg-eval-after-load "remember"
  (setq remember-annotation-functions '(org-remember-annotation)
        remember-handler-functions '(org-remember-handler))
  (cjg-add-hook remember-mode-hook
    (cjg-enable 'flyspell-mode)
    (org-remember-apply-template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb
(autoload 'bbdb-insinuate-gnus "bbdb" nil t)
(cjg-eval-after-load "bbdb"
  (bbdb-initialize 'gnus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cfengine
(autoload 'cfengine-mode "cfengine" nil t)
(cjg-eval-after-load "cfengine"
  (setq cfengine-indent 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ratpoison
(autoload 'ratpoisonrc-mode  "ratpoison" "Major mode for editing ratpoisonrc files." t)
(autoload 'ratpoison-command "ratpoison" "Send a command to ratpoison." t)
(autoload 'ratpoison-line    "ratpoison" "Send current line to ratpoison as a command." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vc
(let ((cjg-vc-backends '(("vc-darcs" . DARCS)
                         ("vc-git"   . GIT))))
  (dolist (elm cjg-vc-backends)
    (when (locate-library (car elm))
      (add-to-list 'vc-handled-backends (cdr elm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; psvn
(autoload 'svn-examine "psvn" nil t )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; git
(autoload 'git-blame-mode "git-blame" "Minor mode for Git incremental blame." t)
(autoload 'git-status "git" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff
(cjg-eval-after-load "ediff"
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tnt
(cjg-eval-after-load "tnt"
  (setq tnt-default-username "bookist"
        tnt-use-timestamps t
        tnt-show-inactive-buddies t
        tnt-show-events-in-mode t))
(autoload 'tnt-open "tnt" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erc
(cjg-eval-after-load "erc"
  (require 'erc-spelling)
  (setq erc-auto-query 'window-noselect))
(autoload 'erc-select "erc" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; message-mode
(cjg-eval-after-load "message"
  (cjg-add-hook message-mode-hook
    (cjg-enable 'flyspell-mode
                'footnote-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-mode
(cjg-add-hook text-mode-hook
  (cjg-enable 'flyspell-mode
              'footnote-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css-mode-simple
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'css-mode "css-mode-simple" nil t)
(add-hook 'css-mode-hook 'flyspell-prog-mode)

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
(cjg-eval-after-load "ido"
  (setq ido-slow-ftp-host-regexps '(".*")
        ido-enable-flex-matching t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(cjg-eval-after-load "org"
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


(server-start)

;;; Local Variables: ***
;;; auto-recompile: t ***
;;; End: ***

;;; .emacs ends here
