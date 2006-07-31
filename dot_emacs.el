;;; .emacs -- emacs customizations -*- Mode: emacs-lisp; auto-recompile: t; -*-
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

;;for shame
;(add-to-list 'load-path "~/lib/emacs/site-lisp/ngnus-0.3")
(add-to-list 'load-path "~/lib/emacs/site-lisp/gnus-cvs/lisp")

(defvar *cjg-local-config-file-name*
  (concat (getenv "HOME")
	  "/lisp/site-lisp/site-"
	  (car (split-string system-name "\\."))
	  ".el"))

(cjg-load-file-if-exists "/usr/share/emacs/site-lisp/site-gentoo.el")
(cjg-load-file-if-exists *cjg-local-config-file-name*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general predicates
;(defmacro define-location-predicate (name regex)

(defun at-ncbi-p ()
  "Returns true if running Emacs at NCBI."
  (save-match-data
    (if (string-match "\\.ncbi\\.nlm\\.nih\\.gov$" (system-name))
	t
      nil)))

(defun at-nyu-p ()
  "Returns true if running Emacs at NYU."
  (save-match-data
    (if (string-match "\\.nyu\\.edu$" (system-name))
	t
      nil)))

(defalias 'at-work-p 'at-nyu-p)

(defun cjg:at-home-p ()
  "Returns true if running Emacs at home."
  (not (at-work-p)))

(defun macosx-p ()
  "Return true if this Emacs is running on MacOSX."
  (string= system-type "darwin"))

(defun carbon-p ()
  "Returns true if running under Carbon."
  (string= window-system "mac"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general config
(column-number-mode t)
(line-number-mode t)
(scroll-bar-mode -1) 			;no scrollbar
(tool-bar-mode -1)			;no toolbar
(menu-bar-mode -1)			;no menubar
(show-paren-mode 1)
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

(defvar *cjg-work-email-address* "chris.grim@nyu.edu")

(when (at-work-p)
  (setq user-mail-address *cjg-work-email-address*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; macosx specific config
(when (macosx-p)
  (when (carbon-p)			; set color scheme for Carbon
    (dolist (elm '((foreground-color . "white")
		   (background-color . "black")
		   (cursor-color . "coral")))
      (add-to-list 'default-frame-alist elm))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global keybindings
(global-set-key (kbd "%")       'cjg-match-paren)
(global-set-key (kbd "C-c o")   'occur)
(global-set-key (kbd "C-c g")   'goto-line)
(global-set-key (kbd "C-c #")   'comment-region)
(global-set-key (kbd "C-c $")   'uncomment-region)
(global-set-key (kbd "C-c t")   'todo-show)
(global-set-key (kbd "C-c i")   'todo-insert-item)
(global-set-key (kbd "C-c cc")  'compile)
(global-set-key (kbd "C-x C-b") 'cjg-buffer-list)
(global-set-key (kbd "RET")     'newline-and-indent)
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; duplicate M-x

;; (defvar *cjg-key-bindings*
;;   '(("%"       'cjg-match-paren)
;;     ("C-c o"   'occur)
;;     ("C-c g"   'goto-line)
;;     ("C-c #"   'comment-region)
;;     ("C-c $"   'uncomment-region)
;;     ("C-c t"   'todo-show)
;;     ("C-c i"   'todo-insert-item)
;;     ("C-c cc"  'compile)
;;     ("C-x C-b" 'cjg-buffer-list)))

;; (dolist (key *cjg-key-bindings*)
;;   (global-set-key (kbd (car key)) (cadr key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; general functions
(defun cjg-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %.
Very blasphemous."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun cjg-improved-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %.
Very blasphemous."
  (interactive "p")
  (let ((current-syntax (string (char-syntax (char-after (point))))))
    (cond ((equal current-syntax "(")
           (forward-list 1)
           (backward-char 1))
          ((equal current-syntax ")")
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

(defmacro def-compile-command (name &rest body)
  `(defun ,name ()
     (unless (or (null buffer-file-name)
		 (file-exists-p "Makefile")
		 (file-exists-p "makefile"))
       (set (make-local-variable 'compile-command) 
	    (progn ,@body)))))

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

(eval-after-load "cperl-mode"
  '(progn
     (setq cperl-electric-keywords nil
	   cperl-electric-parens nil
	   cperl-invalid-face nil
	   cperl-under-as-char t
	   cperl-indent-level 2)
     (when (not (null cperl-mode-abbrev-table))
       (define-abbrev cperl-mode-abbrev-table "__p" "__PACKAGE__")
       (define-abbrev cperl-mode-abbrev-table "__d" "__DATA__")
       (define-abbrev cperl-mode-abbrev-table "__e" "__END__"))))

(def-compile-command cjg:cperl-set-compile-command
  (concat "perl -cw " 
	  (file-name-nondirectory buffer-file-name)))

(defun cjg:cperl-mode-setup ()
  "Setup CPerl-Mode. Called from 'cperl-mode-hook'."
  (cjg:cperl-set-compile-command)
  (auto-insert-mode 1)
  (abbrev-mode 1))

(add-hook 'cperl-mode-hook 'cjg:cperl-mode-setup)

(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

(defun perldoc (args)
  "Like man, but use perldoc instead."
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man args)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tt-mode
(autoload 'tt-mode "tt-mode")
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-lisp-mode
(autoload 'turn-on-eldoc-mode "eldoc" nil t) 

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; these are from cliki:EditingLispCodeWithEmacs
(define-key emacs-lisp-mode-map (kbd "C-t") 'transpose-sexps)
(define-key emacs-lisp-mode-map (kbd "C-M-t") 'transpose-chars)
(define-key emacs-lisp-mode-map (kbd "C-b") 'backward-sexp)
(define-key emacs-lisp-mode-map (kbd "C-M-b") 'backward-char)
(define-key emacs-lisp-mode-map (kbd "C-f") 'forward-sexp)
(define-key emacs-lisp-mode-map (kbd "C-M-f") 'forward-char)
(define-key emacs-lisp-mode-map (kbd "(") 'insert-parentheses)
(define-key emacs-lisp-mode-map (kbd ")") 'move-past-close-and-reindent)

(defun cjg-unintern-symbol-at-point ()
  "Unintern the symbol at point."
  (interactive)
  (let ((sym (symbol-at-point)))
    (unintern sym)))

(require 'auto-recomp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sh-mode
(setq sh-basic-offset 4)

(def-compile-command cjg:sh-set-compile-command
  (concat "bash -n "
	  (file-name-nondirectory buffer-file-name)))

(add-hook 'sh-mode-hook 'cjg:sh-set-compile-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C-mode
(setq c-basic-offset 8)

(def-compile-command cjg:c-set-compile-command
  (let ((file (file-name-nondirectory buffer-file-name)))
    (concat "gcc -g -Wall -o " 
	    (file-name-sans-extension file)
	    " "
	    file)))

(add-hook 'c-mode-hook 'cjg:c-set-compile-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++-mode

(def-compile-command cjg:c++-set-compile-command
  (let ((file (file-name-nondirectory buffer-file-name)))
    (concat "g++ -g -Wall -o " 
	    (file-name-sans-extension file)
	    " "
	    file)))

(add-hook 'c++-mode-hook 'cjg:c++-set-compile-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ruby-mode
(autoload 'ruby-mode "ruby-mode" nil t)
(autoload 'run-ruby "inf-ruby" nil t)
(autoload 'inf-ruby-keys "inf-ruby" nil t)
(autoload 'ruby-electric-mode "ruby-electric" nil t)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


(eval-after-load "ruby-mode"
  '(progn
     (defun cjg-ruby-mode-setup ()
       (ruby-electric-mode 1))
     
     (define-key ruby-mode-map (kbd "RET")
       'ruby-reindent-then-newline-and-indent)
     
     (inf-ruby-keys)
     (add-hook 'ruby-mode-hook 'cjg-ruby-mode-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-mode
(eval-after-load "python"
  '(progn
     (defun cjg-python-mode-setup ()
       (abbrev-mode 1)
       (outline-minor-mode 1)
       (turn-on-eldoc-mode))

     (defun pylint ()
       "Run pylint against the file visited by the current buffer.
Checks if unsaved buffers need to be saved."
       (interactive)
       (let ((command (concat "pylint --parseable=y \""
                              (buffer-file-name (current-buffer))
                              "\"")))
         (save-some-buffers (not compilation-ask-about-save) nil) ; save  files.
         (compilation-start command)))

     (defun cjg-python-electric-dot (n)
       (interactive "p")
       (let ((prev (char-before)))
	 (when (not (or (null prev)
			(python-in-string/comment)
			(equal (char-syntax prev) ?w)))
	   (insert "self")))
       (self-insert-command n))
     
     (modify-syntax-entry ?_ "w" python-mode-syntax-table)
     
     (define-key python-mode-map "." 'cjg-python-electric-dot)
     (define-key python-mode-map "\"" 'cjg-electric-pair)
     (define-key python-mode-map "\'" 'cjg-electric-pair)
     (define-key python-mode-map "(" 'cjg-electric-pair)
     (define-key python-mode-map "[" 'cjg-electric-pair)
     (define-key python-mode-map "{" 'cjg-electric-pair)
     (define-key python-mode-map "," 'cjg-trailing-space)
     
     (define-abbrev python-mode-abbrev-table "__i" "__init__")
     (define-abbrev python-mode-abbrev-table "__m" "__main__")
     (define-abbrev python-mode-abbrev-table "__v" "__version__")
     (define-abbrev python-mode-abbrev-table "__s" "__str__")
     (define-abbrev python-mode-abbrev-table "__n" "__name__")
     (define-abbrev python-mode-abbrev-table "__m" "__main__")     
     
     (add-hook 'python-mode-hook 'cjg-python-mode-setup)))

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
;;; todo-mode
(autoload 'todo-mode "todo-mode" "Major mode for editing TODO lists." t)
(autoload 'todo-show "todo-mode" "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode" "Add TODO item." t)

(add-hook 'todo-mode-hook 'outline-minor-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shell-mode
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t) 
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on) 

;; this will clear the shell buffer - kept here for reference
;;(let ((comint-buffer-maximum-size 0))
;;  (comint-truncate-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell
(eval-after-load "eshell"
  '(progn
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

     (defun cjg-eshell-setup ()
       (add-to-list 'eshell-output-filter-functions
		    'eshell-postoutput-scroll-to-bottom))

     (add-hook 'eshell-mode-hook 'cjg-eshell-setup)

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
	   (view-file (pop args)))))))

;; this overrides the standard eshell/basename, so we have to be
;; careful about when it is loaded
(eval-after-load "em-unix"
  '(progn
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
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(eval-after-load "w3m"
  '(progn
    (require 'w3m-search)
    (add-to-list 'w3m-search-engine-alist 
		 '("search-cpan" 
		   "http://search.cpan.org/search?query=%s&mode=all"))
    (add-to-list 'w3m-search-engine-alist 
		 '("google-groups-clpm" 
		   "http://groups.google.com/groups?hl=en&lr=&ie=ISO-8859-1&q=foo&btnG=Google+Search&meta=group%3Dcomp.lang.perl.misc"))
    (add-to-list 'w3m-uri-replace-alist 
		 '("\\`cpan:" w3m-search-uri-replace "search-cpan"))

    (defun cjg-w3m-setup ()
      "Setup function for w3m-mode. Called from w3m-mode-hook."
      (w3m-toggle-inline-images t))

    (add-hook 'w3m-mode-hook 'cjg-w3m-setup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; table-mode
(autoload 'table-insert "table" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calendar-mode
(eval-after-load "calendar"
  '(progn
     (add-hook 'initial-calendar-window-hook 'mark-calendar-holidays)
     (add-hook 'diary-display-hook 'fancy-diary-display)
     (setq diary-file "~/.diary")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diary-mode
(eval-after-load "diary"
  '(progn
     (add-hook 'diary-display-hook 'fancy-diary-display)
     (add-hook 'diary-hook 'appt-make-list)
     (setq diary-file "~/.diary")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAMP
(eval-after-load "tramp" 
  '(progn
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
                  (cons tramp-file-name-regexp nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jabber-mode
(eval-after-load "jabber"
  '(progn
     (defface jabber-roster-user-xa
       '((t (:foreground "pink" :weight normal :slant italic)))
       "face for displaying extended away users"
       :group 'jabber-faces)
     (setq jabber-server "jabber.ncbi.nlm.nih.gov"
	   jabber-username "grim"
	   jabber-nickname "grim")
     (jabber-activity-mode 1)
     (jabber-mode-line-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; emacs-wiki
(autoload 'emacs-wiki-find-file "emacs-wiki" "Visit an Emacs wiki page" t)
(eval-after-load "emacs-wiki"
  '(progn
     (require 'emacs-wiki-srctag)
     (add-to-list 'emacs-wiki-src-tag-modes-alist 
		  '("perl" . cperl-mode))
     (add-to-list 'emacs-wiki-src-tag-modes-alist 
		  '("sh" . shell-script-mode))
     (add-to-list 'emacs-wiki-interwiki-names
		  '("SystemsWiki" . "http://graceland:6224/irb/wiki/wiki.cgi?"))
     (add-to-list 'emacs-wiki-interwiki-names
		  '("BlastWiki" . "http://yar.ncbi.nlm.nih.gov:6224/staff/coulouri/awkiawki/awki.cgi/"))
     (add-to-list 'emacs-wiki-interwiki-names '("Wiki" . "~/Wiki/default/"))
     (add-to-list 'emacs-wiki-interwiki-names '("Planner" . "~/Plans/"))
     (add-to-list 'emacs-wiki-interwiki-names 
		  ;;XXX:
		  ;; this should be a function that URL-escapes
		  ;; the passed in tag, so things like
		  ;; [[CPAN#Foo::Bar][Foo::Bar]] work properly.
		  '("CPAN" . "http://search.cpan.org/perldoc?"))

     (setq emacs-wiki-projects
	   '(("default" . ((emacs-wiki-directories . ("~/Wiki/default"))))
	     ("graceland" . ((emacs-wiki-directories . ("~/Wiki/graceland"))
			     (emacs-wiki-publishing-directory . "/net/graceland/web/private/htdocs/staff/grim")))))))
	   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; planner-mode
(autoload 'plan "planner" "Start planning the day." t)
(autoload 'planner-create-task-from-buffer "planner" 
  "Create a task based on current buffer" t)
(autoload 'planner-create-note "planner" 
  "Create a note on the current days plan" t)
(eval-after-load "planner"
  '(progn
     (require 'emacs-wiki)
     (require 'planner-diary)
     (require 'planner-lisp)
     (require 'planner-gnus)
     (require 'planner-bbdb)
     (require 'planner-log-edit)
     (require 'planner-cyclic)
     (require 'remember-planner)

     (setq planner-carry-tasks-forward t
	   planner-use-task-numbers t
	   planner-default-task-priority "B"
	   planner-diary-use-diary t)
     (setq planner-day-page-template 
	   "* Tasks\n\n\n* Schedule\n\n\n* Diary\n\n\n* Notes\n\n\n")
     
     (planner-diary-insinuate)
     (planner-calendar-insinuate)
     (planner-gnus-insinuate)

     (setq remember-handler-functions '(remember-planner-append)
	   remember-annotation-functions planner-annotation-functions)

     (defun cjg-planner-mode-setup ()
       (flyspell-mode -1))
     
     (add-hook 'planner-mode-hook 'cjg-planner-mode-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remember
(autoload 'remember "remember" nil t)

(eval-after-load "remember"
  '(progn
     (defun cjg-remember-mode-setup ()
       (flyspell-mode 1))
     (add-hook 'remember-mode-hook 'cjg-remember-mode-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bbdb
(autoload 'bbdb-insinuate-gnus "bbdb" nil t)
(eval-after-load "bbdb"
  '(progn
     (bbdb-initialize 'gnus)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cfengine
(autoload 'cfengine-mode "cfengine" nil t)
(eval-after-load "cfengine"
  '(progn
     (setq cfengine-indent 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newsticker
(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
(autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)
(eval-after-load "newsticker"
  '(progn
     (setq newsticker-url-list '(("CDATA format testing" "http://graceland:6224/staff/grim/test.xml")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ratpoison
(autoload 'ratpoisonrc-mode  "ratpoison" "Major mode for editing ratpoisonrc files." t)
(autoload 'ratpoison-command "ratpoison" "Send a command to ratpoison." t)
(autoload 'ratpoison-line    "ratpoison" "Send current line to ratpoison as a command." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vc
;(add-to-list 'vc-handled-backends 'DARCS) ;setup vc-darcs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ediff
(defun cjg:ediff-setup ()
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(add-hook 'ediff-load-hook 'cjg:ediff-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tnt
(eval-after-load "tnt"
  '(progn
     (setq tnt-default-username "bookist"
           tnt-use-timestamps t
           tnt-show-inactive-buddies t
           tnt-show-events-in-mode t)))

(autoload 'tnt-open "tnt" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erc
(eval-after-load "erc"
  '(progn
     (require 'erc-spelling)
     (setq erc-auto-query 'window-noselect)))

(autoload 'erc-select "erc" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; itunes
(eval-after-load "itunes"
  '(progn
     (global-set-key (kbd "C-x 5n") 'cjg-itunes-next-track)
     (global-set-key (kbd "C-x 5p") 'cjg-itunes-previous-track)
     (global-set-key (kbd "C-x 5s") 'cjg-itunes-stop)
     (global-set-key (kbd "C-x 5l") 'cjg-itunes-play)))

(when (fboundp 'do-applescript)
  (autoload 'cjg5-itunes-next-track "itunes" nil t)
  (autoload 'cjg5-itunes-previuos-track "itunes" nil t)
  (autoload 'cjg5-itunes-stop "itunes" nil t)
  (autoload 'cjg5-itunes-play "itunes" nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; message-mode
(defun cjg-message-mode-setup ()
  (flyspell-mode 1)
  (footnote-mode 1))

(add-hook 'message-mode-hook 'cjg-message-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; text-mode
(defun cjg-text-mode-setup ()
  (flyspell-mode 1)
  (footnote-mode 1))

(add-hook 'text-mode-hook 'cjg-text-mode-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; css-mode-simple
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'css-mode "css-mode-simple" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; work and homebrew modes
;(require 'ncbi-pxe)
;(require 'papyrus)
;(require 'ncbi-facilities)
;(require 'ncbi-hints)

(server-start)

;;; Local Variables: ***
;;; auto-recompile: t ***
;;; End: ***

;;; .emacs ends here
