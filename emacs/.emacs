;;dump bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun prev-window ()
   (interactive)
   (other-window -1))

(define-key global-map (kbd "C-x p") 'prev-window)

(set-face-attribute 'default nil
		    :family "Inconsolata" :height 145 :weight 'normal)

;;on os cocoa enable emacs starts in / (ugh)
(setq default-directory "~sjunkin/")

;; Package setup

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar local-packages '(projectile auto-complete epc jedi magit zenburn-theme flymake-python-pyflakes forecast org-caldav ido-vertical-mode))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

;; This delightful bit adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
	(package-install p)))))

;; Global Jedi config vars

(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

;; Helper functions

;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; Ensure that PATH is taken from shell
;; Necessary on some environments without virtualenv
;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Package specific initialization
(add-hook
 'after-init-hook
 '(lambda ()

    ;; Looks like you need Emacs 24 for projectile
    (unless (< emacs-major-version 24)
      (require 'projectile)
      (projectile-global-mode))

    ;; Auto-complete
    (require 'auto-complete-config)
    (ac-config-default)

    ;; Uncomment next line if you like the menu right away
    ;; (setq ac-show-menu-immediately-on-auto-complete t)

    ;; Can also express in terms of ac-delay var, e.g.:
    ;;   (setq ac-auto-show-menu (* ac-delay 2))

    ;; Jedi
    (require 'jedi)

    ;; (Many) config helpers follow

    ;; Alternative methods of finding the current project root
    ;; Method 1: basic
    (defun get-project-root (buf repo-file &optional init-file)
      "Just uses the vc-find-root function to figure out the project root.
       Won't always work for some directory layouts."
      (let* ((buf-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
	     (project-root (vc-find-root buf-dir repo-file)))
	(if project-root
	    (expand-file-name project-root)
	  nil)))

    ;; Method 2: slightly more robust
    (defun get-project-root-with-file (buf repo-file &optional init-file)
      "Guesses that the python root is the less 'deep' of either:
         -- the root directory of the repository, or
         -- the directory before the first directory after the root
            having the init-file file (e.g., '__init__.py'."

      ;; make list of directories from root, removing empty
      (defun make-dir-list (path)
        (delq nil (mapcar (lambda (x) (and (not (string= x "")) x))
                          (split-string path "/"))))
      ;; convert a list of directories to a path starting at "/"
      (defun dir-list-to-path (dirs)
        (mapconcat 'identity (cons "" dirs) "/"))
      ;; a little something to try to find the "best" root directory
      (defun try-find-best-root (base-dir buffer-dir current)
        (cond
         (base-dir ;; traverse until we reach the base
          (try-find-best-root (cdr base-dir) (cdr buffer-dir)
                              (append current (list (car buffer-dir)))))

         (buffer-dir ;; try until we hit the current directory
          (let* ((next-dir (append current (list (car buffer-dir))))
                 (file-file (concat (dir-list-to-path next-dir) "/" init-file)))
            (if (file-exists-p file-file)
                (dir-list-to-path current)
              (try-find-best-root nil (cdr buffer-dir) next-dir))))

         (t nil)))

      (let* ((buffer-dir (expand-file-name (file-name-directory (buffer-file-name buf))))
             (vc-root-dir (vc-find-root buffer-dir repo-file)))
        (if (and init-file vc-root-dir)
            (try-find-best-root
             (make-dir-list (expand-file-name vc-root-dir))
             (make-dir-list buffer-dir)
             '())
          vc-root-dir))) ;; default to vc root if init file not given

    ;; Set this variable to find project root
    (defvar jedi-config:find-root-function 'get-project-root-with-file)

    (defun current-buffer-project-root ()
      (funcall jedi-config:find-root-function
               (current-buffer)
               jedi-config:vcs-root-sentinel
               jedi-config:python-module-sentinel))

    (defun jedi-config:setup-server-args ()
      ;; little helper macro for building the arglist
      (defmacro add-args (arg-list arg-name arg-value)
        `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))
      ;; and now define the args
      (let ((project-root (current-buffer-project-root)))

        (make-local-variable 'jedi:server-args)

        (when project-root
          (message (format "Adding system path: %s" project-root))
          (add-args jedi:server-args "--sys-path" project-root))

        (when jedi-config:with-virtualenv
          (message (format "Adding virtualenv: %s" jedi-config:with-virtualenv))
          (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv))))

    ;; Use system python
    (defun jedi-config:set-python-executable ()
      (set-exec-path-from-shell-PATH)
      (make-local-variable 'jedi:server-command)
      (set 'jedi:server-command
           (list (executable-find "python") ;; may need help if running from GUI
                 (cadr default-jedi-server-command))))

    ;; Now hook everything up
    ;; Hook up to autocomplete
    (add-to-list 'ac-sources 'ac-source-jedi-direct)

    ;; Enable Jedi setup on mode start
    (add-hook 'python-mode-hook 'jedi:setup)

    ;; Buffer-specific server options
    (add-hook 'python-mode-hook
              'jedi-config:setup-server-args)
    (when jedi-config:use-system-python
      (add-hook 'python-mode-hook
                'jedi-config:set-python-executable))

    ;; And custom keybindings
    (defun jedi-config:setup-keys ()
      (local-set-key (kbd "M-.") 'jedi:goto-definition)
      (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
      (local-set-key (kbd "M-?") 'jedi:show-doc)
      (local-set-key (kbd "M-/") 'jedi:get-in-function-call))

    ;; Don't let tooltip show up automatically
    (setq jedi:get-in-function-call-delay 10000000)
    ;; Start completion at method dot
    (setq jedi:complete-on-dot t)
    ;; Use custom keybinds
    (add-hook 'python-mode-hook 'jedi-config:setup-keys)

    ))

;;; ido for veritcal mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)


;;;Forecast bits.
(require 'forecast)
(setq forecast-latitude 40.7500
       forecast-longitude -111.8833
       forecast-city "Salt Lake City"
       forecast-country "USA"
       forecast-units "us")




;;kernel/c
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/src/linux-trees")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))



(line-number-mode 1)
(column-number-mode 1)

(setq inferior-lisp-program "ibcl")


;;location of external packages
					;(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/side-lisp/jde")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")

;;added 11/23/2004 sdj
(autoload 'gnuplot-mode "gnuplot" "gnuplot mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode))
			      auto-mode-alist))

(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))

(setq search-highlight (eq window-system 'x))



;;a nice feature, changes alt-g to goto line (06-12-2002 sdj)
(global-set-key "\M-g" 'goto-line)
(global-set-key [backspace] 'backward-delete-char-untabify)
;; Some Java Development Env. Stuff 
;;(require 'jde)
;;(setq jde-web-browser "/usr/local/bin/netscape4 -remote")
;;(setq jde-doc-dir "/usr/share/doclib/java/")



(setq path-to-ctags "~/mytags")

;;tag creation..
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -f %s -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )
;;pomodoro stuff
;;(require 'cl)
;;(require 'todochiku)

;;Python Stuff
					;(require 'virtualenvwrapper)
					;(venv-initialize-interactive-shells) ;; if you want interactive shell support
					;(venv-initialize-eshell) ;; if you want eshell support
					;(setq venv-location "~/sandbox/lt/")
					;(add-to-list 'load-path "~/sandbox/emacs_bits/python-django.el")
					;(require 'python-django)

;;(when (load "flymake" t)
;;  (defun flymake-pyflakes-init ()
;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;             'flymake-create-temp-inplace))lo
;;     (local-file (file-relative-name
;;          temp-file
;;          (file-name-directory buffer-file-name))))
;;    ;(list "/usr/local/bin/pyflakes"  (list local-file))))
;;    (list "/Users/junkin/bin/pycheckers"  (list local-file))))
;;    ;(list "/Users/junkin/bin/flake8_ck"  (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks
;;           '("\\.py\\'" flymake-pyflakes-init)))

					;(add-hook 'find-file-hook 'flymake-find-file-hook)
					;(setq flymake-log-level 3)

;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


(setq minibuffer-max-depth nil)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  (load-theme 'zenburn))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default)))
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'flymake)

(defun flymake-slax-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "slaxproc" (list "--check" local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.slax$"
	      flymake-slax-init
	      flymake-simple-cleanup
	      flymake-get-real-file-name)
	    flymake-allowed-file-name-masks))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EMAIL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example configuration for mu4e

;; make sure mu4e is in your load-path
(require 'mu4e)
;;another experimental  bit - org for message composition.
(require 'org-mu4e)

;;experimental support for default emacs mail program: from appendix A of mu4e manual in sandbox/configs/
(setq mail-user-agent 'mu4e-user-agent)

;; Only needed if your maildir is _not_ ~/Maildir
;; Must be a real dir, not a symlink
;;(setq mu4e-maildir "/home/user/Maildir")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder  "/Trash")
(setq mu4e-mu-binary "/usr/local/bin/mu")

(setq mu4e-get-mail-command "offlineimap -q -a Juniper")

;; smtp mail setting; these are the same that `gnus' uses.
;; do I need to setup .authinfo.   this appears to be a hardcore tbd?
;; the ip of the virutal machine runnign davmail will be dynamic, need to do something about that.
(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "10.10.0.114"
   smtpmail-smtp-server         "10.10.0.114"
   smtpmail-local-domain        "juniper.net"
   smtpmail-smtp-service        1025
   smtpmail-user-mail-address "sjunkin@juniper.net"
   smtpmail-auth-credentials "~/.authinfo")

;mu4e cmds.
(setq mu4e-html2text-command "w3m -T text/html"
      mu4e-update-interval 240
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-reply-to-address "sjunkin@juniper.net"
      user-mail-address "sjunkin@juniper.net"
      user-full-name "Scot Junkin"
      )
;;;;;;; org

;;;;;;;;;;;; bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-directory "~/org")
(setq org-agenda-files '("~/org"))


;;;;; org-caldav test.
(require 'org-caldav)
(setq org-caldav-url "http://10.10.0.114:1080/users"
           org-caldav-calendar-id "sjunkin@juniper.net/calendar"
           org-caldav-uuid-extension ".EML")

(setq org-caldav-inbox "~/org/calendar_jnpr.org")

