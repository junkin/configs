;;;;;;;;;
;; TODO move to separate files
;; TODO org mode in .dotfile


;;;;; fix bad defaults
;;dump bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(display-time-mode 1)

;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(defun prev-window ()
   (interactive)
   (other-window -1))


(define-key global-map (kbd "C-x p") 'prev-window)


(set-face-attribute 'default nil
		    :family "Inconsolata" :height 215 :weight 'normal)

;;on os cocoa enable emacs starts in / (ugh)
(setq default-directory "~/")


;;; Generic settings from various places.
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

(line-number-mode 1)
(column-number-mode 1)

;;a nice feature, changes alt-g to goto line (06-12-2002 sdj)
(global-set-key "\M-g" 'goto-line)
(global-set-key [backspace] 'backward-delete-char-untabify)

;;;;; tabs
;; http://www.emacswiki.oeg/emacs/TabCompletion
;; TODO smart-tab - smart tab versus other tab options


;;;;;;;;;;;; bring in some packages.
;; TODO mu4e doesnt come in cleanly via the below, and requires out of band build for mu
;; Package setup
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar local-packages '(projectile auto-complete epc jedi magit zenburn-theme flymake-python-pyflakes forecast org-caldav ido-vertical-mode ess bbdb bbdb-csv-import bbdb-ext bbdb-vcard calfw org-pomodoro markdown-mode org2blog))

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

;;; ido for veritcal mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;;;; markdown mode
(require 'markdown-mode)
;;;Forecast bits.
(require 'forecast)
(setq forecast-latitude 40.7500
       forecast-longitude -111.8833
       forecast-city "Salt Lake City"
       forecast-country "USA"
       forecast-units "us")



;;location of external packages
					;(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/side-lisp/jde")

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/")


;;;;;;;;;;;;;;;;;;;;  Code bits.
;;;;;;;;;; Python
;; Standard Jedi.el setting
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;;;;;kernel/c
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

;;;;; lisp
;; TODO check for ibcl in path?
(setq inferior-lisp-program "ibcl")

;;;;; Gnuplot


;;added 11/23/2004 sdj
;; TODO check for gnuplot in path?
(autoload 'gnuplot-mode "gnuplot" "gnuplot mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode))
			      auto-mode-alist))
;;;;; Maxima
;; TODO check for maxima
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))


;;
(setq search-highlight (eq window-system 'x))


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
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "20e359ef1818a838aff271a72f0f689f5551a27704bf1c9469a5c2657b417e6c" "68d36308fc6e7395f7e6355f92c1dd9029c7a672cbecf8048e2933a053cf27e6" "2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default)))
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

;;experimental support for default emacs mail program: from appendix A of mu4e manual in sandbox/configs/
(setq mail-user-agent 'mu4e-user-agent)

;; Only needed if your maildir is _not_ ~/Maildir
;; Must be a real dir, not a symlink
(setq mu4e-maildir "/home/sjunkin/Maildir/jnpr")

;; these must start with a "/", and must exist
;; (i.e.. /home/user/Maildir/sent must exist)
;; you use e.g. 'mu mkdir' to make the Maildirs if they don't
;; already exist

;; below are the defaults; if they do not exist yet, mu4e offers to
;; create them. they can also functions; see their docstrings.
(setq mu4e-sent-folder   "/Sent")
(setq mu4e-drafts-folder "/Drafts")
(setq mu4e-trash-folder  "/Trash")
(setq mu4e-refile-folder "/archive")   ;; saved messages

(setq mu4e-mu-binary "/usr/bin/mu")


;(setq mu4e-get-mail-command "offlineimap -q -a Juniper")
(setq mu4e-get-mail-command "mbsync -a jnpr")
;; smtp mail setting; these are the same that `gnus' uses.
;; do I need to setup .authinfo.   this appears to be a hardcore tbd?
;; the ip of the virutal machine runnign davmail will be dynamic, need to do something about that.
(setq
   message-send-mail-function   'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.office365.com"
   smtpmail-smtp-server         "smtp.office365.com"
   smtpmail-local-domain        "juniper.net"
   smtpmail-smtp-service        587
   smtpmail-user-mail-address "sjunkin@juniper.net"
   smtpmail-auth-credentials "~/.authinfo"
   smtpmail-queue-mail t
   smtpmail-queue-dir           "~/Maildir/queue"
   )


;mu4e cmds.
(setq mu4e-update-interval 240
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil)

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-reply-to-address "sjunkin@juniper.net"
      user-mail-address "sjunkin@juniper.net"
      user-full-name "Scot Junkin"
      )

;; custom view
;;(setq mu4e-html2text-command "w3m -dump -T text/html")
;;(setq mu4e-html2text-command "html2markdown --body-width=0")
(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 80)
(setq mu4e-view-show-images t)
(setq mu4e-view-prefer-html t)

(setq mu4e-use-fancy-chars t)
(setq mu4e-headers-flagged-mark     '("F" . "⚑")
      mu4e-headers-new-mark         '("N" . "✱")
      mu4e-headers-trashed-mark     '("T" . "♻")
      mu4e-headers-unread-mark      '("u" . "☐")
      mu4e-headers-duplicate-prefix '("=" . "≡")
      mu4e-headers-default-prefix   '("|" . "│"))


;;
;; was always alt-q'ing on reading and reply:
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'fill-paragraph)

;; tip submitted by mu4e user cpbotha
;; clean up paragraphs on sending
(add-hook 'mu4e-compose-mode-hook
          (defun cpb-compose-setup ()
            "Outgoing mails get format=flowed."
            (use-hard-newlines t 'guess)))


;; gpg

(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

;; force flyspell incompose
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;;
(setq mu4e-headers-skip-duplicates t)




;;;;;;;;;; bbdb with mu4e
;
; http://bbdb.sourceforge.net/bbdb.html#SEC13
;
;(autoload 'bbdb-insinuate-mu4e "bbdb-mu4e")
;(bbdb-initialize 'message 'mu4e)
;; (require 'bbdb-loaddefs)
;; (setq bbdb-mail-user-agent (quote message-user-agent))
;; (setq mu4e-view-mode-hook (quote (bbdb-mua-auto-update visual-line-mode)))
;; (setq mu4e-compose-complete-addresses nil)
;; (setq bbdb-mua-pop-up t)
;; (setq bbdb-mua-pop-up-window-size 5)

;;;;;;;;;; org and mu4e playing nice

;;another experimental  bit - org for message composition.
(require 'org-mu4e)

;; store links to message if in header view rather than to the search query.

(setq org-mu4e-link-query-in-headers-mode nil)

(setq org-capture-templates
      '(
	("t" "todo" entry (file+headline "~/Dropbox/org/todo1.org" "Tasks")
	 "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
	("m" "Meeting" entry (file (buffer-name))
	 "*%?\nEntered on %U\n  %i\n  %a")
	("I" "interview" entry (file "~/Dropbox/org/interviews.org" "Tasks")
	 "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")

	("M" "Template for capturing meeting minutes" plain (file (buffer-name)) "" :immediate-finish t)
	("P" "process-soon" entry (file+headline "~/Dropbox/org/todo1.org" "Todo")
	 (org-insert-heading) "TODO %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
	)
)



;;;;;;; org

;;; pomodoro stuff, take 2
(require 'org-pomodoro)

(setq org-timer-default-timer 1)
(add-hook 'org-timer-done-hook' (lambda () (start-process "orgmode" nil "/ usr / bin / notify-send" "Orgmode: Pomodoro complete, rest")))

;;;; mobileorg
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
(setq org-mobile-files "~/Dropbox/org")

;;TODO need to use this instead..
(setq org-root-directory "~/Dropbox/org")

;;; odt output
(setq org-odt-preferred-output-format     "docx")

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;;;;;;;;;;;; bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;;; babel
;; TODO make sure langauges are installed and setup
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (emacs-lisp . nil)))


(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))

;;;;;;; calenderinge
(require 'calfw)
(require 'calfw-org)

;;;;; org-caldav test.
(require 'org-caldav)

(setq org-caldav-url "http://sjunkin-vm:1080/users/")
(setq org-caldav-calendar-id "sjunkin@juniper.net/calendar")
(setq org-caldav-uuid-extension ".EML")
(setq org-caldav-inbox "~/Dropbox/org/calendar_jnpr.org")

;;; transparency test.
(defun transparency (value)
  "Sets transparency of the frame window"
  (interactive "nTransparency Vakue 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(set-frame-parameter (selected-frame) 'alpha '(85 50))



;;;; org2blog

(setq load-path (cons "~/.emacs.d/org2blog" load-path))
(require 'org2blog-autoloads)
