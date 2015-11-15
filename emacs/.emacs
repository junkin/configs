;;dump bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(set-face-attribute 'default nil
		    :family "Inconsolata" :height 145 :weight 'normal)

;;on os cocoa enable emacs starts in / (ugh)
(setq default-directory "~sjunkin/")

;;Setup MELPA ;; need to add package set
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)


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
  (load-theme 'zenburn)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("2e5705ad7ee6cfd6ab5ce81e711c526ac22abed90b852ffaf0b316aa7864b11f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
