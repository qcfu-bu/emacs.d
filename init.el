;;; package --- Summary
;;; Commentary:
;;; My Horrible Emacs Configuration.
;;;
;;; code:

;;; hacks: adjusting garbage collection threshold to improve load times
(setq gc-cons-threshold 402653184)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 16777216)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; package manager
(defvar bootstrap-version)
(setq straight-process-buffer " *straight-process*")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; sanity
(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-arguments nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "PATSHOME")
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :straight t)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(save-place-mode 1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; emacs
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package counsel
  :straight t
  :config
  (ivy-mode))

(use-package company
  :straight t
  :init
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package ivy-prescient
  :straight t
  :after ivy
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

(use-package company-prescient
  :straight t
  :after company
  :config
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook #'show-smartparens-mode)
  (add-hook 'prog-mode-hook #'smartparens-mode))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tools

;;; shell
(use-package shell-pop
  :straight t
  :config
  (setq shell-pop-shell-type
	(quote ("term" "*terminal-popup*"
		(lambda nil (term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;;; git
(use-package magit
  :straight t)

(use-package evil-magit
  :straight t
  :after magit)

;;; treemacs
(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t
  :after treemacs)

;;; projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :straight t
  :after projectile
  :config
  (counsel-projectile-mode))

;;; checker
(use-package flycheck
  :straight t
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode))

(use-package flyspell
  :straight t
  :config
  (add-hook 'text-mode-hook #'flyspell-mode))

;;; todo
(use-package hl-todo
  :straight t
  :config
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (setq hl-todo-keyword-faces
      '(("TODO"   . "orange")
        ("FIXME"  . "red")
        ("DEBUG"  . "purple")
        ("GOTCHA" . "lime green")
        ("STUB"   . "deep sky blue"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; appearance

;;; modeline
(use-package smart-mode-line
  :straight t
  :init
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup))

;;; font
(setq default-frame-alist '((font . "Monaco-14")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; keybindings
(use-package key-chord
  :straight t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1))

(use-package general
  :straight t
  :config

  ;; sanity
  (general-define-key
   :keymaps 'company-active-map
   "RET" 'company-complete-selection)

  (general-define-key
   "M-x" 'counsel-M-x
   "M-f" 'swiper)

  ;; helper
  (defun init-file ()
    (interactive)
    (find-file user-init-file))

  (defun kill-compilation ()
    (interactive)
    (quit-windows-on "*compilation*"))

  (defun scratch-buffer ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun message-buffer ()
    (interactive)
    (switch-to-buffer "*Messages*"))

  ;; space leader
  (general-create-definer space-leader
    :prefix "SPC")

  (general-create-definer space-local-leader
    :prefix "SPC m")

  ;; leader key
  (space-leader
    :states '(normal motion)
    :keymaps 'override

    ;; critical
    "SPC" 'counsel-M-x
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs

    ;; files
    "ff" 'find-file
    "fr" 'counsel-recentf
    "fs" 'save-buffer
    "fd" 'dired
    "fi" 'init-file

    ;; buffer
    "bb" 'ivy-switch-buffer
    "bB" 'list-buffers
    "bk" 'kill-buffer
    "bd" 'kill-this-buffer
    "be" 'eval-buffer
    "bn" 'evil-next-buffer
    "bp" 'evil-prev-buffer
    "bs" 'scratch-buffer
    "bm" 'message-buffer

    ;; windows
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wn" 'evil-window-next
    "wp" 'evil-window-prev
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wd" 'evil-window-delete
    "ww" 'delete-other-windows

    ;; describe
    "hv" 'describe-variable
    "hk" 'describe-key
    "hf" 'describe-function
    "ht" 'counsel-load-theme

    ;; open
    "oo" 'make-frame

    ;; shell
    "'" 'shell-pop
    "\"" 'zsh-term

    ;; compile
    "cc" 'compile
    "cd" 'kill-compilation

    ;; magit
    "gg" 'magit-status

    ;; projectile
    "pp" 'projectile-command-map

    ;; treemacs
    "tt" 'treemacs)

  ;; normal state
  (general-define-key
   :states 'normal
   "gcc" 'comment-line)

  ;; insert state
  (general-define-key
   :states 'insert
   (general-chord "jk") 'evil-normal-state
   (general-chord "kj") 'evil-normal-state)

  ;; visual state
  (general-define-key
   :states 'visual
   "gc" 'comment-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; writing

;;; org
(use-package org
  :straight t
  :defer t
  :mode (("\\.org\\'" . org-mode)))

;;; latex
(use-package latex-preview-pane
  :straight t
  :defer t)

(use-package tex-site
  :straight auctex
  :defer t
  :mode (("\\.tex\\'" . latex-mode)))

;;; markdown
(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; languages

;;; ocaml
(use-package merlin
  :straight t
  :defer t)

(use-package utop
  :straight t
  :defer t
  :init
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package tuareg
  :straight t
  :defer t
  :mode (("\\.ml[lipy]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode))
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

;;; coq
(use-package proof-general
  :straight t
  :defer t
  :mode (("\\.v\\'" . coq-mode))
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package company-coq
  :straight t
  :defer t)

;;; ats
(use-package smart-compile
  :straight t)

(load-file "~/.emacs.d/obscure/ats2-mode.el")

;;; fython
(load-file "~/.emacs.d/obscure/fython.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
