;;; package --- Summary
;;; Commentary:
;;; Another Emacs Configuration.
;;;
;;; code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; startup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 402653184)
(add-hook
 'after-init-hook
 (lambda () (setq gc-cons-threshold 16777216)))

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq-default line-spacing 0)
(setq-default indent-tabs-mode nil)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(save-place-mode 1)

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

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package restart-emacs
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;; appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; themes
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t))

;;; font
(setq default-frame-alist '((font . "Roboto Mono-14")))

;;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package key-chord
  :straight t
  :config
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
   "M-f" 'swiper
   "M-0" 'text-scale-adjust
   "M-=" 'text-scale-increase
   "M--" 'text-scale-decrease)

  ;; helper
  (defun init-file ()
    (interactive)
    (find-file user-init-file))

  (defun browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)
    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))

  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (delq (current-buffer)
                (remove-if-not 'buffer-file-name (buffer-list)))))

  (defun kill-compilation ()
    (interactive)
    (quit-windows-on "*compilation*"))

  (defun scratch-buffer ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun message-buffer ()
    (interactive)
    (switch-to-buffer "*Messages*"))

  (defun zsh-term ()
    (interactive)
    (term "/bin/zsh"))

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
    "bK" 'kill-other-buffers
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
    "oo" 'browse-file-directory

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
;;; writing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; org
(use-package org
  :straight t
  :defer t
  :mode (("\\.org\\'" . org-mode))
  :init
  (setq org-highlight-latex-and-related '(latex))
  (setq org-list-allow-alphabetical t)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

(use-package org-superstar
  :straight t
  :defer t
  :init
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;;; latex
(use-package tex-site
  :straight auctex
  :defer t
  :mode (("\\.tex\\'" . latex-mode)))

(use-package latex-preview-pane
  :straight t
  :defer t)

;;; markdown
(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)))
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ocaml
(use-package merlin
  :straight t
  :defer t)

(use-package utop
  :straight t
  :defer t
  :init
  (setq utop-command "opam config exec -- utop -emacs")
  (add-hook 'utop-mode-hook #'company-mode))

(use-package tuareg
  :straight t
  :defer t
  :mode (("\\.ml[lipy]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode))
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (space-local-leader
    :states '(normal motion)
    :keymaps 'tuareg-mode-map
    "t" 'merlin-type-enclosing
    "d" 'merlin-destruct
    "x" 'merlin-error-next
    "s" 'utop
    "e" 'utop-eval-phrase
    "r" 'utop-eval-region
    "b" 'utop-eval-buffer
    "k" 'utop-kill))

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
;;; custom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
