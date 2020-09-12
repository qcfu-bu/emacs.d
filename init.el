;;; package --- Summary
;;; Commentary:
;;; qcfu-bu's Emacs config
;;;
;;; code:

;; hacks
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))


;; bootstrap package manager
(defvar bootstrap-version)
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


;; sanity
(use-package exec-path-from-shell
  :straight t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq default-frame-alist '((font . "Monaco-14")))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; emacs
(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
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
  :config
  (add-hook 'prog-mode-hook #'company-mode))

(use-package ivy-prescient
  :straight t
  :config
  (ivy-prescient-mode)
  (prescient-persist-mode))

(use-package company-prescient
  :straight t
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

(use-package flycheck
  :straight t
  :config
  (add-hook 'prog-mode-hook #'flycheck-mode))


;; tools

;; shell
(use-package shell-pop
  :straight t
  :config
  (setq shell-pop-shell-type
	(quote ("term" "*term*" (lambda nil (term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; git
(use-package magit
  :straight t)

(use-package evil-magit
  :straight t)

;; treemacs
(use-package treemacs
  :straight t)

(use-package treemacs-evil
  :straight t)


;; keybindings

(use-package key-chord
  :straight t
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-mode 1))

(use-package general
  :straight t
  :init

  ;; sanity
  (general-define-key
   :keymaps 'company-active-map
   "RET" 'company-complete-selection)

  ;; space leader
  (general-auto-unbind-keys)
  (general-create-definer leader-def
    :prefix "SPC")
  (general-create-definer local-leader-def
    :prefix "SPC m")

  ;; leader key
  (leader-def '(normal motion)

    ;; critical
    "SPC" 'execute-extended-command
    "qq" 'save-buffers-kill-terminal

    ;; files
    "ff" 'find-file
    "fr" 'counsel-recentf
    "fs" 'save-buffer
    "fd" 'dired
    "fe" '(lambda () (interactive) (find-file user-init-file))

    ;; buffer
    "bb" 'ivy-switch-buffer
    "bB" 'list-buffers
    "bk" 'kill-buffer
    "bd" 'kill-this-buffer
    "be" 'eval-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer

    ;; windows
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "wo" 'other-window
    "ws" 'split-window-below
    "wv" 'split-window-right
    "wd" 'delete-window
    "wD" 'delete-other-windows

    ;; describe
    "hv" 'describe-variable
    "hk" 'describe-key
    "hf" 'describe-function

    ;; open
    "oo" 'make-frame

    ;; shell
    "'" 'shell-pop
    "\"" '(lambda () (interactive) (term "/bin/zsh"))

    ;; magit
    "gg" 'magit-status

    ;; treemacs
    "tt" 'treemacs
    )

  ;; normal state
  (general-define-key
   :states 'normal
   "ESC" 'keyboard-quit
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


;; writing

;; org
(use-package org
  :straight t
  :defer t
  :mode (("\\.org\\'" . org-mode)))

;; latex
(use-package tex-site
  :straight auctex
  :defer t
  :mode (("\\.tex\\'" . latex-mode)))

;; markdown
(use-package markdown-mode
  :straight t
  :defer t
  :mode (("\\.md\\'" . markdown-mode)))
         

;; languages

;; ocaml
(use-package merlin
  :straight t
  :defer t)

(use-package tuareg
  :straight t
  :defer t
  :mode (("\\.ml[ily]?$" . tuareg-mode)
	 ("\\.topml$" . tuareg-mode))
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package utop
  :straight t
  :defer t)

;; coq
(use-package proof-general
  :straight t
  :defer t
  :mode (("\\.v\\'" . coq-mode))
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package company-coq
  :straight t
  :defer t)



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
