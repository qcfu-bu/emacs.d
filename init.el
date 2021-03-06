;;; package --- Summary
;;; Commentary:
;;; Another Emacs Configuration.
;;;
;;; code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; startup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq-default line-spacing 0)
(setq-default indent-tabs-mode nil)
(customize-set-variable 'make-backup-files nil)
(customize-set-variable 'menu-bar-mode nil)
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'tooltip-mode nil)
(customize-set-variable 'scroll-bar-mode nil)
(customize-set-variable 'horizontal-scroll-bar-mode nil)
(customize-set-variable 'blink-cursor-mode nil)
(save-place-mode 1)
(global-auto-revert-mode t)

(setq straight-check-for-modifications '(check-on-save)
      straight-cache-autoloads t)

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

(use-package esup
  :straight t
  :commands (esup)
  :init
  (setq esup-depth 0))

(use-package restart-emacs
  :straight t
  :defer t)

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

(use-package ivy-prescient
  :straight t
  :defer t)

(use-package counsel
  :straight t
  :config
  (ivy-mode)
  (ivy-prescient-mode)
  (prescient-persist-mode))

(use-package company-prescient
  :straight t
  :defer t)

(use-package company
  :straight t
  :defer t
  :hook (prog-mode . company-mode)
  :init
  (setq company-tooltip-limit 20)
  (setq company-idle-delay 0.3)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package smartparens
  :straight t
  :defer t
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode))
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; shell
(use-package shell-pop
  :straight t
  :defer t
  :init
  (setq shell-pop-shell-type
        (quote ("term" "*terminal-popup*"
                (lambda nil (term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  :config
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;;; git
(use-package magit
  :straight t
  :defer t)

(use-package evil-magit
  :straight t
  :after magit)

;;; treemacs
(use-package treemacs
  :straight t
  :defer t)

(use-package treemacs-evil
  :straight t
  :after treemacs)

;;; projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode 1))

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

;;; checker
(use-package flycheck
  :straight t
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package flyspell
  :straight t
  :defer t
  :hook (text-mode . flyspell-mode))

;;; todo
(use-package hl-todo
  :straight t
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :init
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
  :defer t
  :init
  (load-theme 'doom-one-light t))

(use-package telephone-line
  :straight t
  :config
  (telephone-line-mode 1))

;;; font
(setq default-frame-alist '((font . "Roboto Mono-14")))

;;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package key-chord
  :straight t
  :init
  (setq key-chord-two-keys-delay 0.5)
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

    ;; init file
    "ii" 'init-file

    ;; files
    "ff" 'find-file
    "fr" 'counsel-recentf
    "fs" 'save-buffer
    "fd" 'dired

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
    "wD" 'delete-other-windows

    ;; describe
    "hv" 'describe-variable
    "hk" 'describe-key
    "hf" 'describe-function
    "hm" 'describe-mode
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
   (general-chord "fd") 'evil-normal-state)

  ;; visual state
  (general-define-key
   :states 'visual
   "gc" 'comment-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; prose ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; org
(use-package org
  :straight t
  :defer t
  :mode (("\\.org\\'" . org-mode))
  :hook ((org-mode . org-indent-mode)
         (org-mode . variable-pitch-mode))
  :init
  (setq org-latex-caption-above nil)
  (setq org-highlight-latex-and-related '(latex))
  (setq org-list-allow-alphabetical t)
  (setq org-pretty-entities t))

(use-package evil-org
  :straight t
  :after org)

(use-package org-superstar
  :straight t
  :defer t
  :after org
  :hook (org-mode . org-superstar-mode))

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
(use-package flycheck-ocaml
  :straight t
  :defer t)

(use-package ocp-indent
  :straight t
  :defer t)

(use-package merlin
  :straight t
  :defer t
  :init
  (setq merlin-error-after-save nil)
  :config
  (flycheck-ocaml-setup))

(use-package utop
  :straight t
  :defer t
  :hook (utop-mode . company-mode)
  :init
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package tuareg
  :straight t
  :defer t
  :mode (("\\.ml[lipy]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode))
  :hook ((tuareg-mode . merlin-mode)
         (tuareg-mode . utop-minor-mode)
         (tuareg-mode . ocp-setup-indent)
         (tuareg-mode . ocp-indent-on-save))
  :init
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
    "k" 'utop-kill)
  (defun ocp-indent-on-save ()
    (add-hook 'before-save-hook 'ocp-indent-buffer 0 (merlin-mode))))

;;; coq
(use-package proof-general
  :straight t
  :defer t
  :mode (("\\.v\\'" . coq-mode))
  :hook (coq-mode . company-coq-mode))

(use-package company-coq
  :straight t
  :defer t)

;;; c/c++
(use-package irony
  :straight t
  :defer t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :straight t
  :defer t
  :after company
  :init
  (add-to-list 'company-backends 'company-irony))

(use-package flycheck-irony
  :straight t
  :defer t
  :after flycheck
  :hook (flycheck-mode . flycheck-irony-setup))

;;; java
(use-package meghanada
  :straight t
  :defer t
  :hook ((java-mode . meghanada-mode)
         (before-save . meghanada-code-beautify-before-save))
  :init
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))

;;; ats
(use-package smart-compile
  :straight t
  :defer t
  :hook (ats2-mode . smart-compile-mode))

(load-file "~/.emacs.d/other/ats2-mode.el")

;;; fython
(load-file "~/.emacs.d/other/fython.el")


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
