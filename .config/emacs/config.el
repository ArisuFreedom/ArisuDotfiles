;; -----------------------------
;; Bootstrap straight.el
;; -----------------------------

;; Define bootstrap version
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  ;; If straight.el is not installed, download and install it
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  ;; Load straight.el bootstrap file
  (load bootstrap-file nil 'nomessage))

;; Integrate straight.el with use-package
(straight-use-package 'use-package)

;; Make use-package use straight.el by default
(setq straight-use-package-by-default t)

;; -----------------------------
;; Default UI cleanup
;; -----------------------------

;; Disable startup screen and messages
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; <<< AQUI: Mensagem customizada no scratch >>>
(setq initial-scratch-message ";; Welcome to GNU Emacs, Arisu!\n;; Enjoy your Session!\n")

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Cursor and current line
(blink-cursor-mode 0)
(global-hl-line-mode 1)

;; Line and column numbers
(column-number-mode 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Disable line numbers in specific modes
(dolist (mode '(term-mode shell-mode eshell-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode 0))))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Smoother scrolling
(setq scroll-conservatively 101)

;; Disable backup and autosave files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(use-package beacon
  :config
  ;; (setq beacon-push-mark 35) ;; empurra o mark quando o ponto se move >35 linhas
  (beacon-mode 1))  ;; ativa globalmente

;; Install Evil and Evil Collection
(straight-use-package 'evil)
(straight-use-package 'evil-collection)

;; --- Evil mode with use-package ---
  (use-package evil
    ;; Variables set before Evil loads
    :init
    (setq evil-want-C-u-scroll t) ;; Enable C-u to scroll up
    (setq evil-undo-system 'undo-redo)
    (setq evil-want-keybinding nil) ;; Required for evil-collection
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)

    ;; Enable Evil globally
    :config
    (evil-mode 1)
    
    ;; evil redo functionallity
    (define-key evil-normal-state-map (kbd "C-r") #'evil-redo)
    (define-key evil-normal-state-map (kbd "U") #'evil-redo)
)
  ;; Optional: Evil Collection provides better bindings for many modes
  (use-package evil-collection
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

(straight-use-package 'general)

(use-package general
  :config
  ;; Make general work nicely with Evil
  (general-evil-setup)

  ;; Define a leader key
  (general-create-definer arisu/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")

  ;; -----------------------------
  ;; Global leader keybindings
  ;; -----------------------------

  (arisu/leader
    "." '(find-file :wk "Find file")
    "f c" '((lambda () (interactive) (find-file "~/.config/emacs/config.org")) :wk "Edit emacs config")
    "TAB TAB" '(comment-line :wk "Comment lines"))

  (arisu/leader
    ;; Buffers
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer"))

  (arisu/leader
    "e" '(:ignore t :wk "Evaluate")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e r" '(eval-region :wk "Evaluate elisp in region")) 

  (arisu/leader
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h v" '(describe-variable :wk "Describe variable")
    "h r r" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "Reload emacs config"))
    ;; "h r r" '(reload-init-file :wk "Reload emacs config"))

  (arisu/leader
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines"))
    )

;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; Set default font to JetBrainsMono Nerd Font Mono
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font Mono"
                    :height 120)

;; italic comments
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)

(set-face-attribute 'font-lock-keyword-face nil
                    :slant 'italic)

;; Ensure new frames also use the same font
(add-to-list 'default-frame-alist
             '(font . "JetBrainsMono Nerd Font Mono-12"))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package kanagawa-themes
  :straight t
  :config
  ;; disable other themes
  (mapc #'disable-theme custom-enabled-themes)
  ;; load theme
  (load-theme 'kanagawa-dragon t))

;; which-key shows popup of possible keybindings after prefix
(use-package which-key
  :straight t
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → ")
  (which-key-mode))  ;; ativa o which-key automaticamente

(use-package sudo-edit
  :config
    (arisu/leader
      "fu" '(sudo-edit-find-file :wk "Sudo find file")
      "fU" '(sudo-edit :wk "Sudo edit file")))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

;; Haskell Mode + LSP

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode)
  ;; associa as extensões .hs e .lhs
  :mode (("\\.hs\\'" . haskell-mode)
         ("\\.lhs\\'" . literate-haskell-mode)))

;; Haskell LSP (Language Server)
;;(use-package lsp-mode
;;  :commands lsp
;;  :hook (haskell-mode . lsp))

;; Optional: integração com interface de completions
;;(use-package company
;;  :hook (haskell-mode . company-mode))

;; Optional: UI para LSP
;;(use-package lsp-ui
;;  :commands lsp-ui-mode)

(use-package toc-org
  :straight t
  :hook (org-mode . toc-org-enable))

;; Org indent
(add-hook 'org-mode-hook #'org-indent-mode)

;; Org visual line
(add-hook 'org-mode-hook 'visual-line-mode)
;; org-bullets
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(electric-indent-mode -1)

(require 'org-tempo)
