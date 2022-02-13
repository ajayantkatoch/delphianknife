(defvar bootstrap-version) 
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
     user-emacs-directory))
       (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (let ((install.el
	     "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"))
	(with-current-buffer
	    (url-retrieve-synchronously install.el 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp))))
    (load bootstrap-file nil 'nomessage))

(setq-default straight-use-package-by-default t) 
(straight-use-package 'use-package)
(setq-default use-package-always-defer t)

(setq-default
 cursor-in-non-selected-windows t                 ; Hide the cursor in inactive windows
 help-window-select t                             ; Focus new help windows when opened
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 load-prefer-newer t                              ; Prefer the newest version of a file
 scroll-conservatively most-positive-fixnum       ; Always scroll by one line
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 use-package-always-ensure t                      ; Avoid the :ensure keyword for each package
 user-full-name "Ajayant Katoch"                  ; Set the full name of the current user
 user-mail-address "ajayant@outlook.com")         ; Set the email address of the current user
(column-number-mode 1)                            ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode)                             ; Hightlight current line
(global-linum-mode 1)                             ; Display line number

(use-package kaolin-themes
  :straight kaolin-themes
  :init
  (load-theme 'kaolin-bubblegum t))

(use-package doom-modeline
 :hook (after-init . doom-modeline-mode))

(when window-system
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8)
  (prefer-coding-system 'utf-8)
(when (display-graphic-p)
   (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package css-mode
  :straight nil
  :custom
  (css-indent-offset 2))

(use-package sgml-mode
  :straight nil
  :hook
  (html-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
  (html-mode . sgml-electric-tag-pair-mode)
  (html-mode . sgml-name-8bit-mode)
  :custom
  (sgml-basic-offset 2))

(use-package js2-mode
  :straight nil
  :mode (rx ".js" eos)
  :custom
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (js2-highlight-level 3)
  (js2-idle-timer-delay 0)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package rjsx-mode
  :mode (rx (or ".jsx" (and "components/" (* anything) ".js")) eos)
  :hook
  (rjsx-mode . (lambda () (setq me/pretty-print-function #'sgml-pretty-print)))
  (rjsx-mode . hydra-plus-set-super)
  (rjsx-mode . sgml-electric-tag-pair-mode))

(use-package typescript-mode
  :init
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . typescript-tsx-mode))
  :config
  (add-hook 'typescript-tsx-mode-hook #'sgml-electric-tag-pair-mode)
  :custom
  (typescript-indent-level 2))

(use-package json-mode
  :mode (rx ".json" eos))

(use-package org
  :straight nil
  :bind
  (:map org-mode-map
   ("C-<return>" . nil)
   ("C-<tab>" . me/org-cycle-parent))
:custom
(org-adapt-indentation nil)
(org-cycle-separator-lines 0)
(org-descriptive-links nil)
(org-edit-src-content-indentation 0)
(org-edit-src-persistent-message nil)
(org-fontify-done-headline t)
(org-fontify-quote-and-verse-blocks t)
(org-fontify-whole-heading-line t)
(org-return-follows-link t)
(org-src-preserve-indentation t)
(org-src-tab-acts-natively t)
(org-src-window-setup 'current-window)
(org-startup-truncated nil)
(org-support-shift-select 'always))

(defun me/org-cycle-parent (argument)
  "Go to the nearest parent heading and execute `org-cycle'."
  (interactive "p")
  (if (org-at-heading-p)
      (outline-up-heading argument)
    (org-previous-visible-heading argument))
  (org-cycle))

(use-package org-bullets
:hook ((org-mode) . org-bullets-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :config (unless (find-font (font-spec :name "all-the-icons"))
            (all-the-icons-install-fonts t)))

(use-package flycheck
  :init
  (global-flycheck-mode))
(use-package flycheck-tip
  :bind
  (("C-c C-n" . error-tip-cycle-dwim)
   ("C-c C-p" . error-tip-cycle-dwim-reverse)) )

(use-package which-key
  :custom
  (which-key-mode t))

(use-package autorevert
  :straight nil
  :bind ("C-x R" . revert-buffer)
  :custom (auto-revert-verbose nil)
  :config (global-auto-revert-mode))

(use-package consult
:bind
([remap goto-line] . consult-goto-line)
([remap isearch-forward] . consult-line)
([remap switch-to-buffer] . consult-buffer)
("C-h M" . consult-minor-mode-menu)
:custom
(consult-line-start-from-top t)
(consult-project-root-function #'me/project-root)
(xref-show-definitions-function #'consult-xref)
(xref-show-xrefs-function #'consult-xref))

(use-package corfu
  :hook
  (after-init . corfu-global-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay .5))

(use-package marginalia
  :hook
  (after-init . marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (orderless-component-separator 'orderless-escapable-split-on-space))

(use-package selectrum
  :custom
  (selectrum-resize nil)
  :hook
  (after-init . selectrum-mode))
