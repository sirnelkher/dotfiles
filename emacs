(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode t)

;; Set up visible bell
(setq visible-bell t)


;; Init package sources --------------------------------------------------------

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/") ) )
;; Did not work on my work PC, so used a github mirror
;;(setq package-archives
;;     '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;;        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;;        ("elpa"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(use-package "monokai-theme"
  :defer 1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; ESC also quits, not just C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(hl-line-mode)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (hl-line-mode 0))))


;;Init packages on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


;; Display line numbers
(global-display-line-numbers-mode)
(column-number-mode)

;;Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file))

;; Log commands in a separate window
(use-package command-log-mode
  :defer 1
  :config
   (global-command-log-mode t))

;; Use a nice looking modeline
(use-package doom-modeline)
(doom-modeline-mode t)
(add-hook 'window-setup-hook #'doom-modeline-mode)

;; Show icons when we have GUI
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t))
  :defer 1)

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mod . all-the-icons-dired-mode))

(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs")))
(global-set-key (kbd "C-x c") 'dotemacs)

;; Visual line mode everywhere, please
(global-visual-line-mode t)

;; Show the matching parenthesis
(show-paren-mode 1)
;;Color code the braces w/ different colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Show available keys after 0.3 sec for C-x and C-c
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-keyidle-delay 0.3)
  :defer 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  :defer 1)

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alias nil)) ;;Don't start searches with ^
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :defer t)

(use-package general
  :config
  (general-create-definer gcsordas/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")


  (gcsordas/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme"))
  :defer t)

(use-package hydra
  :defer t)
;(use-package ace-window)

;;(global-set-key
;; (kbd "C-M-o")
;; (defhydra hydra-window (:timeout 5)
;;   "window"
;;   ("h" windmove-left)
;;   ("j" windmove-down)
;;   ("k" windmove-up)
;;   ("l" windmove-right)
;;   ("v" (lambda ()
;; 	 (interactive)
;; 	 (split-window-right)
;; 	 (windmove-right))
;;    "vert")
;;   ("x" (lambda ()
;; 	 (interactive)
;; 	 (split-window-below)
;; 	 (windmove-down))
;;    "horiz")
;;   ("t" transpose-frame "'")
;;   ("o" delete-other-windows "one" :color blue)
;;   ("a" ace-window "ace")
;;   ("s" ace-swap-window "swap")
;;   ("d" ace-delete-window "del")
;;   ("i" ace-maximize-window "ace-one" :color blue)
;;   ("b" ido-switch-buffer "buf")
;;   ("q" nil "cancel")))

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(gcsordas/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :defer t)

(use-package lsp-ivy
  :after lsp-mode
  :defer t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired)
  :defer t)

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
    :commands magit-status
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    :defer t)

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;;(use-package forge
;;  :after magit)


;;Auto-completion
;(use-package auto-complete
;  :ensure t
;  :init
;  (progn
;    (ac-config-default)
;    (global-auto-complete-mode t))
;  :defer t
;  :config
;  (global-company-mode 1))



;; Org mode configuration
(use-package org
  :config
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq otg-log-into-drawer t)
  (setq org-agenda-files '(("~/Documents/emacs/agenda.org"))))

(use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

    ;; Make Org mode work with files ending in .org
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/emacs/orgroam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode)
  :defer t)

;; use calfw
(use-package calfw
  :defer t)



;; Shell script formatting
(use-package shfmt
  :ensure t
  :load-path "lisp/shfmt"
;;  :ensure-system-package shfmt
  :hook (sh-mode . shfmt-on-save-mode)
  :defer t)

;;(use-package flycheck-shfmt
;;  :ensure t
;;  :after flycheck
;;  :load-path "lisp/shfmt"
;;  :config
;;  (flycheck-shfmt-setup))


(use-package elpy
  :defer 1
  :config
;; Enable Elpy for Python development
;; https://elpy.readthedocs.io/en/latest/
  (setq elpy-rpc-python-command "python3")
  (elpy-enable)
  :defer t)

;; Run black on save
(add-hook 'elpy-mode-hook (lambda () (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))

;; Set C-8 to format Python code
(global-set-key (kbd "C-8") 'elpy-black-fix-code)

;; Terraform
;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; YAML
(use-package yaml-mode
  :defer t)
(add-to-list 'auto-mode-alist '("\\.y.ml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;(global-set-key (kbd "C-M-<left>") 'windmove-left)
;;(global-set-key (kbd "C-M-<right>") 'windmove-right)
;;(global-set-key (kbd "C-M-<up>") 'windmove-up)
;;(global-set-key (kbd "C-M-<down>") 'windmove-down)
;;(global-set-key (kbd "C-S-M-<right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "C-S-M-<left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "C-S-M-<up>") 'shrink-window)
;;(global-set-key (kbd "C-S-M-<down>") 'enlarge-window)

(find-file "~/Documents/emacs/notes.org")
(find-file "~/Documents/emacs/todo.org")
;;(find-file "~/Documents/emacs/diary/diary.org")
(defvar journal-dir "~/Documents/emacs/journal")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(sticky ansible ansible-doc ansible-vault company-ansible poly-ansible mines minesweeper 2048-game org-ac org-agenda-property org-alert org-roam-timestamps org-roam-ui org-roam treemacs treemacs-all-the-icons treemacs-evil treemacs-icons-dired treemacs-magit treemacs-projectile treemacs-tab-bar lsp-mode python-environment py-autopep8 yaml-mode which-key use-package terraform-mode terraform-doc shfmt rainbow-delimiters org-bullets monokai-theme markdownfmt markdown-toc magit ivy-rich hydra helpful general flycheck evil-collection elpy doom-modeline dir-treeview diminish-buffer diminish counsel-projectile command-log-mode calfw auto-complete all-the-icons-dired ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
