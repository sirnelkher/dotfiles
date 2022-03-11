(setq custom-file "~/.emacs-custom")
(load custom-file)

(tool-bar-mode -1)
(tooltip-mode -1)

;; Remove menubar
;;(menu-bar-mode -1)

;; Set up visible bell
(setq visible-bell t)

(require 'package)
;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;			 ("org" . "https://orgmode.org/elpa")
;			 ("elpa" . "https://elpa.gnu.org/packages") ) )
(setq package-archives
      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Init packages on non-Linux platforms
(unless (package-install 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Display line and column numbers
(global-display-line-numbers-mode)
(column-number-mode)

;;Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Log commands in a separate frame
(use-package command-log-mode)

;; Use a nice looking modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(add-hook 'window-setup-hook #'doom-modeline-mode)

;; The above is the default in recent emacsen

(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs")))
(global-set-key (kbd "C-x c") 'dotemacs)

;; Visual line mode everywhere, please
(global-visual-line-mode t)

;; Show the matching parenthesis
(show-paren-mode 1)
;;Color code the braces w/ different colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; Show available keys after 1 sec for C-x and C-c
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-keyidle-delay 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alias nil)) ;;Don't start searches with ^



;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; use calfw
(require 'calfw)

(require 'elpy)
;; Enable Elpy for Python development
;; https://elpy.readthedocs.io/en/latest/
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; Run black on save
(add-hook 'elpy-mode-hook (lambda ()
  (add-hook 'before-save-hook 'elpy-black-fix-code nil t)))

;; Set C-8 to format Python code
(global-set-key (kbd "C-8") 'elpy-black-fix-code)

;; Terraform
;; https://github.com/emacsorphanage/terraform-mode
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y.ml\\'" . yaml-mode))


;; auto-open my usual org files
(find-file "~/.emacs.d/todo.org")
(find-file "~/.emacs.d/notes.org")
