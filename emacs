(setq custom-file "~/.emacs-custom")
(load custom-file)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

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

(package)

(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai)

;;Init packages on non-Linux platforms
;(unless (package-installed 'use-package)
;  (package-install 'use-package))

;(require 'use-package)
(setq use-package-always-ensure t)
;; Log commands in a separate frame
;(use-package command-log-mode)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen


(defun dotemacs () (interactive) (switch-to-buffer (find-file-noselect "~/.emacs")))
(global-set-key (kbd "C-x c") 'dotemacs)

;; Visual line mode everywhere, please
(global-visual-line-mode t)

;; Display line numbers
(global-display-line-numbers-mode)

;; Show the matching parenthesis
(show-paren-mode 1)

;; use calfw
(require 'calfw)


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
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
