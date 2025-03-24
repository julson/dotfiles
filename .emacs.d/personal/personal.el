;;; personal.el --- Customizations for Prelude
;;
;;; Commentary:
;;  Requires prelude-modules.el to be set up first
;;  Install the Inconsolata font (https://fonts.google.com/specimen/Inconsolata)

;;; Code:

(prelude-require-packages
 '(solarized-theme robe projectile-rails prettier enh-ruby-mode forge rubocopfmt copilot apheleia corfu))

(load-theme 'solarized-light)
(toggle-scroll-bar -1)

(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
(set-face-attribute 'default t :font "Inconsolata-14")

(require 'robe)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(projectile-rails-global-mode)
(define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)

;; Prevent ruby-mode from inserting encoding info
(setq ruby-insert-encoding-magic-comment nil)

;; Fix js2-mode indentation
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)

;; Allows inf-ruby to search grailed rails console history
(add-hook 'inf-ruby-mode-hook
          (lambda() (let ((p "\\|\\(^Grailed Development *\\)"))
                      (setq inf-ruby-first-prompt-pattern (concat inf-ruby-first-prompt-pattern p))
                      (setq inf-ruby-prompt-pattern (concat inf-ruby-prompt-pattern p)))))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)

(require 'rubocopfmt)
(add-hook 'ruby-mode-hook #'rubocopfmt-mode)
(add-hook 'enh-ruby-mode-hook 'rubocopfmt-mode)

;;(add-hook 'after-init-hook #'global-prettier-mode)

;; Typescript setup
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(flycheck-add-mode 'typescript-tslint 'web-mode)
(setq prelude-format-on-save nil)

(setq typescript-indent-level 2)

;; end Typescript setup

(with-eval-after-load 'magit
  (require 'forge))

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(add-to-list 'copilot-major-mode-alist '("enh-ruby" . "ruby"))
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(add-to-list 'copilot-indentation-alist '(org-mode 2))

(use-package apheleia
  :ensure apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(server-start)
;;; personal.el ends here
