(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key ;; shows matching commands after hitting C-x
  :ensure t
  :config (which-key-mode))

(use-package helm
  :ensure t
  :config
  (require 'helm-config)
  (global-set-key (kbd "M-x") #'helm-M-x)
  (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (helm-mode))

(use-package projectile
  :ensure t
  :config
  (defun maybe-config-esy ()
    (let ((project-root (ignore-errors (projectile-project-root))))
      (message (concat "maybe-config-esy: project-root=" project-root))
      (when (and project-root
                 (file-exists-p (concat project-root "package.json")))
        (message "maybe-config-esy: detected package.json in project-root")
        (let* ((default-directory project-root)
               (env-lines (shell-cmd "esy" "env"))
               (path-str (car (shell-cmd "esy" "sh" "-c" "echo $PATH"))))
          (when env-lines
            (message "maybe-config-esy: setting `process-environment`")
            (setq process-environment env-lines))
          (when path-str
            (message "maybe-config-esy: setting `exec-path`")
            (setq exec-path (parse-colon-path path-str)))))))
  (projectile-global-mode)
  (add-hook 'projectile-after-switch-project-hook 'maybe-config-esy))

(use-package helm-projectile
  :ensure t
  :config
  (require 'helm-projectile)
  (helm-projectile-on))

(use-package helm-ag
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config (projectile-global-mode))

(use-package iedit
  :ensure t)

(use-package auto-complete
  :ensure t
  :config
  (auto-complete-mode)
  (global-set-key (kbd "C-c TAB") 'auto-complete))

(use-package company
  :ensure t
  :config (company-mode))

(use-package tuareg
  :ensure t
  :config
  (defun set-ocaml-error-regexp ()
    (set
     'compilation-error-regexp-alist
     (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
             2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))
  (add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
  (add-hook 'caml-mode-hook 'set-ocaml-error-regexp))

(use-package utop
  :ensure t
  :config
  ;; https://github.com/diml/utop#integration-with-emacs
  (require 'utop)
  (setq utop-command "opam config exec -- rtop -emacs")
  (add-hook 'reason-mode-hook #'utop-minor-mode))

(use-package merlin
  :ensure t
  :config
  (setq merlin-ac-setup t))

(use-package reason-mode
  :ensure t
  :config (add-hook 'reason-mode-hook (lambda ()
                                        (add-hook 'before-save-hook 'refmt-before-save)
                                        (merlin-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Mono"))))
 '(markdown-blockquote-face ((t (:inherit nil))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-inline-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit nil))))
 '(markdown-table-face ((t (:inherit nil)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-use-fuzzy t)
 '(backup-directory-alist (quote (("." . "~/.local/share/emacs/backups"))))
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote (nil "src")))
 '(electric-indent-mode t)
 '(indent-tabs-mode nil)
 '(line-move-visual t)
 '(merlin-command "ocamlmerlin")
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(next-line-add-newlines nil)
 '(package-selected-packages
   (quote
    (projectile which-key try use-package utop tuareg reason-mode merlin helm-projectile company auto-complete)))
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(visible-bell t))

;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(projectile-add-known-project "~/templates/esy-reason-project")
