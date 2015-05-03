;;; Commentary:

;; None so far.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)


(setq inhibit-splash-screen t)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 172) (height . 60)))

;; window changing keybinding:

(global-set-key (kbd "C-o") 'other-window)

;; minimal UI

(when window-system
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (global-set-key (kbd "s-=") 'text-scale-increase))

;; stop making noise when I scroll

(setq ring-bell-function #'ignore)

;; packages


(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous
                      magit writeroom-mode clj-refactor cider
                      clojure-mode floobits web-mode js2-mode
                      markdown-mode projectile exec-path-from-shell
                      auto-complete evil flycheck-clojure flycheck-pos-tip
                      hideshow haskell-mode))


(package-refresh-contents)


(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case err
        (package-install p)
      (error (message "%s" (error-message-string err))))))


;; keybinding for eshell


(defun create-eshell-in-new-buffer ()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (eshell newbuf)))

(defun create-ansi-term-in-new-buffer ()
  (interactive)
  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (ansi-term "/bin/bash" newbuf)))

(global-set-key (kbd "C-c e") 'create-eshell-in-new-buffer)
(global-set-key (kbd "C-c t") 'create-ansi-term-in-new-buffer)

;; fix path?

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; load midje mode manually

(add-to-list 'load-path "~/.emacs.d/vendor/midje-mode")
(require 'midje-mode)

;; helm and projectile

(add-to-list 'load-path "~/.emacs.d/vendor/async")
(add-to-list 'load-path "~/.emacs.d/vendor/helm")
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq projectile-keymap-prefix (kbd "C-c p"))

(projectile-global-mode)

;; autocomplete

(require 'auto-complete-config)
(global-auto-complete-mode t)
(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

;; cider configuration

(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)

(defun eval-and-insert-sexpr ()
  (interactive)
  (setq current-prefix-arg '(2))
  (insert "\n")
  (call-interactively 'cider-eval-last-sexp)
  (move-to-left-margin)
  (insert ";=> \n"))

(add-hook 'clojure-mode-hook (lambda ()
                               (global-set-key (kbd "C-c i")
                                               'eval-and-insert-sexpr)))

(add-hook 'cider-repl-mode-hook (lambda () (paredit-mode)))

(mapc (lambda (s) (put-clojure-indent s 1))
      '(describe describe-server it before-all after-all before after
                 init-state render render-state will-mount did-mount should-update
                 will-receive-props will-update did-update display-name will-unmount
                 describe-with-db describe-with-server swaggered))

(mapc (lambda (s) (put-clojure-indent s 'defun))
      '(GET* PUT* DELETE* POST* PATCH* context))



;; Highlight long lines


(defvar highlight-long-lines nil)


(defun highlight-long-lines ()
  "Turn on highlighting of long lines."
  (interactive)
  (setq highlight-long-lines t)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-pink))


(defun unhighlight-long-lines ()
  "Turn off highlighting of long lines."
  (interactive)
  (setq highlight-long-lines nil)
  (unhighlight-regexp "^.*\\(?:.\\{81\\}\\).*$"))


(defun toggle-highlight-long-lines ()
  (interactive)
  (if highlight-long-lines
    (unhighlight-long-lines)
    (highlight-long-lines)))


(global-set-key (kbd "C-c h") 'toggle-highlight-long-lines)


;; themes


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)


;; Keep popup dialogues from crashing Emacs, maybe


(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent yes-or-no-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent y-or-n-p from activating a dialog"
  (let ((use-dialog-box nil))
    ad-do-it))


;; clojure-mode customizations


(require 'clj-refactor)


(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-r")
                               (highlight-long-lines)))


;; haskell

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)


;; CC-mode customizations

(setq c-default-style "linux"
      c-basic-offset 4)
;; js2-mode customizations (can't use customize because the above popup blocking code is not working)

(custom-set-variables  
 '(js2-basic-offset 2)  
 '(js2-bounce-indent-p t)) 

(add-hook 'markdown-mode-hook (lambda ()
                                (writeroom-mode)
                                (auto-fill-mode)))


;; web-mode settings

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-markup-offset 2))

(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Start Paredit for lisp modes

(defun lmb-start-paredit ()
  (paredit-mode))

(add-hook 'clojure-mode-hook 'lmb-start-paredit)
(add-hook 'scheme-mode-hook 'lmb-start-paredit)
(add-hook 'emacs-lisp-mode-hook 'lmb-start-paredit)
(add-hook 'lisp-mode-hook 'lmb-start-paredit)

;; auto-mode-alist customizations

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; Open this file with M-x open-dot-emacs

(defun open-dot-emacs ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


;; Org mode extensions

(setq org-babel-clojure-backend 'cider)

(require 'ob-clojure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (clojure . t)
   (plantuml . t)))

(setq org-src-fontify-natively t)

(setq org-list-allow-alphabetical t)

(defun lmb-insert-org-src-block (lang)
  (interactive "sEnter source language: ")
  (insert "#+BEGIN_SRC " lang "\n\n#+END_SRC")
  (move-beginning-of-line 1)
  (forward-line -1))

;; Writing settings


(add-hook 'org-mode-hook (lambda () (writeroom-mode)))
(add-hook 'writeroom-mode-hook (lambda () (auto-fill-mode)))


;; Keybindings

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Open a terminal at startup

(ansi-term)
