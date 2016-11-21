;;; packages.el --- SuperUser layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Yongqinchuan Du <chuan.du@Chuans-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `SuperUser-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `SuperUser/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `SuperUser/pre-init-PACKAGE' and/or
;;   `SuperUser/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

;; (setq SuperUser-packages '((evil-helper :location local)
;;                            evil-unimpaired))

(message "SuperUser Loading")



(defconst SuperUser-packages '(evil-unimpaired
                               (evil-helper :location local)
                               evil-cleverparens
                               auto-indent-mode
                               ;; whitespace
                               ;; (cider-helper :location local)
                               ))
;; (setq SuperUser-packages '(auto-indent-mode))

(defun SuperUser/init-auto-indent-mode ()
  (use-package auto-indent-mode
    :ensure t
    :init (progn (message "init auto-ident-mode")
                 (setq auto-indent-on-save-file t
                       auto-indent-blank-lines-on-move nil
                       auto-indent-delete-trailing-whitespace-on-save-file t
                       auto-indent-untabify-on-save-file t
                       auto-indent-indent-style 'aggressive))
    :config (auto-indent-global-mode)))

(defun SuperUser/init-evil-helper ()
  (use-package evil-helper
    :defer t
    :commands (find-next-full-wordds
               find-prev-full-wordds)
    :init (progn (message "init evil-helper"))))

(defun SuperUser/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :ensure t
    :config (evil-cleverparens-mode 1)))

;; (use-package whitespace :ensure t
;;   :init (setq whitespace-style '(face trailing lines-tail tabs)
;;               whitespace-line-column 80))

(defun SuperUser/post-init-evil-unimpaired ()

  ;; Key binding
  (message "Loading: Post Evil key binding")
  (define-key evil-normal-state-map "gn" 'find-next-full-wordds)
  (define-key evil-normal-state-map "gp" 'find-prev-full-wordds)
  ;; evil-search-highlight-persist

  (define-key evil-normal-state-map "gh" 'evil-window-left)
  (define-key evil-normal-state-map "gl" 'evil-window-right)
  (define-key evil-normal-state-map "gj" 'evil-window-down)
  (define-key evil-normal-state-map "gk" 'evil-window-up)

  (define-key evil-normal-state-map (kbd "C-c C-o") 'ace-window)
  (define-key evil-normal-state-map (kbd "C-c o") 'ace-window)

  ;; (define-key evil-motion-state-map (kbd "s-+") 'enlarge-window)
  ;; (define-key evil-motion-state-map (kbd "s-=") 'shrink-window)
  ;; (define-key evil-motion-state-map (kbd "s--") 'evil-window-increase-width)
  ;; (define-key evil-motion-state-map (kbd "s-_") 'evil-window-decrease-width)

  (define-key evil-motion-state-map "gs" 'ace-swap-window)

  ;; (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  ;; (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)

  ;; (define-key evil-normal-state-map (kbd "C-b") 'scroll-down-command)
  ;; (define-key evil-normal-state-map (kbd "C-f") 'scroll-up-command)

  ;; (define-key evil-normal-state-map (kbd "M-b") 'paredit-backward)
  ;; (define-key evil-normal-state-map (kbd "M-f") 'paredit-forward)

  (define-key evil-normal-state-map (kbd "M-;") 'block-toggle-comments)
  ;; (define-key evil-normal-state-map (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "M-/") 'comment-line)

  ;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

  ;; (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
  ;; (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "RET") 'evil-toggle-fold)
  ;; (define-key evil-visual-state-map (kbd "RET") 'er/expand-region)

  ;; Configuration
  (message "Loading: Post Evil configuration")
  (setq evil-move-cursor-back nil)

  ;; Other
  (when (display-graphic-p)
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow)))

  ;;  undefine key-binding
  ;; (defun evil-undefine ()
  ;;   (interactive)
  ;;   (let (evil-mode-map-alist)
  ;;     (call-interactively (key-binding (this-command-keys)))))
  ;; (define-key evil-normal-state-map (kbd "C-a") 'evil-undefine)
  ;; (define-key evil-normal-state-map (kbd "C-e") 'evil-undefine)
  ;; (define-key evil-normal-state-map (kbd "M-.") 'evil-undefine)

  )

(defun SuperUser/init-cider ()
  (use-package cider
    :config
    (progn
      (evil-leader/set-key-for-mode 'clojure-mode
        "ml" 'zoo/cider-switch-and-load
        "m," 'zoo/cider-load-and-test
        "mi" 'cider-inspect
        "mq" 'cider-quit
        "msc" 'cider-connect)
      (setq cider-repl-pop-to-buffer-on-connect t)
      (add-hook 'cider-repl-mode-hook 'zoo/clojure-after-hook))))

;; Other helper fucntion
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun block-toggle-comments ()
  "If region is set, [un]comments it. Otherwise [un]comments current block.
   - Works for Clojure and ClojureScript"
  (interactive)
  (if (eq mark-active nil)
      (progn
        (evil-first-non-blank)
        ;; (move-beginning-of-line nil)
        (if (or (equal major-mode 'clojure-mode)
                (equal major-mode 'clojurescript-mode))
            (if (string= (string (char-after (point))) "#")
                (delete-char 2)
              (insert "#_"))
          (comment-dwim nil)))
    (comment-dwim nil))
  (deactivate-mark))

(defun set-face-to-small ()
  (interactive)
  (set-face-attribute 'default nil :font "Consolas 10")
  (set-face-attribute 'mode-line nil :font "Consolas 12")
  (balance-windows))
(defun set-face-to-medium ()
  (interactive)
  (set-face-attribute 'default nil :font "Consolas 14")
  (set-face-attribute 'mode-line nil :font "Consolas 12")
  (balance-windows))
(defun set-face-to-large ()
  (interactive)
  (set-face-attribute 'default nil :font "Consolas 16")
  (set-face-attribute 'mode-line nil :font "Consolas 14")

  ;; (set-face-attribute 'default nil :font "DejaVu Sans Mono 14")
  ;; (set-face-attribute 'default nil :font "Menlo 10")
  ;; (set-face-attribute 'default nil :font "Monaco 13")
  ;; (set-face-attribute 'default nil :font "Monospace 14")
  ;; (set-face-attribute 'default nil :font "Courier 14")

  (balance-windows))

(defun other-frame-or-window ()
  (interactive)
  (let ((frame-count (length (frame-list))))
    (if (= 1 frame-count)
        (other-window 1)
      (other-frame 1))))

;; https://github.com/TheBB/spacemacs-layers
;;; packages.el ends here

(message "SuperUser Loading End")

;; Scratch
;; (add-hook 'clojurescript-mode-hook
;;           (lambda ()
;;             ;; Preferred comment style
;;             (message "clojure script hook called")
;;             (setq comment-start "#_"
;;                   comment-end "")))

;;; esc quits
;;   (defun minibuffer-keyboard-quit ()
;;     "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;     (interactive)
;;     (if (and delete-selection-mode transient-mark-mode mark-active)
;;         (setq deactivate-mark  t)
;;       (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;       (abort-recursive-edit)))

;;   (define-key evil-normal-state-map [escape] 'keyboard-quit)
;;   (define-key evil-visual-state-map [escape] 'keyboard-quit)
;;   (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
;;   (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;   ;; (define-key evil-normal-state-map [escape]'cider-popup-buffer-quit-function)
;;   ;; (define-key cider-popup-buffer-mode [escape]'cider-popup-buffer-quit-function)
;;   ;; (define-key cider-macroexpansion-mode-map [escape]'cider-popup-buffer-quit-function)
;;   ;; (define-key cider-stacktrace-mode-map [escape]'cider-popup-buffer-quit-function)
;;   (define-key cider-test-report-mode-map  [escape]'cider-popup-buffer-quit-function)

;;   (global-set-key [escape] 'evil-exit-emacs-state)
