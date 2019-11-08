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

(message "SuperUser Loading")

(defconst SuperUser-packages '(evil-unimpaired
                               (evil-helper :location local)
                               ;;(cider-helper :location local)
                               evil-cleverparens
                               clojure
                               cider
                               auto-indent-mode
                               paredit))
;; (setq SuperUser-packages '(auto-indent-mode))
;; TODO pretty symbol mode set up

(defun SuperUser/post-init-paredit ()
  (message "Loading: Post init paredit mode")
  ;; (let ((paredit-setup (lambda () (paredit-mode 1))))
  ;;   (add-hook 'lisp-mode-hook paredit-setup)
  ;;   (add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode 1)))
  ;;   (add-hook 'clojure-mode-hook paredit-setup)
  ;;   (add-hook 'elisp-mode-hook (lambda () (paredit-mode 1)))
  ;;   (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  ;;   (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1))))
  )

(defun SuperUser/init-auto-indent-mode ()
  (use-package auto-indent-mode
    :ensure t
    :init (progn (message "Loading: init auto-ident-mode")
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
    :init (progn (message "Laoding: init evil-helper"))))

(defun SuperUser/post-init-evil-cleverparens ()
  (message "Loading: Post init eveil clever parens")
  (evil-cleverparens-mode 1)
  ;; (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (evil-leader/set-key "tP" 'evil-cleverparens-mode)
  (let ((mode-hooker (lambda () (evil-cleverparens-mode 1))))
    (add-hook 'lisp-mode-hook mode-hooker)
    ;; (add-hook 'lisp-interaction-mode-hook mode-hooker)
    (add-hook 'clojure-mode-hook mode-hooker)
    (add-hook 'emacs-lisp-mode-hook mode-hooker)
    (add-hook 'cider-repl-mode-hook mode-hooker)))

(defun SuperUser/post-init-evil-unimpaired ()
  ;; Key binding
  (message "Loading: Post Evil key binding")

  (evil-leader/set-key "sn" 'helm-yas-visit-snippet-file)

  ;; (global-set-key (kbd "M-`") 'other-frame-or-window)

  (define-key evil-normal-state-map "gn" 'find-next-full-wordds)
  (define-key evil-normal-state-map "gp" 'find-prev-full-wordds)

  (define-key evil-normal-state-map "gh" 'evil-window-left)
  (define-key evil-normal-state-map "gl" 'evil-window-right)
  (define-key evil-normal-state-map "gj" 'evil-window-down)
  (define-key evil-normal-state-map "gk" 'evil-window-up)

  (define-key evil-normal-state-map (kbd "C-c C-o") 'ace-window)
  (define-key evil-normal-state-map (kbd "C-c o") 'ace-window)

  (define-key evil-motion-state-map (kbd "M-+") 'enlarge-window)
  (define-key evil-motion-state-map (kbd "M-=") 'shrink-window)
  (define-key evil-motion-state-map (kbd "M--") 'evil-window-increase-width)
  (define-key evil-motion-state-map (kbd "M-_") 'evil-window-decrease-width)

  ;; (define-key evil-normal-state-map (kbd "C-b") 'scroll-down-command)
  ;; (define-key evil-normal-state-map (kbd "C-f") 'scroll-up-command)

  ;; (define-key evil-normal-state-map (kbd "M-b") 'paredit-backward)
  ;; (define-key evil-normal-state-map (kbd "M-f") 'paredit-forward)

  (define-key evil-normal-state-map (kbd "M-;") 'block-toggle-comments)
  (define-key evil-normal-state-map (kbd "C-M-;") 'add-block-toggle-comments)
  ;; (define-key evil-normal-state-map (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map (kbd "M-/") 'spacemacs/comment-or-uncomment-lines)


  ;; (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
  ;; (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "RET") 'evil-toggle-fold)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-toggle-fold)
  ;; (define-key evil-visual-state-map (kbd "RET") 'er/expand-region)

  ;; Configuration
  (message "Loading: Post Evil configuration")
  (setq evil-move-cursor-back nil)

  ;; Other
  ;; (when (display-graphic-p)
  ;;   (setq evil-emacs-state-cursor '("red" box))
  ;;   (setq evil-normal-state-cursor '("green" box))
  ;;   (setq evil-visual-state-cursor '("orange" box))
  ;;   (setq evil-insert-state-cursor '("red" bar))
  ;;   ;; (setq evil-replace-state-cursor '("red" bar))
  ;;   (setq evil-operator-state-cursor '("red" hollow)))

  ;;  undefine key-binding
  ;; (defun evil-undefine ()
  ;;   (interactive)
  ;;   (let (evil-mode-map-alist)
  ;;     (call-interactively (key-binding (this-command-keys)))))
  ;; (define-key evil-normal-state-map (kbd "C-a") 'evil-undefine)
  ;; (define-key evil-normal-state-map (kbd "C-e") 'evil-undefine)
  ;; (define-key evil-normal-state-map (kbd "M-.") 'evil-undefine)

  )

(defun SuperUser/post-init-cider ()
  ;; Not working need to reload
  (message "Loading: Post init cider")
  ;; (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "sn" 'cider-repl-set-ns)
  (spacemacs/set-leader-keys-for-major-mode 'cider-repl-mode
    "sh" 'cider-repl-previous-matching-input
    "sn" 'cider-repl-set-ns)
  (message "Loading: Remapping keys for clojure-mode")
  (evil-leader/set-key-for-mode 'clojure-mode
    "sd" 'cider-find-dwim
    "et" 'cider-pprint-eval-defun-at-point
    "cc" 'cider-load-buffer))

;; (defun SuperUser/post-init-cider-helper ()
;;   (message "Loading: post init cider")

;;   (progn ;; cider
;;     ;; (evil-leader/set-key "g" 'cider-repl-previous-matching-input)
;;     ;; (evil-leader/set-key "t" 'cider-pprint-eval-defun-at-point)
;;     ;; (evil-leader/set-key "q" 'kill-buffer-and-window)
;;     ;; (evil-leader/set-key "e" 'cider-eval-last-sexp)
;;     ;; (evil-leader/set-key "n" 'cider-repl-set-ns)

;;     ;; (evil-leader/set-key-for-mode 'clojure-mode "cc" 'cider-load-buffer)
;;     ;; (evil-leader/set-key-for-mode 'cider-repl-mode "cc" 'cider-repl-clear-buffer)
;;     ))

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

(defun add-block-toggle-comments ()
  "If region is set, [un]comments it. Otherwise [un]comments current block.
   - Works for Clojure and ClojureScript"
  (interactive)
  (progn
    (evil-first-non-blank)
    (if (and (or (equal major-mode 'clojure-mode)
                 (equal major-mode 'clojurescript-mode))
             (not (string= (string (char-after (point))) ";")))
        (insert "#_")
      (spacemacs/comment-or-uncomment-lines 1))))

(defun block-toggle-comments ()
  "If region is set, [un]comments it. Otherwise [un]comments current block.
   - Works for Clojure and ClojureScript"
  (interactive)
  (progn
    (evil-first-non-blank)
    (if (and (or (equal major-mode 'clojure-mode)
                 (equal major-mode 'clojurescript-mode))
             (not (string= (string (char-after (point))) ";")))
        (if (string= (string (char-after (point))) "#")
            (delete-char 2)
          (insert "#_"))
      (spacemacs/comment-or-uncomment-lines 1)))
  ;; Forget why active do here
  ;; (if (eq mark-active nil)
  ;;     (progn
  ;;       (evil-first-non-blank)
  ;;       (if (and (or (equal major-mode 'clojure-mode)
  ;;                    (equal major-mode 'clojurescript-mode))
  ;;                (not (string= (string (char-after (point))) ";")))
  ;;           (if (string= (string (char-after (point))) "#")
  ;;               (delete-char 2)
  ;;             (insert "#_"))
  ;;         (spacemacs/comment-or-uncomment-lines 1)))
  ;;   (comment-dwim nil))
  ;; (deactivate-mark)
  )

(defun set-face-to-mini ()
  (interactive)
  (set-face-attribute 'default nil :font "Menlo 11")
  (set-face-attribute 'mode-line nil :font "Menlo 9")
  (balance-windows))
(defun set-face-to-small ()
  (interactive)
  (set-face-attribute 'default nil :font "Menlo 12")
  (set-face-attribute 'mode-line nil :font "Menlo 11")
  (balance-windows))
(defun set-face-to-medium ()
  (interactive)
  (set-face-attribute 'default nil :font "Menlo 14")
  (set-face-attribute 'mode-line nil :font "Menlo 12")
  (balance-windows))
(defun set-face-to-large ()
  (interactive)
  (set-face-attribute 'default nil :font "Menlo 16")
  (set-face-attribute 'mode-line nil :font "Menlo 14")

  ;; (set-face-attribute 'default nil :font "DejaVu Sans Mono 14")
  ;; (set-face-attribute 'default nil :font "Menlo 10")
  ;; (set-face-attribute 'default nil :font "Monaco 13")
  ;; (set-face-attribute 'default nil :font "Monospace 14")
  ;; (set-face-attribute 'default nil :font "Consolas 14")
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
;; NOTE
;; M, j i Jump definition in file, heml-jump-in-buffer
;; Cider
;; 'cider-repl-previous-matching-input)

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
