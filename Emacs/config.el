(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) ; Enable guiqe-key-mode


(setq ido-separator "\n")
(setq ido-enable-flex-matching t)

(when (display-graphic-p) (load-theme 'solarized-dark t))


(ido-mode 1)
;; Start Emacs server so we can call emacs client
(server-start)

(use-package neotree :ensure t
  :bind ("<f8>" . neotree-toggle))

                                        ; (global-unset-key "C-g")
                                        ; (local-unset-key "M-d")

(use-package multiple-cursors :ensure t :pin melpa-stable
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)))

(message "hi we are so good")

(use-package fold-dwim :ensure t
  :pin melpa-stable
  :bind (("<M-tab>" . fold-dwim-toggle)
         ("C-f" . fold-dwim-hide-all)
         ("H-s" . fold-dwim-show-all)))

(use-package hideshowvis :ensure t
  ;;Puts a plus/minus symbol in the fringe for things you can fold (hide)
  :init (add-hook 'prog-mode-hook 'hideshowvis-minor-mode))

;;; An attempt to make Emacs automatically adjust font size based on resolution
;; (defun fontify-frame (&optional frame)
;;   (interactive)
;;   (let ((target (or frame (window-frame))))
;;     (if window-system
;;         (if (or
;;              (> (frame-pixel-height) 2000)
;;              (> (frame-pixel-width) 2000))
;;             (set-frame-parameter target 'font "Menlo 16")
;;           (set-frame-parameter target 'font "Menlo 15"))
;;       (set-frame-parameter target 'font "Menlo 15")
;;       )))
;; ;;; Fontify current frame for startup
;; (fontify-frame)
;; ;;; Fontify new frames that are created
;; (add-to-list 'after-make-frame-functions 'fontify-frame)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-15")
(set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-12")

;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")
;; (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-11")


;;; Evil stuff
(use-package evil-leader
  :ensure t)

(use-package evil
  :ensure t)

(use-package drag-stuff
  :load-path "/Users/chuan.du/.emacs.d/elpa/drag-stuff-0.1.0/")

(use-package evil-cleverparens
  :ensure t
  ;; :defer 2
  ;; :init
  ;; (add-hook 'after-init-hook 'evil-cleverparens-mode)
  (evil-cleverparens-mode 1))


(use-package evil-visual-mark-mode
  :ensure t
  ;; (progn
  ;;   )
  )

(global-evil-visualstar-mode 1)

(use-package evil-visualstar
  :ensure t)


;; (evil-visual-mark-mode 1)
(evil-mode 1)


(evil-leader/set-leader ",")
(evil-leader/set-key "w" 'save-buffer)
(evil-leader/set-key "q" 'kill-buffer-and-window)

(evil-leader/set-key "h" 'dired-jump)
(evil-leader/set-key "v" 'split-window-right)
(evil-leader/set-key "e" 'pp-eval-last-sexp)
(evil-leader/set-key "," 'other-window)
(evil-leader/set-key "b" 'ibuffer)
(evil-leader/set-key "f" 'projectile-find-file-dwim)
;;; cider
(evil-leader/set-key "n" 'cider-repl-set-ns)
(evil-leader/set-key "r" 'cider-switch-to-repl-buffer)
(evil-leader/set-key "k" 'kill-buffer-and-frame)
(evil-leader/set-key "c" 'cider-load-buffer)

;; (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;; (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;; (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;; (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)
;; (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)

(define-key evil-motion-state-map (kbd "M--") 'evil-window-decrease-height)
(define-key evil-motion-state-map (kbd "M-+") 'evil-window-increase-height)

(define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
(define-key evil-visual-state-map (kbd "SPC") 'er/expand-region)

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; (define-key evil-normal-state-map [escape]'cider-popup-buffer-quit-function)
;; (define-key cider-popup-buffer-mode [escape]'cider-popup-buffer-quit-function)
;; (define-key cider-macroexpansion-mode-map [escape]'cider-popup-buffer-quit-function)
;; (define-key cider-stacktrace-mode-map [escape]'cider-popup-buffer-quit-function)
(define-key cider-test-report-mode-map  [escape]'cider-popup-buffer-quit-function)

;;  undefine key-binding
(defun evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(defun moveForward ()
  (evil-execute-in-emacs-state)
  (paredit-forward))

;;-key evil-normal-state-map (kbd "C-M-f") 'moveForward)
;;-key evil-normal-state-map (kbd "C-M-f") 'moveForward)

(define-key evil-normal-state-map (kbd "M-.") 'evil-undefine)
;; (define-key evil-normal-state-map (kbd "C-M-b") 'evil-undefine)
;; (define-key cider-stacktrace-mode-map (kbd "q") 'evil-undefine)



                                        ; Set cursor colors depending on mode
(when (display-graphic-p)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow)))

(global-evil-leader-mode)
                                        ; (evil-leader-mode)
