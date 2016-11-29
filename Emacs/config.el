(message "Hi, Start loading customer config")

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t)

(use-package bind-key
  :ensure t
  :bind ())

(global-set-key (kbd "M-`") 'other-frame-or-window)
(setq ido-separator "\n")
(setq ido-enable-flex-matching t)

(ido-mode 1)
(setq ns-pop-up-frames nil)
(setq truncate-lines -1)

;;; (when (display-graphic-p) (load-theme 'solarized-dark t))

;; Start Emacs server so we can call emacs client
;; (server-start)

(defun other-frame-or-window ()
  (interactive)
  (let ((frame-count (length (frame-list))))
    (if (= 1 frame-count)
        (other-window 1)
      (other-frame 1))))

(use-package smooth-scroll
  :ensure t
  :config (progn
            (smooth-scroll-mode 1)
            (setq scroll-margin 50
                  scroll-conservatively 9999
                  auto-window-vscroll nil
                  scroll-step 1)))

(use-package neotree :ensure t
  :bind ("<f8>" . neotree-toggle))

(use-package multiple-cursors :ensure t :pin melpa-stable
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e" . mc/edit-ends-of-lines)))

(message "Hi we are so good so far")

(use-package fold-dwim :ensure t
  :pin melpa-stable
  :bind (("<M-tab>" . fold-dwim-toggle)
         ("C-f" . fold-dwim-hide-all)
         ("H-s" . fold-dwim-show-all)))

(use-package hideshowvis :ensure t
  ;;Puts a plus/minus symbol in the fringe for things you can fold (hide)
  :init (add-hook 'prog-mode-hook 'hideshowvis-minor-mode))

;; (use-package drag-stuff
;;   :load-path "/Users/chuan.du/.emacs.d/elpa/drag-stuff-0.1.0/")


;; Not working yet
;;Exit insert mode by pressing j and then j quickly
;; (use-package key-chord
;;   :ensure t
;;   :config (progn (setq key-chord-two-keys-delay 0.3)
;;                  (key-chord-mode 1)))

;; (use-package minimap
;;   :ensure t
;;   :config (progn
;;             (minimap-mode 0)))

;; edit-server allow emacs to edit stuff form browser
;; (use-package edit-server
;;   :ensure t
;;   :config (edit-server-start))

;;; Evil stuff
(use-package evil
  :ensure t
  :config (evil-mode 1))


(evil-define-motion find-next-full-wordds (count &optional symbol)
  "Go to first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t)))
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (dotimes (var (or count 1))
      (evil-search-word t nil string))))

(evil-define-motion find-prev-full-wordds (count &optional symbol)
  ;; TODO
  "Go to first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t)))
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (dotimes (var (or count 1))
      (evil-search-word nil nil string))))

(defun cider-switch-to-repl-buffer-clojure-buffer ()
  (interactive)
  (if (equal major-mode 'cider-repl-mode)
      (cider-switch-to-last-clojure-buffer)
    (cider-switch-to-repl-buffer)))

(progn
  (define-key evil-motion-state-map "gn" 'find-next-full-wordds)
  (define-key evil-motion-state-map "gp" 'find-prev-full-wordds)

  (define-key evil-motion-state-map "gl" 'evil-window-right)
  (define-key evil-motion-state-map "gj" 'evil-window-down)
  (define-key evil-motion-state-map "gk" 'evil-window-up)
  (define-key evil-motion-state-map "gh" 'evil-window-left)

  (define-key evil-motion-state-map "gs" 'ace-swap-window)

  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

  ;; (define-key evil-normal-state-map (kbd "C-b") 'evil-scroll-up)
  ;; (define-key evil-normal-state-map (kbd "C-f") 'evil-scroll-down)

  (define-key evil-normal-state-map (kbd "C-b") 'scroll-down-command)
  (define-key evil-normal-state-map (kbd "C-f") 'scroll-up-command)

  (define-key evil-normal-state-map (kbd "M-b") 'paredit-backward)
  (define-key evil-normal-state-map (kbd "M-f") 'paredit-forward)

  (define-key evil-normal-state-map (kbd "M-;") 'block-toggle-comments)
  (define-key evil-normal-state-map (kbd "M-/") 'evilnc-comment-or-uncomment-lines)
  ;; (add-hook 'clojurescript-mode-hook
  ;;           (lambda ()
  ;;             ;; Preferred comment style
  ;;             (message "clojure script hook called")
  ;;             (setq comment-start "#_"
  ;;                   comment-end "")))

  (define-key evil-normal-state-map (kbd "C-c C-o") 'ace-window)
  (define-key evil-normal-state-map (kbd "C-c o") 'ace-window)

  (define-key evil-motion-state-map (kbd "M-=") 'enlarge-window)
  (define-key evil-motion-state-map (kbd "M--") 'shrink-window)
  (define-key evil-motion-state-map (kbd "M-+") 'evil-window-increase-width)
  (define-key evil-motion-state-map (kbd "M-_") 'evil-window-decrease-width)

  (define-key evil-normal-state-map (kbd "SPC") 'evil-toggle-fold)
  (define-key evil-visual-state-map (kbd "SPC") 'er/expand-region)

;;; esc quits
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

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

  (global-set-key [escape] 'evil-exit-emacs-state)

  (setq evil-move-cursor-back nil)
  ;;  undefine key-binding
  (defun evil-undefine ()
    (interactive)
    (let (evil-mode-map-alist)
      (call-interactively (key-binding (this-command-keys)))))

  ;;-key evil-normal-state-map (kbd "C-M-f") 'moveForward)
  ;;-key evil-normal-state-map (kbd "C-M-f") 'moveForward)

  (define-key evil-normal-state-map (kbd "C-a") 'evil-undefine)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-undefine)
  (define-key evil-normal-state-map (kbd "M-.") 'evil-undefine)

;;; Set cursor colors depending on mode
  (when (display-graphic-p)
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))))

;; (put-clojure-indent 'partial 5)

;; Tip M+: Eval a Expression
;; (equal major-mode 'cider-repl-mode)

(use-package evil-leader
  :ensure t
  :config (progn
            (evil-leader/set-leader ",")

            (evil-leader/set-key "w" 'save-buffer)
            (evil-leader/set-key "u" 'dired-jump)
            (evil-leader/set-key "," 'evil-switch-to-windows-last-buffer)
            ;; (evil-leader/set-key "h" 'other-window)
            (evil-leader/set-key "m" 'minimap-mode)
            (evil-leader/set-key "b" 'ido-switch-buffer)
            (evil-leader/set-key "f" 'projectile-find-file-dwim)
            (evil-leader/set-key "r" 'ido-imenu)
            (evil-leader/set-key "g" 'cider-repl-previous-matching-input)
            (evil-leader/set-key-for-mode 'text-mode "k" 'edit-server-done) ;; connection for edit-server on brower
            ;; (evil-leader/set-key "d" 'edit-server-done)
            ;; (evil-leader/set-key "x" 'kill-this-buffer)

            ;; Ace-jump mode
            ;; toggle-case-fold-search
            (evil-leader/set-key "o" 'evil-ace-jump-line-mode)
            (evil-leader/set-key "j" 'evil-ace-jump-word-mode)
            (evil-leader/set-key "s" 'ag-project)

            ;; (evil-leader/set-key
            ;;   "ci" 'evilnc-comment-or-uncomment-lines
            ;;   "cl" 'evilnc-quick-comment-or-uncomment-to-the-line)


            ;; Buffer and windwow
            (evil-leader/set-key "h" 'cider-popup-buffer-quit-function)
            (evil-leader/set-key "k" 'kill-this-buffer)
            (evil-leader/set-key "H" 'evil-window-move-far-left)
            (evil-leader/set-key "L" 'evil-window-move-far-right)
            (evil-leader/set-key "J" 'evil-window-move-very-bottom)
            (evil-leader/set-key "K" 'evil-window-move-very-top)


            (evil-leader/set-key "T" 'toggle-truncate-lines)
            (evil-leader/set-key "E"
              (lambda () (interactive)
                (eval-current-buffer)
                (set-face-to-large)))
            (evil-leader/set-key "W" 'whitespace-mode)

            ;; cider
            (evil-leader/set-key "t" 'cider-pprint-eval-defun-at-point)
            (evil-leader/set-key "q" 'kill-buffer-and-window)
            (evil-leader/set-key "e" 'cider-eval-last-sexp)
            (evil-leader/set-key "n" 'cider-repl-set-ns)

            (evil-leader/set-key "cn" 'neotree-toggle)
            (evil-leader/set-key-for-mode 'clojure-mode "cc" 'cider-load-buffer)
            (evil-leader/set-key-for-mode 'cider-repl-mode "cc" 'cider-repl-clear-buffer)


            (evil-leader/set-key "v" 'cider-switch-to-repl-buffer-clojure-buffer)
            (evil-leader/set-key "l" 'evil-cleverparens-mode)
            (evil-leader/set-key "'" 'evil-visual-mark-mode)

            ;; Window operation
            ;; C-w, r Rotate windows
            ;; gs windows swap
            ;; (evil-window-rotate-upwards)
            ;;  C-w L/K/J/H.
            ;; (evil-leader/set-key "0" 'delete-window)
            (evil-leader/set-key "C" 'delete-window)
            (evil-leader/set-key "1" 'delete-other-windows)

            (evil-leader/set-key "S" 'ace-swap-window)
            (evil-leader/set-key "7" 'set-face-to-small)
            (evil-leader/set-key "8" 'set-face-to-medium)
            (evil-leader/set-key "9" 'set-face-to-large)
            (global-evil-leader-mode 1)
            (evil-leader-mode)))

;;; displays all the evil marks you have registered on a buffer
(use-package evil-visual-mark-mode
  :ensure t
  :config (evil-visual-mark-mode 1))

;;; hit `*` to search the selection forward, or # to search that selection backward.
(use-package evil-visualstar
  :ensure t
  :config (global-evil-visualstar-mode 1))

(use-package evil-cleverparens
  :ensure t
  :config (evil-cleverparens-mode 1))

(use-package whitespace :ensure t
  :init (setq whitespace-style '(face trailing lines-tail tabs)
              whitespace-line-column 80))

(use-package auto-indent-mode :ensure t
  :init (setq auto-indent-on-save-file t
              auto-indent-blank-lines-on-move nil
              auto-indent-delete-trailing-whitespace-on-save-file t
              auto-indent-untabify-on-save-file t
              auto-indent-indent-style 'aggressive)
  :config (auto-indent-global-mode)
  :diminish auto-indent-mode)

(use-package solarized-theme :ensure t
  :init (progn
          (setq solarized-high-contrast-mode-line t
                solarized-use-less-bold t
                solarized-emphasize-indicators nil
                solarized-scale-org-headlines nil
                x-underline-at-descent-line t)
          (load-theme 'solarized-dark' no-confirm))
  :config (setq color-theme-is-global t))

(defun powerline-center-evil-theme-cus ()
  "Setup a mode-line with major, evil, and minor modes centered."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((file-name (buffer-file-name (current-buffer)))
                          (active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "[" face1)
                                     (powerline-raw (projectile-project-name) face1)
                                     (powerline-raw (concat ""
                                                            (when (and file-name vc-mode)
                                                              (concat ":" (-> file-name
                                                                              vc-working-revision
                                                                              (string-utils-truncate-to 10))
                                                                      "")))
                                                    face1)
                                     (powerline-raw "]" face1)
                                     (powerline-raw " %*%b%* " face1)
                                     (funcall separator-left face1 mode-line)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)

                                     ;; (powerline-narrow face2 'l)
                                     ;; (powerline-vc face1)
                                     ))
                          (rhs (append (list (powerline-raw global-mode-string face1 'r)
                                             (powerline-raw "%4l" face1 'r)
                                             (powerline-raw ":" face1)
                                             (powerline-raw "%3c" face1 'r)
                                             (funcall separator-right face1 mode-line)
                                             (powerline-raw " ")
                                             (powerline-raw "%3p" nil 'r)
                                             )
                                       (if evil-mode
                                           (list (funcall separator-right mode-line face1)
                                                 (powerline-raw evil-mode-line-tag face1 'l)
                                                 (powerline-raw " " face1)
                                                 ;; (funcall separator-left face1 face2)
                                                 ))
                                       ;; (list
                                       ;;  (powerline-hud face1 face2))
                                       ))
                          (center (append (list (powerline-raw " " face1)
                                                (funcall separator-left face1 face2)
                                                (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                                  (powerline-raw erc-modified-channels-object face2 'l))
                                                (powerline-major-mode face2 'l)
                                                (powerline-process face2)
                                                (powerline-raw " " face2))
                                          ;; (if (split-string (format-mode-line minor-mode-alist))
                                          ;;     ;; (append (if evil-mode
                                          ;;     ;;             (list (funcall separator-right face2 face1)
                                          ;;     ;;                   (powerline-raw evil-mode-line-tag face1 'l)
                                          ;;     ;;                   (powerline-raw " " face1)
                                          ;;     ;;                   (funcall separator-left face1 face2)))
                                          ;;     ;;         ;; (list (powerline-minor-modes face2 'l)
                                          ;;     ;;         ;;       (powerline-raw " " face2)
                                          ;;     ;;         ;;       (funcall separator-right face2 face1))
                                          ;;     ;;         )
                                          ;;     (list (powerline-raw evil-mode-line-tag face2)
                                          ;;           (funcall separator-right face2 face1)))
                                          )))
                     (concat (powerline-render lhs)
                             (powerline-fill-center face1 (/ (powerline-width center) 2.0))
                             ;; (powerline-render center)
                             (powerline-fill face1 (powerline-width rhs))
                             (powerline-render rhs)))))))

(use-package powerline :ensure t
  :config (progn
            ;; (powerline-light-thme)
            ;; (powerline-default-theme)
            (powerline-center-evil-theme-cus)
            (setq powerline-arrow-shape 'arrow)))


;; (use-package layout-restore
;;   :ensure t)

(defun my-update-modified-flag ()
  "Update the buffer modified flag."
  (interactive)
  (let* ((buffer (current-buffer))
         (basefile
          (or (buffer-file-name buffer)
              (error "Buffer %s has no associated file" buffer)))
         (tempfile (make-temp-file "buffer-content-")))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) tempfile nil 'silent)))
    (if (= (call-process "diff" nil nil nil basefile tempfile) 0)
        (progn
          (set-buffer-modified-p nil)
          (message "Buffer matches file"))
      (message "Buffer doesn't match file"))
    (delete-file tempfile)))

(message "Hi, We have load almost all config")

;; Disable the annoying square
(setq ring-bell-function 'ignore)

(global-set-key (kbd "M-9") 'paredit-wrap-round)
;; (global-set-key (kbd "C-<right>") 'paredit-forward-slurp-sexp)

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


;; Figwheel
;; (setq cider-cljs-lein-repl
;;       "(do (require 'figwheel-sidecar.repl-api)
;;            (figwheel-sidecar.repl-api/start-figwheel!)
;;            (figwheel-sidecar.repl-api/cljs-repl))")

(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(defun user/cider-send-to-repl ()
  (interactive)
  (let ((s (buffer-substring-no-properties
            (nth 0 (cider-last-sexp 'bounds))
            (nth 1 (cider-last-sexp 'bounds)))))
    (with-current-buffer (cider-current-connection)
      (insert s)
      (cider-repl-return))))


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
