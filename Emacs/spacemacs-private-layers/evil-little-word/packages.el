(setq evil-little-word-packages '((evil-little-word :location local)
                                  ))


(defun evil-little-word/init-evil-little-word ()
  (use-package evil-little-word
    :commands (evil-forward-little-word-begin
               evil-backward-little-word-begin
               evil-forward-little-word-end
               evil-backward-little-word-end
               evil-a-little-word
               evil-inner-little-word)
    :init
    (progn
      (message "loading evil little word")
      (define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
      (define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
      (define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
      (define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
      ; (define-key evil-motion-state-map "gs" 'evil-window-right)
      ; (define-key evil-motion-state-map "gk" 'evil-window-left)
      ; (define-key evil-motion-state-map "gp" 'evil-window-left)
      (define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
      (define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word))))


; (defun evil-little-word/init-evil-helper ()
;   (use-package evil-helper
;     :commands (find-next-full-wordds
;       find-prev-full-wordds)
;     :init
;     (progn
;       (message "loading evil little word22")
;       (define-key evil-normal-state-map "gn" 'find-next-full-wordds)
;       (define-key evil-normal-state-map "gp" 'find-prev-full-wordds)
;       )))
