(require 'cider)

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

(defun cider-send-to-repl ()
  (interactive)
  (let ((s (buffer-substring-no-properties
            (nth 0 (cider-last-sexp 'bounds))
            (nth 1 (cider-last-sexp 'bounds)))))
    (with-current-buffer (cider-current-connection)
      (insert s)
      (cider-repl-return))))

(defun cider-switch-to-repl-buffer-clojure-buffer ()
  (interactive)
  (if (equal major-mode 'cider-repl-mode)
      (cider-switch-to-last-clojure-buffer)
    (cider-switch-to-repl-buffer)))


(provide 'cider-helper)
