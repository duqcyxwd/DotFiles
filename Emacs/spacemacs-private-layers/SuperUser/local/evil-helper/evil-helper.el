(require 'evil)
(message "loading Evil helper")

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
  "Go to first occurrence of symbol under point."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t)))
    (interactive (list (prefix-numeric-value current-prefix-arg)
                       evil-symbol-word-search))
    (dotimes (var (or count 1))
      (evil-search-word nil nil string))))

(provide 'evil-helper)
