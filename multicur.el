(defface multicur-cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'multiple-cursors)

(defvar multicur-overlays (list))

(defvar multicur-current-overlay -1)

(defvar multicur-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'quit-window)
    map))

(defvar multicur-buffer-name "*Multicur*")

(defun multicur-create-fake-cursor-at-point ()
  "Add a fake cursor and possibly a fake active region overlay based on point and mark.
Saves the current state in the overlay to be restored later."
  (let ((overlay (multicur-make-cursor-overlay-at-point)))
    (overlay-put overlay 'type 'fake-cursor)
    (overlay-put overlay 'priority 100)
    (setq multicur-overlays (append multicur-overlays (list overlay)))
    overlay))

(defun multicur-make-cursor-overlay-at-eol (pos)
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (overlay-put overlay 'after-string (propertize " " 'face 'multicur-cursor-face))
    overlay))

(defun multicur-make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
    (overlay-put overlay 'face 'multicur-cursor-face)
    overlay))

(defun multicur-make-cursor-overlay-at-point ()
  "Create overlay to look like cursor.
Special case for end of line, because overlay over a newline
highlights the entire width of the window."
  (if (eolp)
      (multicur-make-cursor-overlay-at-eol (point))
    (multicur-make-cursor-overlay-inline (point))))

(defun multicur-all-fake-cursors (&optional start end)
  (remove-if-not 'multicur-fake-cursor-p
                 (overlays-in (or start (point-min))
                              (or end   (point-max)))))

(defun multicur-fake-cursor-p (o)
  "Predicate to check if an overlay is a fake cursor"
  (eq (overlay-get o 'type) 'fake-cursor))

(defun multicur-goto-next-cursor ()
  (interactive)
  (let ((found nil))
    (dolist (overlay (nreverse (multicur-all-fake-cursors)))
      (when (and (null found)
                 (> (overlay-start overlay) 
                    (point)))
        (goto-char (overlay-start overlay))
        (setq found t)))))

(defun multicur-goto-previous-cursor ()
  (interactive)
  (let ((found nil))
    (dolist (overlay (multicur-all-fake-cursors))
      (when (and (null found)
                 (< (overlay-start overlay)
                    (point)))
        (goto-char (overlay-start overlay))
        (setq found t)))))

(defun multicur-delete-cursor ()
  (interactive)
  (let ((current-overlay (nth multicur-current-overlay multicur-overlays)))
    (setq multicur-overlays (delq current-overlay multicur-overlays))
    (delete-overlay current-overlay)
    (if (= (length multicur-overlays) 0)
        (progn
          (kill-buffer-and-window)
          (message "No more cursors."))
      (multicur-switch-to-next-cursor))))

(defun multicur-delete-all-cursors ()
  (interactive)
  (dolist (overlay multicur-overlays)
    (delete-overlay overlay))
  (setq multicur-overlays (list))
  (setq multicur-current-overlay -1)
  (kill-buffer multicur-buffer-name))

(defun multicur-switch-to-next-cursor ()
  (interactive)
  (setq multicur-current-overlay (+ multicur-current-overlay 1))
  (when (> multicur-current-overlay (- (length multicur-overlays) 1))
    (setq multicur-current-overlay 0))
  (multicur-switch))

(defun multicur-switch-to-previous-cursor ()
  (interactive)
  (setq multicur-current-overlay (- multicur-current-overlay 1))
  (when (< multicur-current-overlay 0)
    (setq multicur-current-overlay (- (length multicur-overlays) 1)))
  (multicur-switch))

(defun multicur-switch-to-cursor ()
  (let ((multicur-buffer (get-buffer-create multicur-buffer-name)))
    (when (= multicur-current-overlay -1)
      (message "No more cursors."))
    (when (> multicur-current-overlay 0)
      (let* ((current-overlay (nth multicur-current-overlay multicur-overlays))
             (buffer-to-switch (overlay-buffer current-overlay))
             (overlay-start (overlay-start current-overlay))
             (shrink-window-p t))
        (when (and (bufferp buffer-to-switch)
                   (buffer-live-p buffer-to-switch))
          (when (display-buffer-reuse-window multicur-buffer nil)
            (setq shrink-window-p nil))
          (pop-to-buffer multicur-buffer)
          (when shrink-window-p
            (shrink-window 10))
          (with-current-buffer multicur-buffer
            (multicur-minor-mode 1)
            (erase-buffer)
            (let ((buffer-content nil)
                  (start-point nil)
                  (end-point nil)
                  (future-point nil))
              (with-current-buffer buffer-to-switch
                (goto-char overlay-start)
                (save-excursion
                  (forward-line -10)
                  (beginning-of-line)
                  (setq start-point (point))
                  (setq future-point (- overlay-start start-point)))
                (save-excursion
                  (forward-line 10)
                  (beginning-of-line)
                  (setq end-point (point)))
                (setq buffer-content (buffer-substring start-point end-point)))
              (insert buffer-content)
              (goto-char future-point)
              (recenter))))))))

(defun multicur-select ()
  (interactive)
  (let* ((current-overlay (nth multicur-current-overlay multicur-overlays))
         (buffer-to-switch (overlay-buffer current-overlay)))
    (kill-buffer-and-window)
    (switch-to-buffer buffer-to-switch)
    (with-current-buffer buffer-to-switch
      (goto-char (overlay-start current-overlay)))))

(defun multicur-kill-buffer ()
  (interactive)
  (kill-buffer))

(define-minor-mode multicur-minor-mode
  "Toggle multicur minor mode."
  :lighter " MultiCur"
  :keymap (let ((map (make-sparse-keymap)))
            (suppress-keymap map)
            (define-key map "q" 'kill-buffer-and-window)
            (define-key map (kbd "RET") 'multicur-select)
            (define-key map (kbd "M-n") 'multicur-switch-to-next-cursor)
            (define-key map (kbd "M-p") 'multicur-switch-to-previous-cursor)
            (define-key map (kbd "d") 'multicur-delete-cursor)
            map)
  :group 'multicur)


(provide 'multicur)

;;; multicur.el ends here
