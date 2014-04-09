;; (defvar mc--current-cursor-id 0
;;   "Var to store increasing id of fake cursors, used to keep track of them for undo.")

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
    ;; (overlay-put overlay 'mc-id (or id (mc/create-cursor-id)))
    (overlay-put overlay 'type 'fake-cursor)
    (overlay-put overlay 'priority 100)
    (setq multicur-overlays (append multicur-overlays (list overlay)))
    ;; (append multicur-overlays (list overlay))
    ;; (mc/store-current-state-in-overlay overlay)
    ;; (when (use-region-p)
    ;;   (overlay-put overlay 'region-overlay
    ;;                (mc/make-region-overlay-between-point-and-mark)))
    overlay))

;; (defun mc/store-current-state-in-overlay (o)
;;   "Store relevant info about point and mark in the given overlay."
;;   (overlay-put o 'point (set-marker (make-marker) (point)))
;;   (overlay-put o 'mark (set-marker (make-marker) (mark)))
;;   (dolist (var mc/cursor-specific-vars)
;;     (when (boundp var) (overlay-put o var (symbol-value var))))
;;   o)

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

;; (defun mc/create-cursor-id ()
;;   "Returns a unique cursor id"
;;   (incf mc--current-cursor-id))

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

(defun multicur-switch-next ()
  (interactive)
  (setq multicur-current-overlay (+ multicur-current-overlay 1))
  (when (> multicur-current-overlay (- (length multicur-overlays) 1))
    (setq multicur-current-overlay 0))
  (multicur-switch))

(defun multicur-switch-previous ()
  (interactive)
  (setq multicur-current-overlay (- multicur-current-overlay 1))
  (when (< multicur-current-overlay 0)
    (setq multicur-current-overlay (- (length multicur-overlays) 1)))
  (multicur-switch))

(defun multicur-switch ()
  (let* ((multicur-buffer (get-buffer-create multicur-buffer-name))
         (current-overlay (nth multicur-current-overlay multicur-overlays))
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
          (recenter))))))

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
            (define-key map (kbd "M-n") 'multicur-switch-next)
            (define-key map (kbd "M-p") 'multicur-switch-previous)
            ;; (define-key map (kbd "RET") 'multicur-select)
            map)
  :group 'multicur)

(defun multicur-save-history ()
  "Save ido history and cache information between sessions."
  (interactive)
  (when (and ido-last-directory-list ido-save-directory-list-file)
    (let ((buf (get-buffer-create " *ido session*"))
	  (version-control 'never))
      (unwind-protect
	  (with-current-buffer buf
	    (erase-buffer)
	    (insert ";;; -*- coding: utf-8 -*-\n")
	    (setq buffer-file-coding-system 'utf-8)
	    (ido-pp 'ido-last-directory-list)
	    (ido-pp 'ido-work-directory-list)
	    (ido-pp 'ido-work-file-list)
	    (ido-pp 'ido-dir-file-cache "\n\n ")
	    (if (listp ido-unc-hosts-cache)
		(ido-pp 'ido-unc-hosts-cache)
	      (insert "\n;; ----- ido-unc-hosts-cache -----\nt\n"))
	    (write-file ido-save-directory-list-file nil))
	(kill-buffer buf)))))

(defun multicur-load-history (&optional arg)
  "Load ido history and cache information from previous session.
With prefix argument, reload history unconditionally."
  (interactive "P")
  (if (or arg (and ido-save-directory-list-file (not ido-last-directory-list)))
      (let ((file (expand-file-name ido-save-directory-list-file))
	    buf)
	(when (file-readable-p file)
	  (setq buf (get-buffer-create " *ido session*"))
	  (unwind-protect
	      (with-current-buffer buf
		(erase-buffer)
		(insert-file-contents file)
		(condition-case nil
		    (setq ido-last-directory-list (read (current-buffer))
			  ido-work-directory-list (read (current-buffer))
			  ido-work-file-list (read (current-buffer))
			  ido-dir-file-cache (read (current-buffer))
			  ido-unc-hosts-cache (read (current-buffer)))
		  (error nil)))
	    (kill-buffer buf)))))
  (ido-wash-history))
