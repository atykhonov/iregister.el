(defvar iswitchr-current-register 0)

(defvar iswitchr-minibuffer-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iswitchr-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iswitchr-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'iswitchr-select)
    (define-key map (kbd "M-n") 'iswitchr-switch-to-next-register)
    (define-key map (kbd "M-p") 'iswitchr-switch-to-previous-register)
    ;; (define-key map (kbd "d") 'multicur-delete-cursor)
    map))

(defvar iswitchr-action nil)

(defvar iswitchr-minibuffer-position nil)

(defun iswitchr-select ()
  (interactive)
  ;; (add-hook 'minibuffer-exit-hook 'multicur-minibuffer-exit-hook t)
  (setq iswitchr-action 'select)
  (exit-minibuffer))

(defun iswitchr-minibuffer-keyboard-quit ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'iswitchr-minibuffer-setup-hook)
  (minibuffer-keyboard-quit))

(defun iswitchr-minibuffer-setup-hook ()
  (interactive)
  (when iswitchr-minibuffer-position
    (goto-char iswitchr-minibuffer-position))
  (setq iswitchr-minibuffer-position nil)
  (recenter-top-bottom))

(defun iswitchr-next-register ()
  "Retrieve next not in use register."
  (let ((register-candidate nil))
    (when (null iswitchr-register-candidates)
      (setq iswitchr-register-candidates
            (string-to-list iswitchr-possible-registers)))
    (setq register-candidate (car iswitchr-register-candidates))
    (if (null iswitchr-registers-in-use)
        (setq iswitchr-registers-in-use (list register-candidate))
      (add-to-list 'iswitchr-registers-in-use register-candidate t))
    (setq iswitchr-register-candidates (cdr iswitchr-register-candidates))
    register-candidate))

(defun iswitchr-elements-with-markers ()
  (interactive)
  (let ((result (list)))
    (dolist (item register-alist)
      (when (markerp (cdr item))
        (setq result (cons item result))))
    result))

(defun iswitchr-elements-with-markers-length ()
  (length (iswitchr-elements-with-markers)))

(defun iswitchr-elements-with-markers-length-1 ()
  (- (iswitchr-elements-with-markers-length) 1))

(defun iswitchr-point-to-register ()
  (interactive)
  (let ((register (iswitchr-next-register)))
    (point-to-register register)))

(defun iswitchr-switch-to-next-register ()
  (interactive)
  (setq iswitchr-current-register (+ iswitchr-current-register 1))
  (when (> iswitchr-current-register (iswitchr-elements-with-markers-length-1))
    (setq iswitchr-current-register 0))
  (if (minibufferp)
      (progn
        (setq iswitchr-action 'next)
        (exit-minibuffer))
    (iswitchr-switch-to-register)))

(defun iswitchr-switch-to-previous-register ()
  (interactive)
  (setq iswitchr-current-register (- iswitchr-current-register 1))
  (when (< iswitchr-current-register 0)
    (setq iswitchr-current-register (iswitchr-elements-with-markers-length-1)))
  (if (minibufferp)
      (progn
        (setq iswitchr-action 'previous)
        (exit-minibuffer))
    (iswitchr-switch-to-register)))

(defun iswitchr-switch-to-register ()
  (when (= (iswitchr-elements-with-markers-length) 0)
    (message "No more registers."))
  (when (> (iswitchr-elements-with-markers-length) 0)
    (let* ((register-element (car (nthcdr iswitchr-current-register
                                          (iswitchr-elements-with-markers))))
           (register-name (car register-element))
           (register-marker (cdr register-element))
           (buffer-to-switch nil)
           (position nil))
      (setq buffer-to-switch (marker-buffer register-marker))
      (setq position (marker-position register-marker))
      (add-hook 'minibuffer-setup-hook 'iswitchr-minibuffer-setup-hook t)
      (read-from-minibuffer
       ""
       (let ((buffer-content nil)
             (start-point nil)
             (end-point nil))
         (with-current-buffer buffer-to-switch
           (save-excursion
             (goto-char position)
             (forward-line -20)
             (beginning-of-line)
             (setq start-point (point))
             (setq iswitchr-minibuffer-position (+ (- position start-point) 1)))
           (save-excursion
             (goto-char position)
             (forward-line 20)
             (beginning-of-line)
             (setq end-point (point)))
           (buffer-substring start-point end-point)))
       iswitchr-minibuffer-keymap)
      (when (equal iswitchr-action 'select)
        (register-to-point register-name))
      (when (or (equal iswitchr-action 'next)
                (equal iswitchr-action 'previous))
        (iswitchr-switch-to-register))
      (setq iswitchr-action nil))))

(defun read-minibuff ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iswitchr-keyboard-quit)
    (define-key map (kbd "C-g") 'iswitchr-keyboard-quit)
    (define-key map (kbd "RET") 'iswitchr-switch-to-register)
    (define-key map (kbd "M-n") 'iswitchr-switch-to-next-register)
    (define-key map (kbd "M-p") 'iswitchr-switch-to-previous-register)
    (add-hook 'minibuffer-setup-hook 'read-minibuff-setup-hook t)
    (read-from-minibuffer
     ""
     (with-current-buffer (current-buffer)
       (let ((buffer-content nil)
             (start-point nil)
             (end-point nil)
             (curr-point (point))
             (future-point nil))
         (save-excursion
           (forward-line -20)
           (beginning-of-line)
           (setq start-point (point))
           (setq future-point (- curr-point start-point))
           (setq read-minibuff-future-point (- curr-point start-point)))
         (save-excursion
           (forward-line 20)
           (beginning-of-line)
           (setq end-point (point)))
         (buffer-substring start-point end-point)))
     map)))
