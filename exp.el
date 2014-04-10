(defvar test-message-var "")

(defun insert-to-minibuff ()
  (interactive)
  (select-window (minibuffer-window))
  (with-current-buffer (window-buffer (minibuffer-window))
    (let ((buffer-content nil)
          (start-point nil)
          (end-point nil)
          (curr-point (point))
          (future-point nil))
      (save-excursion
        (forward-line -10)
        (beginning-of-line)
        (setq start-point (point))
        (setq future-point (- curr-point start-point)))
      (save-excursion
        (forward-line 10)
        (beginning-of-line)
        (setq end-point (point)))
      (setq buffer-content (buffer-substring start-point end-point))
      (insert buffer-content)
      (goto-char future-point)
      (recenter))))

(defun read-minibuff-setup-hook ()
  (interactive)
  (when read-minibuff-future-point
    (goto-char read-minibuff-future-point)
    ;; (multicur-create-fake-cursor-at-point)
    (recenter-top-bottom)))

(defun read-minibuff-keyboard-quit ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'read-minibuff-setup-hook)
  (minibuffer-keyboard-quit))

(defvar read-minibuff-future-point nil)

(defun read-minibuff ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'read-minibuff-keyboard-quit)
    (define-key map (kbd "C-g") 'read-minibuff-keyboard-quit)
    (define-key map (kbd "RET") 'multicur-select)
    (define-key map (kbd "M-n") 'multicur-switch-to-next-cursor)
    (define-key map (kbd "M-p") 'multicur-switch-to-previous-cursor)
    (define-key map (kbd "d") 'multicur-delete-cursor)
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

(defvar multicur-buffer-to-switch nil)

(defvar multicur-overlay-to-switch nil)

(defun multicur-minibuffer-exit-hook ()
  (interactive)
  ;; (message "%s" (buffer-name multicur-buffer-to-switch))
  ;; (set-buffer multicur-buffer-to-switch)
  
  (pop-to-buffer multicur-buffer-to-switch)
  ;; (switch-to-other-window)
  ;; (set-buffer multicur-buffer-to-switch)
  ;; (switch-to-buffer multicur-buffer-to-switch)
  ;; (message "%s" (buffer-name (current-buffer)))
  ;; (switch-to-other-window)
  ;; (pop-to-buffer multicur-buffer-to-switch)
  (message "%s" (buffer-name (current-buffer)))
  (with-current-buffer multicur-buffer-to-switch
    (goto-char (overlay-start multicur-overlay-to-switch)))
  (remove-hook 'minibuffer-exit-hook 'multicur-minibuffer-exit-hook))

(defvar multicur-action nil)

(defun multicur-select ()
  (interactive)
  (let* ((current-overlay (nth multicur-current-overlay multicur-overlays))
         (buffer-to-switch (overlay-buffer current-overlay)))
    (setq multicur-buffer-to-switch buffer-to-switch)
    (setq multicur-overlay-to-switch current-overlay)
    (add-hook 'minibuffer-exit-hook 'multicur-minibuffer-exit-hook t)
    ;; (switch-to-other-window)
    ;; (switch-to-buffer-other-window multicur-buffer-to-switch)
    
    ;; (message "%s" (buffer-name (current-buffer)))
    (setq multicur-action 'select)

    (exit-minibuffer)
    ;; (self-insert-and-exit)
    ;; (exit-minibuffer)
    ;; (read-minibuff-keyboard-quit)
    ;; (switch-to-buffer buffer-to-switch)
    ;; (with-current-buffer buffer-to-switch
    ;;   (goto-char (overlay-start current-overlay)))
    ))

(defun multicur-switch-to-cursor ()
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'read-minibuff-keyboard-quit)
    (define-key map (kbd "C-g") 'read-minibuff-keyboard-quit)
    (define-key map (kbd "RET") 'multicur-select)
    (define-key map (kbd "M-n") 'multicur-switch-to-next-cursor)
    (define-key map (kbd "M-p") 'multicur-switch-to-previous-cursor)
    (define-key map (kbd "d") 'multicur-delete-cursor)
    (when (= multicur-current-overlay -1)
      (message "No more cursors."))
    (when (>= multicur-current-overlay 0)
      (let* ((current-overlay (nth multicur-current-overlay multicur-overlays)))
        (when (overlayp current-overlay)
          (let* ((buffer-to-switch (overlay-buffer current-overlay))
                 (overlay-start (overlay-start current-overlay))
                 (shrink-window-p t))
            (when (and (bufferp buffer-to-switch)
                       (buffer-live-p buffer-to-switch))
              (add-hook 'minibuffer-setup-hook 'read-minibuff-setup-hook t)
              (read-from-minibuffer
               ""
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
                     (setq future-point (- overlay-start start-point))
                     (setq read-minibuff-future-point (- overlay-start start-point)))
                   (save-excursion
                     (forward-line 10)
                     (beginning-of-line)
                     (setq end-point (point)))
                   ;; (multicur-create-fake-cursor-at-point)
                   (buffer-substring start-point end-point)))
               map)

              (when (equal multicur-action 'select)
                (pop-to-buffer-same-window buffer-to-switch)
                (with-current-buffer buffer-to-switch
                  (goto-char (overlay-start current-overlay)))))))))))

(defun test-message ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let ((buffer-content nil)
          (start-point nil)
          (end-point nil)
          (curr-point (point))
          (future-point nil))
      (save-excursion
        (forward-line -10)
        (beginning-of-line)
        (setq start-point (point))
        (setq future-point (- curr-point start-point)))
      (save-excursion
        (forward-line 10)
        (beginning-of-line)
        (setq end-point (point)))
      (message (buffer-substring start-point end-point))
      (setq test-message-var (window-body-height (minibuffer-window)))
      )))


(print (current-frame-configuration))


(frame-configuration (#<frame emacs@demi 0x118a320> ((parent-id . 12584184) (explicit-name) (display . ":0.0") (visibility . t) (icon-name) (outer-window-id . "58720271") (window-id . "58720271") (top . 73) (left . 0) (buried-buffer-list #<buffer *Completions*> #<buffer *Help*> #<buffer *Backtrace*>) (buffer-list #<buffer *scratch*> #<buffer *Messages*> #<buffer  *Minibuf-1*> #<buffer smex.el> #<buffer multicur.el> #<buffer google-translate-default-ui.el> #<buffer google-translate-core-ui.el> #<buffer  *Minibuf-0*> #<buffer ido.el> #<buffer *Warnings*> #<buffer *Org Agenda*>) (unsplittable) (minibuffer . #<window 4 on  *Minibuf-0*>) (modeline . t) (width . 145) (height . 40) (name . "emacs@demi") (environment) (cursor-color . "light blue") (background-mode . dark) (display-type . color) (horizontal-scroll-bars . t) (window-system . x) (alpha) (scroll-bar-width . 0) (cursor-type . bar) (auto-lower) (auto-raise) (icon-type . t) (tool-bar-position . top) (fullscreen) (wait-for-wm . t) (title) (buffer-predicate) (tool-bar-lines . 0) (menu-bar-lines . 0) (scroll-bar-background . "#5f5f5f") (scroll-bar-foreground) (right-fringe . 13) (left-fringe . 13) (line-spacing) (screen-gamma) (border-color . "#3f3f3f") (mouse-color . "#dcdccc") (background-color . "#3f3f3f") (foreground-color . "#dcdccc") (vertical-scroll-bars) (internal-border-width . 1) (border-width . 0) (font-parameter . "Source Code Pro Medium:size=22") (font . "-adobe-Source Code Pro-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1") (font-backend xft)) #<window-configuration>))
(frame-configuration (#<frame emacs@demi 0x118a320> ((parent-id . 12584184) (explicit-name) (display . ":0.0") (visibility . t) (icon-name) (outer-window-id . "58720271") (window-id . "58720271") (top . 73) (left . 0) (buried-buffer-list #<buffer *Completions*> #<buffer *Help*> #<buffer *Backtrace*>) (buffer-list #<buffer *scratch*> #<buffer *Messages*> #<buffer  *Minibuf-1*> #<buffer smex.el> #<buffer multicur.el> #<buffer google-translate-default-ui.el> #<buffer google-translate-core-ui.el> #<buffer  *Minibuf-0*> #<buffer ido.el> #<buffer *Warnings*> #<buffer *Org Agenda*>) (unsplittable) ...) #<window-configuration>))


(print (frame-parameters))


((parent-id . 12584184) (explicit-name) (display . ":0.0") (visibility . t) (icon-name) (outer-window-id . "58720271") (window-id . "58720271") (top . 73) (left . 0) (buried-buffer-list #<buffer *Completions*> #<buffer *Help*> #<buffer *Backtrace*>) (buffer-list #<buffer *scratch*> #<buffer  *Minibuf-1*> #<buffer *Messages*> #<buffer smex.el> #<buffer multicur.el> #<buffer google-translate-default-ui.el> #<buffer google-translate-core-ui.el> #<buffer  *Minibuf-0*> #<buffer ido.el> #<buffer *Warnings*> #<buffer *Org Agenda*>) (unsplittable) (minibuffer . #<window 4 on  *Minibuf-0*>) (modeline . t) (width . 145) (height . 40) (name . "emacs@demi") (environment) (cursor-color . "light blue") (background-mode . dark) (display-type . color) (horizontal-scroll-bars . t) (window-system . x) (alpha) (scroll-bar-width . 0) (cursor-type . bar) (auto-lower) (auto-raise) (icon-type . t) (tool-bar-position . top) (fullscreen) (wait-for-wm . t) (title) (buffer-predicate) (tool-bar-lines . 0) (menu-bar-lines . 0) (scroll-bar-background . "#5f5f5f") (scroll-bar-foreground) (right-fringe . 13) (left-fringe . 13) (line-spacing) (screen-gamma) (border-color . "#3f3f3f") (mouse-color . "#dcdccc") (background-color . "#3f3f3f") (foreground-color . "#dcdccc") (vertical-scroll-bars) (internal-border-width . 1) (border-width . 0) (font-parameter . "Source Code Pro Medium:size=22") (font . "-adobe-Source Code Pro-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1") (font-backend xft))
((parent-id . 12584184) (explicit-name) (display . ":0.0") (visibility . t) (icon-name) (outer-window-id . "58720271") (window-id . "58720271") (top . 73) (left . 0) (buried-buffer-list #<buffer *Completions*> #<buffer *Help*> #<buffer *Backtrace*>) (buffer-list #<buffer *scratch*> #<buffer  *Minibuf-1*> #<buffer *Messages*> #<buffer smex.el> #<buffer multicur.el> #<buffer google-translate-default-ui.el> #<buffer google-translate-core-ui.el> #<buffer  *Minibuf-0*> #<buffer ido.el> #<buffer *Warnings*> #<buffer *Org Agenda*>) (unsplittable) ...)

(parent-id . 12584184)(parent-id . 12584184)
((parent-id . 12584184) (explicit-name) (display . ":0.0") (visibility . t) (icon-name) (outer-window-id . "58720271") (window-id . "58720271") (top . 73) (left . 0) (buried-buffer-list #<buffer *Completions*> #<buffer *Help*> #<buffer *Backtrace*>) (buffer-list #<buffer *scratch*> #<buffer  *Minibuf-1*> #<buffer *Messages*> #<buffer smex.el> #<buffer multicur.el> #<buffer google-translate-default-ui.el> #<buffer google-translate-core-ui.el> #<buffer  *Minibuf-0*> #<buffer ido.el> #<buffer *Warnings*> #<buffer *Org Agenda*>) (unsplittable) ...)

(let ((test "best"))(parent-id . 12584184)
  (message "Ok"))(parent-id . 12584184)

(let ((test "best"))
  (message "Test"))
