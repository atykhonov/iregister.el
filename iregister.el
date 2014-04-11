;;; iregister.el --- Interactive register commands for Emacs.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/iregister.el
;; Version: 0.1.0
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; This package is built on top of register.el package and it allows to use registers
;; interactively.  For now it does wrap only functionality which relates to points
;; (`register-to-point', `point-to-register').
;;
;; Assuming that there are already stored (by means of `point-to-register') some
;; points in the registers (in current Emacs session).  Execute, for example,
;; `iregister-jump-to-next-marker' and the minibuffer will display the snippet of the
;; stored point's buffer.  That snippet will contain the text arround of stored point
;; which allows to figure out whether is that right place to be jumped in or not.
;; If yes, then just hit the `RET' and the right buffer will be displayed and the
;; point will be in the same place as it was stored before.  If no, then try to hit
;; `M-n' (`iregister-jump-to-next-marker') or `M-p' (`iregister-jump-to-previous-marker')
;; to view next/previous markers (points) previously stored in the registry.  In the
;; meantime, in the minibuffer, you could hit `d' key to delete current point from
;; the register.  To quit from the minibuffer press `q' key.
;;
;; Optionally you could use `iregister-point-to-register' command from any buffer to
;; store current point to register.  That command executes without any prompt, it just
;; finds any empty register and stores there current point.
;;

;; Installation:

;; Assuming that the file `iregister.el' is somewhere on the load path, add the
;; following lines to your `.emacs' file:

;; (require 'iregister)
;; (global-set-key (kbd "M-n") 'iregister-jump-to-next-marker)
;; (global-set-key (kbd "M-p") 'iregister-jump-to-previous-marker)
;; (global-set-key (kbd "M-u") 'iregister-point-to-register)
;;
;; Change the key bindings to your liking.

;;; Code:


(defvar iregister-current-register 0)

(defvar iregister-action nil)

(defvar iregister-minibuffer-position nil)

(defvar iregister-minibuffer-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'iregister-jump-to-current-marker)
    (define-key map (kbd "M-n") 'iregister-jump-to-next-marker)
    (define-key map (kbd "M-p") 'iregister-jump-to-previous-marker)
    (define-key map (kbd "d") 'iregister-delete-register)
    map))

(defun iregister-point-to-register ()
  (interactive)
  (let ((idx 0)
        (stored nil))
    (while (and (< idx 256)
                (null stored))
      (when (null (get-register idx))
        (setq stored t)
        (point-to-register idx))
      (setq idx (+ idx 1)))))

(defun iregister-delete-register ()
  (interactive)
  (let* ((register-element (car (nthcdr iregister-current-register
                                        (iregister-elements-with-markers))))
         (register-name (car register-element)))
    (set-register register-name nil)
    (iregister-jump-to-next-marker)))

(defun iregister-jump-to-current-marker ()
  (interactive)
  ;; (add-hook 'minibuffer-exit-hook 'multicur-minibuffer-exit-hook t)
  (setq iregister-action 'jump)
  (exit-minibuffer))

(defun iregister-minibuffer-keyboard-quit ()
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'iregister-minibuffer-setup-hook)
  (minibuffer-keyboard-quit))

(defun iregister-minibuffer-setup-hook ()
  (interactive)
  (when iregister-minibuffer-position
    (goto-char iregister-minibuffer-position))
  (setq iregister-minibuffer-position nil)
  (recenter-top-bottom))

(defun iregister-elements-with-markers ()
  (interactive)
  (let ((result (list)))
    (dolist (item register-alist)
      (when (markerp (cdr item))
        (setq result (cons item result))))
    result))

(defun iregister-elements-with-markers-length ()
  (length (iregister-elements-with-markers)))

(defun iregister-elements-with-markers-length-1 ()
  (- (iregister-elements-with-markers-length) 1))

(defun iregister-jump-to-next-marker ()
  (interactive)
  (setq iregister-current-register (+ iregister-current-register 1))
  (when (> iregister-current-register (iregister-elements-with-markers-length-1))
    (setq iregister-current-register 0))
  (if (minibufferp)
      (progn
        (setq iregister-action 'next)
        (exit-minibuffer))
    (iregister--jump-to-marker)))

(defun iregister-jump-to-previous-marker ()
  (interactive)
  (setq iregister-current-register (- iregister-current-register 1))
  (when (< iregister-current-register 0)
    (setq iregister-current-register (iregister-elements-with-markers-length-1)))
  (if (minibufferp)
      (progn
        (setq iregister-action 'previous)
        (exit-minibuffer))
    (iregister--jump-to-marker)))

(defun iregister--jump-to-marker ()
  (when (= (iregister-elements-with-markers-length) 0)
    (message "No more registers."))
  (when (> (iregister-elements-with-markers-length) 0)
    (let* ((register-element (car (nthcdr iregister-current-register
                                          (iregister-elements-with-markers))))
           (register-name (car register-element))
           (register-marker (cdr register-element))
           (buffer-to-switch (marker-buffer register-marker))
           (position (marker-position register-marker)))
      (add-hook 'minibuffer-setup-hook 'iregister-minibuffer-setup-hook t)
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
             (setq iregister-minibuffer-position (+ (- position start-point) 1)))
           (save-excursion
             (goto-char position)
             (forward-line 20)
             (beginning-of-line)
             (setq end-point (point)))
           (buffer-substring start-point end-point)))
       iregister-minibuffer-keymap)
      (when (equal iregister-action 'jump)
        (register-to-point register-name))
      (when (or (equal iregister-action 'next)
                (equal iregister-action 'previous))
        (iregister--jump-to-marker))
      (setq iregister-action nil))))

(defun iregister-copy-to-register (start end &optional delete-flag)
  "Copy region into the any empty register."
  (interactive "r\nP")
  (let ((idx 0)
        (stored nil))
    (while (and (< idx 256)
                (null stored))
      (when (null (get-register idx))
        (setq stored t)
        (copy-to-register idx start end))
      (setq idx (+ idx 1)))))


(provide 'iregister)

;;; iregister.el ends here
