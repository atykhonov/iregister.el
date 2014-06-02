;;; iregister.el --- Interactive register commands for Emacs.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/iregister.el
;; Version: 0.5.0
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
;; interactively.
;;
;; Jump interactivelly to the markers stored in the registers.
;;
;; Assuming that there are already stored some points (by means of
;; `point-to-register' or `iregister-point-to-register' command) in the
;; registers. Execute, for example, `iregister-jump-to-next-marker' and the
;; minibuffer will display the snippet of the stored point's buffer. That snippet
;; will contain the text arround of stored point which allows to figure out whether
;; is that right place to be jumped in or not. If yes, then just hit the `RET' and
;; the right buffer will be displayed and the point will be in the same place as it
;; was stored before. If no, then try to hit `n' key or `p' key to view next/previous
;; markers (points) previously stored in the registers. In the meantime, in the
;; minibuffer, you could hit `d' key to delete current point from the register. To
;; quit from the minibuffer press `q' key (or C-g).
;;
;; Optionally you could use `iregister-point-to-register' command from any buffer to
;; store current point to register. That command works exactly as `point-to-register'
;; command but without any prompt, it just finds any empty register and stores there
;; current point.
;; 
;; Insert (append/prepend) interactivelly the texts stored in the registers.
;;
;; Assuming that there are already stored some texts (by means of `copy-to-register'
;; or `iregister-copy-to-register' command) in the registers. Execute
;; `iregister-text' and the minibuffer will display the text stored in some
;; register. In the appeared minibuffer you can figure out whether is that right text
;; to be inserted or not. If yes, then just hit the `RET' and the text will be
;; inserted. If no, then try to hit `n' key or `p' key to view next/previous texts
;; previously stored in the registers. In the meantime, in the minibuffer, you could
;; hit `d' key to delete current text from the register. To quit from the minibuffer
;; press `q' key (or C-g). Also you could use `a' key for appending or `A' key for
;; prepending selected text to the current text registry.
;;
;; Optionally you could use `iregister-copy-to-register' command from any buffer to
;; store selected text to a register. That command works exactly as
;; `copy-to-register' command but without any prompt, it just finds any empty
;; register and stores there selected text.
;;
;; If execute `C-u M-x iregister-copy-to-register' then selected text will be deleted
;; without modifying the kill ring. In case of `C-u C-u M-x
;; iregister-copy-to-register' the selected text will be deleted and saved in the
;; kill ring and in a register as well. In case of `C-u C-u C-u M-x
;; iregister-copy-to-register' the selected text will be `kill-ring-save' (See
;; the documentation for the `kill-ring-save' function).
;;
;; Instead of `C-u M-x iregister-copy-to-register' or `C-u C-u M-x
;; iregister-copy-to-register' or `C-u C-u C-u M-x iregister-copy-to-register' you
;; could use such functions as `iregister-copy-to-register-delete',
;; `iregister-copy-to-register-kill' and
;; `iregister-copy-to-register-kill-ring-save'. These commands are useful to use with
;; key bindings.
;;
;; The command `iregister-append-to-latest-register' allows to append selected text to
;; the last used register. After execution of `iregister-copy-to-register' the command
;; `iregister-append-to-latest-register' will append selected text to the same register.
;;
;; If execute `C-u M-x iregister-append-to-latest-register' the selected text will be
;; deleted without modifying the kill ring. In case of `C-u C-u M-x
;; iregister-append-to-latest-register' the selected text will be deleted and saved
;; in the kill ring.
;;
;; Instead of `C-u M-x iregister-append-to-latest-register' and `C-u C-u M-x
;; iregister-append-to-latest-register' you could use such functions as
;; `iregister-append-to-latest-register-delete' and
;; `iregister-append-to-latest-register-kill'. These commands are useful to use with
;; key bindings.
;;
;; Utility functions

;; There are also two helpful functions which you could use with some key binding:
;; `iregister-point-or-text-to-register' and
;; `iregister-point-or-text-to-register-kill-ring-save'. `iregister-point-or-text-to-register'
;; function stores point to register in case of region is inactive, otherwise stores
;; a text to register. `iregister-point-or-text-to-register-kill-ring-save' behaves
;; mostly in the same way as `iregister-point-or-text-to-register' but it perform
;; `kill-ring-save' on active region thus it save the text for a window system cut
;; and paste. You can bind `iregister-point-or-text-to-register-kill-ring-save' to
;; the `M-w' key and that would allow to `kill-ring-save' an active region and store
;; it to the register. In case of region is inactive it will store point to register.

;; Installation:

;; From MELPA or Marmalade.

;; Just run `M-x package-install RET iregister RET`

;; Manual installation.

;; Assuming that the file `iregister.el' is somewhere on the load path, add the
;; following lines to your `.emacs' file:

;; (require 'iregister)
;; (global-set-key (kbd "M-n") 'iregister-jump-to-next-marker)
;; (global-set-key (kbd "M-p") 'iregister-jump-to-previous-marker)
;; (global-set-key (kbd "M-u") 'iregister-point-or-text-to-register)
;;
;; If region is active then `iregister-point-or-text-to-register' command stores a
;; text to any empty register, otherwise it stores a point.

;; (global-set-key (kbd "M-l") 'iregister-latest-text)
;;
;; You can also try to bind `iregister' functions in the following way:
;;
;; (global-set-key (kbd "M-w") 'iregister-point-or-text-to-register-kill-ring-save)
;; (global-set-key (kbd "C-w") 'iregister-copy-to-register-kill)
;; (global-set-key (kbd "M-y") 'iregister-latest-text)
;;
;; Anyway change the key bindings to your liking.

;;; Code:



(require 'iregister-text)

(defvar iregister-list-text-registers-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'iregister-list-text-registers-insert)
    (define-key map (kbd "C-j") 'iregister-list-text-registers-insert)
    (define-key map (kbd "n") 'iregister-list-text-registers-next-register)
    (define-key map (kbd "p") 'iregister-list-text-registers-previous-register)
    (define-key map (kbd "i") 'iregister-list-text-registers-insert)
    (define-key map (kbd "d") 'iregister-list-text-registers-delete-register)
    map)
  "Keymap for minibuffer when display a text register.")

(defvar iregister-action-contents nil
  "Temp variable which contains action contents.")

(defun iregister--shade-color (intensity)
  "print the #rgb color of the background, dimmed according to intensity"
  (interactive "nIntensity of the shade : ")
  (apply 'format "#%02x%02x%02x" 
         (mapcar (lambda (x)
                   (if (> (lsh x -8) intensity)
                       (- (lsh x -8) intensity)
                     0))
                 (color-values (cdr (assoc 'background-color (frame-parameters)))))))

(defalias 'list-text-registers 'iregister-list-text-registers)
(defun iregister-list-text-registers ()
  "List all text registers."
  (interactive)
  (when (minibufferp)
    (progn
      (setq iregister-action 'list-text-registers)
      (iregister-exit-minibuffer)))
  (if (= (length (iregister-elements-with-strings)) 0)
      (message "There are not any text registers.")
    (let ((buffer-name (get-buffer-create "*iRegister: text registers*"))
          (inhibit-modification-hooks t))
      (make-face 'iregister-temp-face)
      (set-face-background 'iregister-temp-face (iregister--shade-color 15))
      (set-face-attribute 'iregister-temp-face nil :height 0.3)
      (read-from-minibuffer
       ""
       (with-temp-buffer
         (dolist (item (reverse (iregister-elements-with-strings)))
           (let (ov-start ov-end
                          (register (car item)))
             (setq ov-start (point))
             (let ((beg (point)))
               (insert (cdr item))
               (put-text-property beg (point) 'current-point beg)
               (put-text-property beg (point) 'next-point (+ (point) 2)))
             (insert "\n")
             (let ((beg (point)))
               (insert "\n")
               (setq ov-end (point))
               (put-text-property beg (point) 'face 'iregister-temp-face))
             (put-text-property ov-start ov-end 'register register)))
         (setq iregister-minibuffer-position 1)
         (buffer-substring (point-min) (point-max)))
       iregister-list-text-registers-keymap)
      (when (equal iregister-action 'insert)
        (insert iregister-action-contents))
      (when (equal iregister-action 'delete)
        (iregister-list-text-registers)))))

(defun iregister-list-text-registers-to-register (direction)
  "Move to the next register in the list if direction equal 1 and
move to the previous register in the list if direction equal -1."
  (let ((found nil)
        (previous-register nil)
        (current-register nil)
        (idx 0)
        (register-position nil)
        (registers-list (if (eq direction -1)
                            (reverse (iregister-elements-with-strings))
                          (iregister-elements-with-strings))))
    (when (eq direction -1)
      (setq ))
    (while (and (< idx (length registers-list))
                (null found))
      (setq previous-register current-register)
      (setq current-register (car (nth idx registers-list)))
      (when (eq current-register (get-text-property (point) 'register))
        (setq found t))
      (setq idx (+ idx 1)))
    (if previous-register
        (progn
          (setq register-position (text-property-any (point-min) (point-max) 'register previous-register))
          (when register-position
            (goto-char register-position)))
      (if (eq direction 1)
          (goto-char (point-min))
        (progn
          (goto-char (point-max))
          (forward-line -2))))))

(defun iregister-list-text-registers-next-register ()
  "Move cursor to the next register in the list."
  (interactive)
  (iregister-list-text-registers-to-register 1))

(defun iregister-list-text-registers-previous-register ()
  "Move cursor to the previous register in the list"
  (interactive)
  (iregister-list-text-registers-to-register -1))

(defun iregister-list-text-registers-insert ()
  "Insert to the buffer a text from the current text register."
  (interactive)
  (let* ((register (get-text-property (point) 'register))
         (register-contents (cdr (assoc register register-alist))))
    (setq iregister-action 'insert)
    (setq iregister-action-contents register-contents)
    (iregister-exit-minibuffer)))

(defun iregister-list-text-registers-delete-register ()
  "Delete current text marker from the list registers."
  (interactive)
  (let* ((register (get-text-property (point) 'register))
         (start (text-property-any (point-min) (point-max) 'register register))
         (end (text-property-not-all start (point-max) 'register register)))
    (set-register register nil)
    (if (null end)
        (setq end (point-max))
      (setq end (+ end 2)))
    (delete-region start end)
    (when (eobp)
      (forward-line -2))
    (when (= (length (iregister-elements-with-strings)) 0)
      (iregister-exit-minibuffer))))

(provide 'iregister-list-text)
;;; iregister-list-text.el ends here
