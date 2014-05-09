;;; iregister.el --- Interactive register commands for Emacs.

;; Copyright (C) 2014 Andrey Tykhonov <atykhonov@gmail.com>

;; Author: Andrey Tykhonov <atykhonov@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/iregister.el
;; Version: 0.3.1
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
;; iregister-copy-to-register' you could use such functions as
;; `iregister-delete-copy-to-register' and `iregister-kill-copy-to-register'. These
;; commands are useful to use with key bindings.
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
;; `iregister-delete-append-to-latest-register' and
;; `iregister-kill-append-to-latest-register'. These commands are useful to use with
;; key bindings.

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

;; (global-set-key (kbd "M-l") 'iregister-text)
;;
;; Change the key bindings to your liking.

;;; Code:



(defvar iregister-current-marker-register 0
  "An index to the current marker register.")

(defvar iregister-current-text-register 0
  "An index to the current text register.")

(defvar iregister-action nil
  "Temp variable to allow to take right action after minibuffer
exit.")

(defvar iregister-minibuffer-position nil
  "Temp variable which holds a point position to which it is
required to jump.")

(defvar iregister-last-used-register nil
  "A last used register in the `iregister-copy-to-register'
function.")

(defvar iregister-minibuffer-marker-keymap
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'iregister-jump-to-current-marker)
    (define-key map (kbd "C-j") 'iregister-jump-to-current-marker)
    (define-key map (kbd "n") 'iregister-jump-to-next-marker)
    (define-key map (kbd "p") 'iregister-jump-to-previous-marker)
    (define-key map (kbd "d") 'iregister-delete-marker-register)
    map)
  "Keymap for the minibuffer when display a marker register.")

(defvar iregister-minibuffer-text-keymap  
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "C-g") 'iregister-minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'iregister-insert-text)
    (define-key map (kbd "C-j") 'iregister-insert-text)
    (define-key map (kbd "n") 'iregister-next-text)
    (define-key map (kbd "p") 'iregister-previous-text)
    (define-key map (kbd "a") 'iregister-append-text)
    (define-key map (kbd "A") 'iregister-prepend-text)
    (define-key map (kbd "d") 'iregister-delete-text-register)
    (define-key map (kbd "s") 'iregister-save-text-to-register)
    map)
  "Keymap for minibuffer when display a text register.")



;; Common

(defun iregister-minibuffer-keyboard-quit ()
  "A thin wrapper arround `minibuffer-keyboard-quit'."
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'iregister-minibuffer-setup-hook)
  (minibuffer-keyboard-quit))

;;;###autoload
(defun iregister-point-or-text-to-register (&optional delete-flag)
  "Store point or text to any empty register. If region is active
then store a text, otherwise a point. With a `C-u' prefix
argument, delete active region. With a `C-u C-u' prefix argument,
kill active region."
  (interactive "P")
  (if (region-active-p)
      (iregister-copy-to-register (region-beginning) (region-end) delete-flag)
    (iregister-point-to-register)))

;; Interactive registers with markers

(defun iregister-minibuffer-setup-hook ()
  "Setup hook to be triggered after entering minibuffer."
  (interactive)
  (when iregister-minibuffer-position
    (goto-char iregister-minibuffer-position))
  (setq iregister-minibuffer-position nil)
  (recenter-top-bottom))

;;;###autoload
(defun iregister-point-to-register ()
  "Find empty register and store current point to it.  Works
exactly as `point-to-register' does work except it doesn't prompt
for register-name."
  (interactive)
  (let ((idx 0)
        (stored nil))
    (while (and (< idx 256)
                (null stored))
      (when (null (get-register idx))
        (setq stored t)
        (point-to-register idx))
      (setq idx (+ idx 1)))))

;;;###autoload
(defun iregister-jump-to-current-marker ()
  "Jump to the current marker from the minibuffer."
  (interactive)
  (setq iregister-action 'jump)
  (exit-minibuffer))

(defun iregister-delete-marker-register ()
  "Delete the current marker from the register."
  (interactive)
  (let* ((register-element (car (nthcdr iregister-current-marker-register
                                        (iregister-elements-with-markers))))
         (register-name (car register-element)))
    (set-register register-name nil)
    (iregister-jump-to-next-marker)))

(defun iregister-elements-with-markers ()
  "Retrieve all elements from the register which contain
markers."
  (interactive)
  (let ((result (list)))
    (dolist (item register-alist)
      (when (markerp (cdr item))
        (setq result (cons item result))))
    result))

(defun iregister-elements-with-markers-length ()
  "Length of the list with marker elements."
  (length (iregister-elements-with-markers)))

(defun iregister-elements-with-markers-length-1 ()
  (- (iregister-elements-with-markers-length) 1))

;;;###autoload
(defun iregister-jump-to-next-marker ()
  "If the minibuffer is the current buffer then jump to the next
marker.  Otherwise show the minibuffer with the text arround next
marker and allows to select interactively required
marker.  Markers retrieves from the registers."
  (interactive)
  (setq iregister-current-marker-register (+ iregister-current-marker-register 1))
  (when (> iregister-current-marker-register (iregister-elements-with-markers-length-1))
    (setq iregister-current-marker-register 0))
  (if (minibufferp)
      (progn
        (setq iregister-action 'next)
        (exit-minibuffer))
    (iregister--jump-to-marker)))

;;;###autoload
(defun iregister-jump-to-previous-marker ()
  "If the minibuffer is the current buffer then jump to the
previous marker.  Otherwise show the minibuffer with the text
arround previous marker and allows to select interactively
required marker.  Markers retrieves from the registers."
  (interactive)
  (setq iregister-current-marker-register (- iregister-current-marker-register 1))
  (when (< iregister-current-marker-register 0)
    (setq iregister-current-marker-register (iregister-elements-with-markers-length-1)))
  (if (minibufferp)
      (progn
        (setq iregister-action 'previous)
        (exit-minibuffer))
    (iregister--jump-to-marker)))

(defun iregister--jump-to-marker ()
  "Show minibuffer with a text arround current marker. Marker
retrieves from the registers."
  (when (= (iregister-elements-with-markers-length) 0)
    (message "No more registers."))
  (when (> (iregister-elements-with-markers-length) 0)
    (let* ((register-element (car (nthcdr iregister-current-marker-register
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
       iregister-minibuffer-marker-keymap)
      (when (equal iregister-action 'jump)
        (register-to-point register-name))
      (when (or (equal iregister-action 'next)
                (equal iregister-action 'previous))
        (iregister--jump-to-marker))
      (setq iregister-action nil))))

;; Interactive registers with text

(defun iregister-delete-text-register ()
  "Deletes current text marker from the registers."
  (interactive)
  (let* ((register-element (car (nthcdr iregister-current-text-register
                                        (iregister-elements-with-strings))))
         (register-name (car register-element)))
    (set-register register-name nil)
    (iregister-next-text)))

(defun iregister-elements-with-strings ()
  "Retrieves all elements with strings from the registers."
  (interactive)
  (let ((result (list)))
    (dolist (item register-alist)
      (when (stringp (cdr item))
        (setq result (cons item result))))
    result))

(defun iregister-elements-with-strings-length ()
  "Length of the list of registers which contains a string."
  (length (iregister-elements-with-strings)))

(defun iregister-elements-with-strings-length-1 ()
  (- (iregister-elements-with-strings-length) 1))

;;;###autoload
(defun iregister-copy-to-register (start end &optional delete-flag)
  "Copy region into the any empty register. With a `C-u' prefix
argument delete selected text. With a `C-u C-u' prefix argument
kill selected text. With a `C-u C-u C-u' prefix argument
`kill-ring-save' selected text."
  (interactive "r\nP")
  (let ((idx 0)
        (stored nil))
    (while (and (< idx 256)
                (null stored))
      (when (null (get-register idx))
        (setq stored t)
        (setq iregister-last-used-register idx)
        (copy-to-register idx start end)
        (when (equal delete-flag '(4))
          (delete-region start end))
        (when (equal delete-flag '(16))
          (kill-region start end))
        (when (equal delete-flag '(64))
          (kill-ring-save start end)))
      (setq idx (+ idx 1)))))

;;;###autoload
(defun iregister-delete-copy-to-register (start end)
  "Copy region into the any empty register and delete the region."
  (interactive "r")
  (iregister-copy-to-register start end '(4)))

;;;###autoload
(defun iregister-kill-copy-to-register (start end)
  "Copy region into the any empty register and kill the region."
  (interactive "r")
  (iregister-copy-to-register start end '(16)))

;;;###autoload
(defun iregister-append-to-latest-register (start end &optional delete-flag)
  "Append selected text to the latest used register in the
`iregister-copy-to-register' function. With a `C-u' prefix
argument delete selected text. With a `C-u C-u' prefix argument
kill selected text."
  (interactive "r\nP")
  (if (region-active-p)
      (if iregister-last-used-register
          (progn
            (append-to-register iregister-last-used-register
                                start end)
            (when (equal delete-flag '(4))
              (delete-region start end))
            (when (equal delete-flag '(16))
              (kill-region start end)))
        (message "No registers has been used within iregister.el."))
    (message "Region is not active.")))

;;;###autoload
(defun iregister-delete-append-to-latest-register (start end)
  "Append selected text to the latest used register in the
`iregister-copy-to-register' function. Delete selected text."
  (interactive "r")
  (iregister-append-to-latest-register start end '(4)))

;;;###autoload
(defun iregister-kill-append-to-latest-register (start end)
  "Append selected text to the latest used register in the
`iregister-copy-to-register' function. Kill selected text."
  (interactive "r")
  (iregister-append-to-latest-register start end '(16)))

;;;###autoload
(defun iregister-next-text ()
  "If the minibuffer is the current buffer then jump to the next
text.  Otherwise show the minibuffer with the next text and allows
to select interactively required text.  Texts retrieves from the
registers."
  (interactive)
  (setq iregister-current-text-register (+ iregister-current-text-register 1))
  (when (> iregister-current-text-register (iregister-elements-with-strings-length-1))
    (setq iregister-current-text-register 0))
  (if (minibufferp)
      (progn
        (setq iregister-action 'next)
        (exit-minibuffer))
    (iregister-text)))

;;;###autoload
(defun iregister-previous-text ()
  "If the minibuffer is the current buffer then jump to the
previous text.  Otherwise show the minibuffer with the previous text
and allows to select interactively required text. Texts retrieves
from the registers."
  (interactive)
  (setq iregister-current-text-register (- iregister-current-text-register 1))
  (when (< iregister-current-text-register 0)
    (setq iregister-current-text-register (iregister-elements-with-strings-length-1)))
  (if (minibufferp)
      (progn
        (setq iregister-action 'previous)
        (exit-minibuffer))
    (iregister-text)))

;;;###autoload
(defun iregister-text ()
  "Show the minibuffer with the current text."
  (interactive)
  (when (= (iregister-elements-with-strings-length) 0)
    (message "No more registers with strings."))
  (when (> (iregister-elements-with-strings-length) 0)
    (let* ((register-element (car (nthcdr iregister-current-text-register
                                          (iregister-elements-with-strings))))
           (register-name (car register-element))
           (register-string (cdr register-element)))
      (read-from-minibuffer
       ""
       register-string
       iregister-minibuffer-text-keymap)
      (when (equal iregister-action 'append)
        (append-to-register register-name (region-beginning) (region-end))
        (iregister-text))
      (when (equal iregister-action 'prepend)
        (prepend-to-register register-name (region-beginning) (region-end))
        (iregister-text))
      (when (equal iregister-action 'insert)
        (insert register-string))
      (when (or (equal iregister-action 'next)
                (equal iregister-action 'previous))
        (iregister-text))
      (setq iregister-action nil))))

(defun iregister-append-text ()
  "Append selected text to the current text register."
  (interactive)
  (setq iregister-action 'append)
  (exit-minibuffer))

(defun iregister-prepend-text ()
  "Prepend selected text to the current text register."
  (interactive)
  (setq iregister-action 'prepend)
  (exit-minibuffer))

(defun iregister-insert-text ()
  "Insert to the buffer a text from the current text register."
  (interactive)
  (setq iregister-action 'insert)
  (exit-minibuffer))

(defun iregister-save-text-to-register ()
  "Save minibuffer contents to the current register."
  (interactive)
  (let ((register-element (car (nthcdr iregister-current-text-register
                                       (iregister-elements-with-strings)))))
    (set-register (car register-element)
                  (buffer-substring (point-min) (point-max)))))


(provide 'iregister)

;;; iregister.el ends here

