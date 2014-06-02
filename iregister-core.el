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



(defcustom iregister-max-mini-window-height 0.75
  "Maximum height for resizing mini-windows (the minibuffer and the echo area).
If nil, maximum height depends from `max-mini-window-height'
variable. If a float, it specifies a fraction of the mini-window
frame's height. If an integer, it specifies a number of lines.

Actually, when none nil, iregister sets specified value to the
`max-mini-window-height' variable and after minibuffer exit
returns original value."
  :group 'iregister-core
  :type '(integer))

(defvar iregister-min-register 128
  "Codes 0 through 127 are ASCII codes; the rest are
non-ASCII. Let's use for the registers storing only non-ASCII
codes as ASCII codes may be used by a user so it would be better
do not intersect.")

(defvar iregister-max-register 4194303
  "Characters in strings and buffers are currently limited to the
range of 0 to 4194303. As registers stores as a character let's
limit registers to max character value.")

(defvar iregister-minibuffer-position nil
  "Temp variable which holds a point position to which it is
required to jump.")

(defvar iregister-action nil
  "Temp variable to allow to take right action after minibuffer
exit.")

(defvar iregister-last-used-register nil
  "The last used register.")

(defvar iregister-max-mini-window-height-orig nil
  "Preserves value of `max-mini-window-height' to restore after
minibuffer exit.")

(defun iregister-minibuffer-setup-hook ()
  "Setup hook to be triggered after entering minibuffer."
  (interactive)
  (when iregister-max-mini-window-height
    (setq iregister-max-mini-window-height-orig max-mini-window-height)
    (setq max-mini-window-height iregister-max-mini-window-height)))

(defun iregister-minibuffer-exit-hook ()
  "Exit hook to be triggered after exit minibuffer."
  (interactive)
  (iregister--restore-max-mini-window-height))

(defun iregister--restore-max-mini-window-height ()
  (when (and iregister-max-mini-window-height
             iregister-max-mini-window-height-orig)
    (setq iregister-max-mini-window-height-orig nil)
    (setq max-mini-window-height iregister-max-mini-window-height-orig)))

(defun iregister-remove-hooks ()
  "Remove hooks add by iregister."
  (interactive)
  (remove-hook 'minibuffer-setup-hook 'iregister-minibuffer-setup-hook)
  (remove-hook 'minibuffer-setup-hook 'iregister-text-minibuffer-setup-hook)
  (remove-hook 'minibuffer-setup-hook 'iregister-point-minibuffer-setup-hook)
  (remove-hook 'minibuffer-exit-hook 'iregister-minibuffer-exit-hook))

(defun iregister-minibuffer-keyboard-quit ()
  "A thin wrapper arround `minibuffer-keyboard-quit'."
  (interactive)
  (iregister-remove-hooks)
  (iregister--restore-max-mini-window-height)
  (minibuffer-keyboard-quit))

(defun iregister-exit-minibuffer ()
  (interactive)
  (iregister-remove-hooks)
  (iregister--restore-max-mini-window-height)
  (exit-minibuffer))

(defun iregister-next-free-register ()
  "Return next free (empty) register."
  (let ((idx (if iregister-last-used-register
                 (+ iregister-last-used-register 1)
               iregister-min-register))
        (free-register nil))
    (while (and (< idx iregister-max-register)
                (null free-register))
      (when (null (get-register idx))
        (setq free-register idx))
      (setq idx (+ idx 1)))
    free-register))


(provide 'iregister-core)
;;; iregister-core.el ends here
