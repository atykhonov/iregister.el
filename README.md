# iregister

### Summary

iregister - Interactive register commands for Emacs.

This package is built on top of register.el package and it allows to use registers
interactively.

###### Jump to the markers (stored in the registers) interactivelly.

Assuming that there are already stored some points (by means of `point-to-register`
or `iregister-point-to-register` command) in the registers. Execute, for example,
`iregister-jump-to-next-marker` and the minibuffer will display the snippet of the
stored point's buffer. That snippet will contain the text arround of stored point
which allows to figure out whether is that right place to be jumped in or not. If
yes, then just hit the `RET` and the right buffer will be displayed and the point
will be in the same place as it was stored before. If no, then try to hit `n` key or
`p` key to view next/previous markers (points) previously stored in the registers. In
the meantime, in the minibuffer, you could hit `d` key to delete current point from
the register. To quit from the minibuffer press `q` key (or C-g).

Optionally you could use `iregister-point-to-register` command from any buffer to
store current point to register. That command works exactly as `point-to-register`
command but without any prompt, it just finds any empty register and stores there
current point.

###### Insert (append/prepend) interactivelly the texts stored in the registers.

Assuming that there are already stored some texts (by means of `copy-to-register` or
`iregister-copy-to-register` command) in the registers. Execute, for example,
`iregister-next-text` and the minibuffer will display the text stored in some
register. In the appeared minibuffer you can figure out whether is that right text to
be inserted or not. If yes, then just hit the `RET` and the text will be inserted. If
no, then try to hit `n` key or `p` key to view next/previous texts previously stored
in the registers. In the meantime, in the minibuffer, you could hit `d` key to delete
current text from the register. To quit from the minibuffer press `q` key (or
C-g). Also you could use `a` key for appending or `A` key for prepending selected
text to the current text registry.

Optionally you could use `iregister-copy-to-register` command from any buffer to
store selected text to a register. That command works exactly as `copy-to-register`
command but without any prompt, it just finds any empty register and stores there
selected text.

### Installation

Assuming that the file `iregister.el` is somewhere on the load path, add the
following lines to your `.emacs` file:

(require 'iregister)
(global-set-key (kbd "M-n") 'iregister-jump-to-next-marker)
(global-set-key (kbd "M-p") 'iregister-jump-to-previous-marker)
(global-set-key (kbd "M-u") 'iregister-point-or-text-to-register)

If region is active then `iregister-point-or-text-to-register` command stores a
text to any empty register, otherwise it stores a point.

(global-set-key (kbd "M-l") 'iregister-text)

Change the key bindings to your liking.

### Contribution

All contributions are much welcome and appreciated!

##################################################################################

This package is built on top of register.el package and it allows to use registers
interactively. For now it does wrap only functionality which relates to points
(`register-to-point`, `point-to-register`).

Assuming that there are already stored (by means of `point-to-register`) some points
in the registers (in current Emacs session). Execute, for example,
`iregister-jump-to-next-marker` and the minibuffer will display the snippet of the
stored point's buffer. That snippet will contain the text arround of stored point
which allows to figure out whether is that right place to be jumped in or not. If
yes, then just hit the `RET` and the right buffer will be displayed and the point
will be in the same place as it was stored before. If no, then try to hit `M-n`
(`iregister-jump-to-next-marker`) or `M-p` (`iregister-jump-to-previous-marker`) to
view next/previous markers (points) previously stored in the registry. In the
meantime, in the minibuffer, you could hit `d` key to delete current point from the
register. To quit from the minibuffer press `q` key.

Optionally you could use `iregister-point-to-register` command from any buffer to
store current point to register. That command executes without any prompt, it just
finds any empty register and stores there current point.

### Installation

Assuming that the file `iregister.el` is somewhere on the load path, add the
following lines to your `.emacs` file:

```
(require 'iregister)
(global-set-key (kbd "M-n") 'iregister-jump-to-next-marker)
(global-set-key (kbd "M-p") 'iregister-jump-to-previous-marker)
(global-set-key (kbd "M-u") 'iregister-point-to-register)
```

Change the key bindings to your liking.

### TODO

* Implement interactive functions of all other functions from register.el (not only
  which relate to point)

### Contribution

All contributions are much welcome and appreciated!
