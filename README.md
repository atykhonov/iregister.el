# iregister

### Summary

iregister - Interactive register commands for Emacs.

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
