# miking-emacs
This repo contains an Emacs mode for editing
[Miking](https://github.com/miking-lang/miking/) code
(MCore/MLang). Pull requests and issue reports are welcome!

To use the mode, add the following to your `init.el` file:

```
;; MCore mode
(add-to-list 'load-path "/path/to/miking-emacs/")
(require 'mcore-mode)
(require 'miking-syn-mode)
```

(or run `M-x eval-buffer` in the file defining the mode)

## Tree-sitter support

`miking-emacs` supports parser-based syntax highlighting based on the new tree-sitter support in Emacs 29.
To utilize this functionality, first make sure your Emacs version is at least 29.0.60 and that `(treesit-available-p)` returns `t`.
Then, build the `tree-sitter-miking` grammar by following the instructions [here](https://git.sr.ht/~aathn/tree-sitter-miking).
Move the resulting `libtree-sitter-mlang.so` file to `$HOME/.emacs.d/tree-sitter/`, and you're good to go!
The advanced syntax highlighting will be enabled automatically, and you can control the level of fontification using `treesit-font-lock-level`.
For instance,

```elisp
(setq-default treesit-font-lock-level 3)
```

will set a moderate level of fontification (values between 1-4 are possible).

You can also explore the syntax tree of an MCore file using the `treesit-explore-mode` command, or get a mode line indication of your position in the tree using `treesit-inspect-mode`.

## Companion Packages

- [Ctags support](https://github.com/miking-lang/miking-ctags)
