# uniform.el

uniform.el provides functions for formatting buffer and region in a uniform way.

## Usage

Add your favorite formatter for each major mode to `uniform-mode-formatters`:

```elisp
(add-to-list 'uniform-mode-formatters '(terraform-mode . terraform-format-buffer))

(reformatter-define my/ruff-format :program "ruff" :args '("format" "-"))
(add-to-list 'uniform-mode-formatters '(python-mode my/ruff-format-buffer my/ruff-format-region))
(add-to-list 'uniform-mode-formatters '(python-ts-mode my/ruff-format-buffer my/ruff-format-region))
```

When you run `M-x uniform-format-buffer` or `M-x uniform-format-region` in a configured mode, the corresponding formatter will be called.
If Eglot is enabled and a language server provides a formatter, `eglot-format-buffer` and `eglot-code-action-organize-imports` take precedence, and the formatter configured in `uniform-mode-formatters` will not be called.


Enabling `uniform-format-buffer-on-save-mode` or `global-uniform-format-buffer-on-save-mode` will automatically format the buffer upon saving:

```elisp
(global-uniform-format-buffer-on-save-mode 1)
```


## License
GPLv3
