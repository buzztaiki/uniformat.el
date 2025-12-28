# uniformat.el

uniformat.el provides functions for formatting buffer and region in a uniform way.

## Usage

Add your favorite formatter for each major mode to `uniformat-mode-formatters`:

```elisp
(add-to-list 'uniformat-mode-formatters '(terraform-mode . terraform-format-buffer))

(reformatter-define my/ruff-format :program "ruff" :args '("format" "-"))
(add-to-list 'uniformat-mode-formatters '(python-mode my/ruff-format-buffer my/ruff-format-region))
```

When you run `M-x uniformat-buffer` or `M-x uniformat-region` in a configured mode, the corresponding formatter will be called.
If Eglot is enabled and a language server provides a formatter, `eglot-format-buffer` and `eglot-code-action-organize-imports` take precedence, and the formatter configured in `uniformat-mode-formatters` will not be called.


Enabling `uniformat-buffer-on-save-mode` or `global-uniformat-buffer-on-save-mode` will automatically format the buffer upon saving:

```elisp
(global-uniformat-buffer-on-save-mode 1)
```


## License
GPLv3
