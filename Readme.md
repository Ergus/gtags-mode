Readme
======

This package provides GNU Global integration with xref, project,
completion-at-point (capf) and imenu in emacs.

There are some other packages with their own approach and set of more
complete/complex features, maps and functionalities; like ggtags,
gtags.el, gxref, agtags and some others. They are listed with a brief
description in: [related sites](https://www.gnu.org/software/global/links.html).

This package let all the work to the EMACS tools available for such
functionalities and avoids external dependencies.  Unlike other
packages; this module does not create extra special maps, bindings or
menus, but just adds support to the mentioned features to use
gtags/global as a backend when possible.  We do special emphasis on
minimalism, simplicity, efficiency and tramp support.

This package may be extended in the future with new features and to
support other tools, but only if they are required and included in
an emacs distribution and don't need external dependencies.

Usage
-----

Just load and enable the mode: `gtags-mode` or call it in a hook as
usual. The mode is a global-minor-mode.

With `use-packages`

```elisp
(use-package gtags-mode
  :hook ((emacs-startup . gtags-mode)))
```
or simply

```elisp
(unless (fboundp 'gtags-mode)
	(autoload #'gtags-mode "gtags-mode" nil t))
(add-hook 'emacs-startup-hook #'gtags-mode)
```

There are only 3 extra commands that the user may need to know:

- `gtags-mode` : To enable the global minor mode.
- `gtags-mode-create` : To create a gtags database in case it doesn't exist for the current project.
- `gtags-mode-update` : To manually update an existent database; specially useful if the project has been modified outside emacs.

Configuration
-------------

This packages tries to do its best to not require user configuration.
The package provides a minimal set of configuration options in case
the global/gtags executable are not in the usual locations, so the
user can set them. `gtags-mode-global-executable`
`gtags-mode-gtags-executable` are buffer local configuration options
to set the path or names in case the user needs it or the user setup
is do special that the command `executable-find` fails searching.

TRAMP users can use
[*connection-local-variable*](https://www.gnu.org/software/emacs/manual/html_node/elisp/Connection-Local-Variables.html)
to set these values per individual hosts or users if needed.

`gtags-mode-features` is the list of enabled features in
gtags-mode. By default all the features: `(project xref completion
imenu hooks)` are enabled. The user only needs to remove one of these
entries and restart `gtags-mode` to disable it.
TODO: Improve the custom type for this variable in order to restrict
possible values.

The custom variable `gtags-mode-lighter` can be used to change the
default mode-line message to use when the mode is enabled.

The verbosity of messages printed can be controlled with
`gtags-mode-verbose-level` higher verbose level implies more
messages and 0 prints no messages at all (not recommended!!). The most
verbose messages are not printed in the echo area, but only in the
`\*Messages\*` buffer.

It is possible to pass extra arguments to update commands using the
variable `gtags-mode-update-args`. Usually these can be used to use
different `global` backends.  For example, to use
[universal-ctags](https://ctags.io/) instead of the default backend,
the Arch Linux configuration is:

```lisp
(setq gtags-mode-update-args "--gtagsconf=/usr/share/gtags/gtags.conf --gtagslabel=universal-ctags")

```

This variable can be set in the
[*.dir-locals*](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
or as a
[*connection-local-variable*](https://www.gnu.org/software/emacs/manual/html_node/elisp/Connection-Local-Variables.html)

