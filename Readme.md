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
	(autoload #´gtags-mode "gtags-mode" nil t))
(add-hook 'emacs-startup-hook #´gtags-mode)
```

There are only 3 extra commands that the user may need to know:

- **gtags-mode** : To enable the global minor mode.
- **gtags-mode-create** : To create a gtags database in case it doesn't exist for the current project.
- **gtags-mode-update** : To manually update an existent database; specially useful if the project has been modified outside emacs.

Configuration
-------------

This packages tries to do its best to not require user configuration.
The package provides a minimal set of configuration options in case
the global/gtags executable are not in the usual locations, so the
user can set them. **gtags-mode-global-executable**
**gtags-mode-gtags-executable** are buffer local configuration options
to set the path or names in case the user needs it or the user setup
is do special that the command *executable-find* fails searching.

TRAMP users can use *connection-local-variables* to set these values
per individual hosts or users if needed.

The custom variable **gtags-mode-lighter** can be used to change the
default mode-line message to use when the mode is enabled.

