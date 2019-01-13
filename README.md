fancy-dabbrev
=============

fancy-dabbrev essentially wraps Emacs's standard [dabbrev-expand] command, with
two improvements:

1. **Preview**: If `fancy-dabbrev-mode` is enabled, a preview of the first
   expansion candidate will be shown when any text has been entered. If
   `fancy-dabbrev` then is called, the candidate will be expanded.
2. **Popup menu**: The first call to `fancy-dabbrev` will expand the entered
   word prefix just like `dabbrev-expand`. But the second call will show a
   popup menu with other candidates (with the second candidate selected). The
   third call will advance to the third candidate, etc. It is also possible to
   go back to a previous candidate by calling `fancy-dabbrev-backward`.
   Selection from the menu can be canceled with `C-g`. Any cursor movement or
   typing will hide the menu again.


Example
-------

After typing "defi":

![Example 1](fancy-dabbrev-1.png)

After pressing TAB (assuming it is bound to `fancy-dabbrev`):

![Example 2](fancy-dabbrev-2.png)

After pressing TAB a second time:

![Example 3](fancy-dabbrev-3.png)

After pressing TAB a third time:

![Example 4](fancy-dabbrev-4.png)

After pressing space:

![Example 5](fancy-dabbrev-5.png)


Installation
------------

To load `fancy-dabbrev`, store `fancy-dabbrev.el` in your Emacs load path and
put something like this in your Emacs configuration file:

    ;; Load fancy-dabbrev.el:
    (require 'fancy-dabbrev)

    ;; Enable fancy-dabbrev previews in all modes:
    (global-fancy-dabbrev-mode)

    ;; Bind fancy-dabbrev and fancy-dabbrev-backward to your keys of choice:
    (global-set-key (kbd "TAB") 'fancy-dabbrev)
    (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)

    ;; If you want TAB to indent the line like it usually does when the cursor
    ;; is not next to an expandable word, use 'fancy-dabbrev-or-indent instead:
    (global-set-key (kbd "TAB") 'fancy-dabbrev-or-indent)


Configuration
-------------

Here are the variables that affect `fancy-dabbrev`'s behavior:

* `fancy-dabbrev-menu-height` (default: `10`)

  How many expansion candidates to show in the menu.

* `fancy-dabbrev-preview-delay` (default: `0.2`)

  How long (in seconds) to wait until displaying the preview after a keystroke.

* `fancy-dabbrev-preview-context` (default: `'at-eol`)

  When to show the preview. If `'at-eol`, only show the preview if no other
  text (except whitespace) is to the right of the cursor. If
  `'before-non-word`, show the preview whenever the cursor is not immediately
  before (or inside) a word. If `'everywhere`, always show the preview after
  typing.

* `fancy-dabbrev-no-expansion-for` (default: `'(multiple-cursors-mode)`)

  A list of variables which, if bound and non-nil, will inactivate
  `fancy-dabbrev` expansion. The variables typically represent major or minor
  modes. When inactive, `fancy-dabbrev` will fall back to running
  `dabbrev-expand`.

* `fancy-dabbrev-no-preview-for` (default:
  `'(iedit-mode isearch-mode multiple-cursors-mode)`)

  A list of variables which, if bound and non-nil, will inactivate
  `fancy-dabbrev` preview. The variables typically represent major or minor
  modes.

Why?
----

There are many other Emacs packages for doing more or less advanced
auto-completion in different ways. After trying out some of the more popular
ones and not clicking with them, I kept coming back to `dabbrev-expand` due to
its simplicity. Since I missed the preview feature and a way of selecting
expansions candidates from a menu if the first candidate isn't the right one, I
wrote `fancy-dabbrev`.

Have fun!

/Joel Rosdahl <joel@rosdahl.net>

[dabbrev-expand]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dynamic-Abbrevs.html
