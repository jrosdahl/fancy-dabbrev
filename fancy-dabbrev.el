;;; fancy-dabbrev.el --- Like dabbrev-expand with preview and popup menu -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018-2019 Joel Rosdahl
;;
;; Author: Joel Rosdahl <joel@rosdahl.net>
;; Version: 1.0
;; License: BSD-3-clause
;; Package-Requires: ((emacs "24") (popup "0.5.3"))
;; URL: https://github.com/jrosdahl/fancy-dabbrev
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; * Neither the name of the copyright holder nor the names of its contributors
;;   may be used to endorse or promote products derived from this software
;;   without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
;;
;; ============================================================================
;;
;;; Commentary:
;;
;; fancy-dabbrev essentially wraps the Emacs built-in dabbrev functionality,
;; with two improvements:
;;
;; 1. Preview: If fancy-dabbrev-mode is enabled, a preview of the first
;;    expansion candidate will be shown when any text has been entered. If
;;    fancy-dabbrev-expand then is called, the candidate will be expanded.
;;
;; 2. Popup menu: The first call to fancy-dabbrev-expand will expand the
;;    entered word prefix just like dabbrev-expand. But the second call will
;;    show a popup menu with other candidates (with the second candidate
;;    selected). The third call will advance to the third candidate, etc. It is
;;    also possible to go back to a previous candidate by calling
;;    fancy-dabbrev-backward. Selection from the menu can be canceled with C-g.
;;    Any cursor movement or typing will hide the menu again.
;;
;;
;; INSTALLATION
;; ============
;;
;; To load fancy-dabbrev, store fancy-dabbrev.el in your Emacs load path and
;; put something like this in your Emacs configuration file:
;;
;;   ;; Load fancy-dabbrev.el:
;;   (require 'fancy-dabbrev)
;;
;;   ;; Enable fancy-dabbrev previews everywhere:
;;   (global-fancy-dabbrev-mode)
;;
;;   ;; Bind fancy-dabbrev-expand and fancy-dabbrev-backward to your keys of
;;   ;; choice:
;;   (global-set-key (kbd "TAB") 'fancy-dabbrev-expand)
;;   (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
;;
;;   ;; If you want TAB to indent the line like it usually does when the cursor
;;   ;; is not next to an expandable word, use 'fancy-dabbrev-expand-or-indent
;;   ;; instead:
;;   (global-set-key (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
;;
;;
;; CONFIGURATION
;; ============
;;
;; fancy-dabbrev-expand uses dabbrev-expand under the hood, so most dabbrev-*
;; configuration options affect fancy-dabbrev-expand as well. For instance, if
;; you want to use fancy-dabbrev-expand when programming, you probably want to
;; use these dabbrev settings:
;;
;;   ;; Let dabbrev searches ignore case and expansions preserve case:
;;   (setq dabbrev-case-distinction nil)
;;   (setq dabbrev-case-fold-search t)
;;   (setq dabbrev-case-replace nil)
;;
;; Here are fancy-dabbrev's own configuration options:
;;
;; * fancy-dabbrev-menu-height (default: 10)
;;
;;   How many expansion candidates to show in the menu.
;;
;; * fancy-dabbrev-preview-delay (default: 0.2)
;;
;;   How long (in seconds) to wait until displaying the preview after a
;;   keystroke.
;;
;; * fancy-dabbrev-preview-context (default: 'at-eol)
;;
;;   When to show the preview. If 'at-eol, only show the preview if no other
;;   text (except whitespace) is to the right of the cursor. If
;;   'before-non-word, show the preview whenever the cursor is not immediately
;;   before (or inside) a word. If 'everywhere, always show the preview after
;;   typing.
;;
;; * fancy-dabbrev-no-expansion-for (default: '(multiple-cursors-mode))
;;
;;   A list of variables which, if bound and non-nil, will inactivate
;;   fancy-dabbrev expansion. The variables typically represent major or minor
;;   modes. When inactive, fancy-dabbrev-expand will fall back to running
;;   dabbrev-expand.
;;
;; * fancy-dabbrev-no-preview-for (default: '(iedit-mode isearch-mode
;;   multiple-cursors-mode))
;;
;;   A list of variables which, if bound and non-nil, will inactivate
;;   fancy-dabbrev preview. The variables typically represent major or minor
;;   modes.
;;
;; WHY?
;; ====
;;
;; There are many other Emacs packages for doing more or less advanced
;; auto-completion in different ways. After trying out some of the more popular
;; ones and not clicking with them, I kept coming back to dabbrev due to its
;; simplicity. Since I missed the preview feature and a way of selecting
;; expansions candidates from a menu if the first candidate isn't the right
;; one, I wrote fancy-dabbrev.
;;
;; Have fun!
;;
;; /Joel Rosdahl <joel@rosdahl.net>

;;; Code:

(require 'cl-lib)
(require 'dabbrev)
(require 'popup)

(defgroup fancy-dabbrev nil
  "Fancy dabbrev"
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-menu-height 10
  "How many expansion candidates to show in the menu."
  :type 'integer
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-sort-menu
  nil
  "Determine the order of matching candidates in the popup menu.

If nil, the popup menu will show matching candidates in the order
that repeated calls to ‘dabbrev-expand’ would return (i.e., first
candidates before the cursor, then after the cursor and then from
other buffers). If t, the candidates (except the first one) will
be sorted."
  :type 'boolean
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-preview-delay 0.2
  "How long to wait until displaying the preview after a keystroke.

The value is in seconds."
  :type 'float
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-preview-context
  'at-eol
  "When to show the preview.

If 'at-eol, only show the preview if no other text (except
whitespace) is to the right of the cursor. If 'before-non-word,
show the preview whenever the cursor is not immediately
before (or inside) a word. If 'everywhere, always show the
preview after typing."
  :type '(choice (const :tag "Only at end of lines" at-eol)
                 (const :tag "When cursor is not immediately before a word"
                        before-non-word)
                 (const :tag "Everywhere" everywhere))
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-no-expansion-for
  '(multiple-cursors-mode)
  "Determine when to disable expansion.

This is a list of variables which, if bound and non-nil, will
inactivate fancy-dabbrev expansion. The variables typically
represent major or minor modes. When inactive,
`fancy-dabbrev-expand' will fall back to running
`dabbrev-expand'."
  :type '(repeat variable)
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-no-preview-for
  '(iedit-mode isearch-mode multiple-cursors-mode)
  "Determine when to disable preview.

This is a list of variables which, if bound and non-nil, will
inactivate fancy-dabbrev preview. The variables typically
represent major or minor modes."
  :type '(repeat variable)
  :group 'fancy-dabbrev)

(defconst fancy-dabbrev--commands
  '(fancy-dabbrev-expand
    fancy-dabbrev-expand-or-indent
    fancy-dabbrev-backward))

(defvar fancy-dabbrev-mode nil)
(defvar fancy-dabbrev--popup nil)
(defvar fancy-dabbrev--expansions nil)
(defvar fancy-dabbrev--selected-expansion nil)
(defvar fancy-dabbrev--entered-abbrev nil)
(defvar fancy-dabbrev--preview-overlay nil)
(defvar fancy-dabbrev--preview-timer nil)

(defface fancy-dabbrev-menu-face
  '((t (:inherit popup-face)))
  "Face for the fancy-dabbrev menu.")

(defface fancy-dabbrev-selection-face
  '((t (:inherit popup-menu-selection-face)))
  "Face for the selected item in the fancy-dabbrev menu.")

(defface fancy-dabbrev-preview-face
  '((t (:foreground "#777" :underline t)))
  "Face for the preview.")

;;;###autoload
(defun fancy-dabbrev-expand ()
  "Expand previous word \"dynamically\", potentially with a popup menu.

This function executes `dabbrev-expand' when called the first
time. Seqsequent calls will execute `dabbrev-expand' while
showing a popup menu with the expansion candidates."
  (interactive)
  (unless (fancy-dabbrev--expand)
    (error "No expansion possible here")))

;;;###autoload
(defun fancy-dabbrev-expand-or-indent ()
  "Expand previous word \"dynamically\" or indent.

This function executes `fancy-dabbrev-expand' if the cursor is
after an expandable prefix, otherwise `indent-for-tab-command'."
  (interactive)
  (unless (fancy-dabbrev--expand)
    (indent-for-tab-command)))

;;;###autoload
(defun fancy-dabbrev-backward ()
  "Select the previous expansion candidate.

If run after `fancy-dabbrev-expand', this function selects the
previous expansion candidate in the menu."
  (interactive)
  (if (and fancy-dabbrev--expansions
           (fancy-dabbrev--is-fancy-dabbrev-command last-command))
      (fancy-dabbrev--expand-again nil)
    (setq fancy-dabbrev--expansions nil)
    (error "No previous expansion candidate")))

(defmacro fancy-dabbrev--with-suppressed-message (&rest body)
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun fancy-dabbrev--looking-back-at-expandable ()
  (and (not (bolp))
       (looking-back "[^[:space:]]" nil)
       (thing-at-point 'symbol)))

(defun fancy-dabbrev--in-previewable-context ()
  (cond ((eq fancy-dabbrev-preview-context 'at-eol)
         (looking-at "[[:space:]]*$"))
        ((eq fancy-dabbrev-preview-context 'before-non-word)
         (looking-at "$\\|[^[:word:]_]"))
        ((eq fancy-dabbrev-preview-context 'everywhere)
         t)
        (t nil)))

(defun fancy-dabbrev--is-fancy-dabbrev-command (command)
  (memq command fancy-dabbrev--commands))

(defun fancy-dabbrev--any-bound-and-true (variables)
  (cl-some (lambda (x) (and (boundp x) (symbol-value x))) variables))

(defun fancy-dabbrev--expand ()
  (let ((last-command-did-expand
         (and (fancy-dabbrev--is-fancy-dabbrev-command last-command)
              fancy-dabbrev--expansions)))
    (if (not (or last-command-did-expand
                 (fancy-dabbrev--looking-back-at-expandable)))
        (setq fancy-dabbrev--expansions nil)
      (if (fancy-dabbrev--any-bound-and-true fancy-dabbrev-no-expansion-for)
          (dabbrev-expand nil)
        (add-hook 'post-command-hook #'fancy-dabbrev--post-command-hook)
        (if last-command-did-expand
            (fancy-dabbrev--expand-again t)
          (fancy-dabbrev--expand-first-time)))
      t)))

(defun fancy-dabbrev--pre-command-hook ()
  (when fancy-dabbrev--preview-overlay
    (delete-overlay fancy-dabbrev--preview-overlay)))

(defun fancy-dabbrev--post-command-hook ()
  (when (timerp fancy-dabbrev--preview-timer)
    (cancel-timer fancy-dabbrev--preview-timer))
  (unless (fancy-dabbrev--is-fancy-dabbrev-command this-command)
    (fancy-dabbrev--on-exit))
  (when (and fancy-dabbrev-mode
             (not (fancy-dabbrev--any-bound-and-true
                   fancy-dabbrev-no-preview-for))
             (not (minibufferp (current-buffer)))
             (eq this-command #'self-insert-command))
    (setq fancy-dabbrev--preview-timer
          (run-with-idle-timer
           fancy-dabbrev-preview-delay nil #'fancy-dabbrev--preview))))

(defun fancy-dabbrev--abbrev-start-location ()
  (- dabbrev--last-abbrev-location (length fancy-dabbrev--entered-abbrev)))

(defun fancy-dabbrev--insert-expansion (expansion)
  (delete-region (fancy-dabbrev--abbrev-start-location) (point))
  (insert expansion))

(defun fancy-dabbrev--get-first-expansion ()
  (fancy-dabbrev--with-suppressed-message
    (dabbrev--reset-global-variables)
    (setq fancy-dabbrev--entered-abbrev (dabbrev--abbrev-at-point))
    (dabbrev--find-expansion
     fancy-dabbrev--entered-abbrev 0 dabbrev-case-fold-search)))

(defun fancy-dabbrev--preview ()
  (when (and (fancy-dabbrev--looking-back-at-expandable)
             (fancy-dabbrev--in-previewable-context))
    (let ((expansion (fancy-dabbrev--get-first-expansion)))
      (when expansion
        (let ((expansion
               (substring expansion (length fancy-dabbrev--entered-abbrev))))
          (setq fancy-dabbrev--preview-overlay (make-overlay (point) (point)))
          (add-text-properties 0 1 '(cursor 1) expansion)
          (add-text-properties
           0 (length expansion) '(face fancy-dabbrev-preview-face) expansion)
          (overlay-put
           fancy-dabbrev--preview-overlay 'after-string expansion))))))

(defun fancy-dabbrev--expand-first-time ()
  (setq fancy-dabbrev--expansions nil)
  (let* ((expansion (fancy-dabbrev--get-first-expansion)))
    (if (null expansion)
        (error "No expansion found for \"%s\"" fancy-dabbrev--entered-abbrev)
      (setq fancy-dabbrev--expansions (list expansion))
      (fancy-dabbrev--insert-expansion expansion))))

(defun fancy-dabbrev--get-popup-point ()
  (let ((start (fancy-dabbrev--abbrev-start-location))
        (line-width (- (line-end-position) (line-beginning-position))))
    (if (> line-width (window-width))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun fancy-dabbrev--expand-again (next)
  (fancy-dabbrev--init-expansions)
  (unless fancy-dabbrev--popup
    (setq fancy-dabbrev--popup
          (popup-create
           (fancy-dabbrev--get-popup-point)
           (apply #'max (mapcar #'length fancy-dabbrev--expansions))
           fancy-dabbrev-menu-height
           :around t
           :face 'fancy-dabbrev-menu-face
           :selection-face 'fancy-dabbrev-selection-face))
    (popup-set-list fancy-dabbrev--popup fancy-dabbrev--expansions)
    (popup-draw fancy-dabbrev--popup))
  (let ((diff))
    (if next
        (progn
          (popup-next fancy-dabbrev--popup)
          (setq diff 1))
      (popup-previous fancy-dabbrev--popup)
      (setq diff -1))
    (setq fancy-dabbrev--selected-expansion
          (mod (+ fancy-dabbrev--selected-expansion diff)
               (length fancy-dabbrev--expansions))))
  (fancy-dabbrev--insert-expansion
   (nth fancy-dabbrev--selected-expansion fancy-dabbrev--expansions)))

(defun fancy-dabbrev--init-expansions ()
  (when (= (length fancy-dabbrev--expansions) 1)
    (let ((i 1)
          expansion
          new-expansions)
      (while (and (< i fancy-dabbrev-menu-height)
                  (setq expansion
                        (dabbrev--find-expansion
                         fancy-dabbrev--entered-abbrev
                         0
                         dabbrev-case-fold-search)))
        (setq new-expansions (cons expansion new-expansions))
        (setq i (1+ i)))
      (setq fancy-dabbrev--expansions
            (cons (car fancy-dabbrev--expansions)
                  (if fancy-dabbrev-sort-menu
                      (sort new-expansions
                            (if (fboundp #'string-collate-lessp)
                                #'string-collate-lessp
                              #'string<))
                    (reverse new-expansions))))
      (setq fancy-dabbrev--selected-expansion 0))))

(defun fancy-dabbrev--on-exit ()
  (when fancy-dabbrev--popup
    (popup-delete fancy-dabbrev--popup)
    (setq fancy-dabbrev--popup nil)
    (setq fancy-dabbrev--expansions nil)
    (setq fancy-dabbrev--selected-expansion nil)))

(defadvice keyboard-quit (before fancy-dabbrev--expand activate)
  (when (fancy-dabbrev--is-fancy-dabbrev-command last-command)
    (fancy-dabbrev--insert-expansion fancy-dabbrev--entered-abbrev)))

;;;###autoload
(define-minor-mode fancy-dabbrev-mode
  "Toggle fancy-dabbrev-mode.

With a prefix argument ARG, enable fancy-dabbrev-mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil.

When fancy-dabbrev-mode is enabled, fancy-dabbrev's preview
functionality is activated."
  :lighter " FD"
  :init-value nil
  (if fancy-dabbrev-mode
      (progn
        (add-hook 'pre-command-hook #'fancy-dabbrev--pre-command-hook nil t)
        (add-hook 'post-command-hook #'fancy-dabbrev--post-command-hook nil t))
    (remove-hook 'pre-command-hook #'fancy-dabbrev--pre-command-hook t)
    (remove-hook 'post-command-hook #'fancy-dabbrev--post-command-hook t)))

;;;###autoload
(define-globalized-minor-mode global-fancy-dabbrev-mode
  fancy-dabbrev-mode
  (lambda () (fancy-dabbrev-mode 1)))

(provide 'fancy-dabbrev)

;;; fancy-dabbrev.el ends here
