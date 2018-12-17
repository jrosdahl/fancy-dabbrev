;;; fancy-dabbrev.el --- dabbrev-expand with preview and popup menu
;;
;; Copyright (C) 2018 Joel Rosdahl
;;
;; Author: Joel Rosdahl <joel@rosdahl.net>
;; License: BSD-3-clause
;; Package-Requires: (popup)
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
;; fancy-dabbrev acts very similar to the Emacs built-in dabbrev-expand command
;; but with two improvements:
;;
;; 1. Preview: If fancy-dabbrev-mode is enabled, a preview of the first
;;    expansion candidate will be shown when any text has been entered. If
;;    fancy-dabbrev then is called, the candidate will be expanded. The preview
;;    will only be shown if no other text (except whitespace) is to the right
;;    of the cursor.
;;
;; 2. Popup menu: The first call to fancy-dabbrev will expand the entered word
;;    prefix just like dabbrev-expand. But the second call will show a popup
;;    menu with other candidates (with the second candidate selected). The
;;    third call will advance to the third candidate, etc. It is also possible
;;    to go back to a previous candidate by calling fancy-dabbrev-backward.
;;    Selection from the menu can be canceled with C-g. Any cursor movement or
;;    typing will hide the menu again.
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
;;   ;; Enable fancy-dabbrev previews in all modes:
;;   (global-fancy-dabbrev-mode)
;;
;;   ;; Bind fancy-dabbrev and fancy-dabbrev-backward to your keys of choice:
;;   (global-set-key (kbd "TAB") 'fancy-dabbrev)
;;   (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
;;
;;
;; CONFIGURATION
;; =============
;;

;; Here are the variables that affect fancy-dabbrev's behavior:
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
;; * fancy-dabbrev-no-expansion-for (default: '(multiple-cursors-mode))
;;
;;   A list of variables which, if bound and non-nil, will inactivate
;;   fancy-dabbrev expansion. The variables typically represent major or minor
;;   modes. When inactive, fancy-dabbrev will fall back to running
;;   dabbrev-expand.
;;
;; * fancy-dabbrev-no-preview-for (default: '(iedit-mode isearch-mode multiple-cursors-mode))
;;
;;   A list of variables which, if bound and non-nil, will inactivate
;;   fancy-dabbrev preview. The variables typically represent major or minor
;;   modes.
;;
;; * fancy-dabbrev-commands (default: '(fancy-dabbrev fancy-dabbrev-backward))
;;
;;   A list of commands after which fancy-dabbrev should continue to the next
;;   expansion candidate. If you write a command of your own that wraps
;;   fancy-dabbrev or fancy-dabbrev-backward, add it to this list to maintain
;;   fancy-dabbrev functionality.

(require 'cl)
(require 'dabbrev)
(require 'popup)

(defgroup fancy-dabbrev nil
  "Fancy dabbrev"
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-menu-height 10
  "How many expansion candidates to show in the menu."
  :type 'integer
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-preview-delay 0.2
  "How long to wait until displaying the preview after
keystroke."
  :type 'float
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-no-expansion-for
  '(multiple-cursors-mode)
  "A list of variables which, if bound and non-nil, will
inactivate `fancy-dabbrev' expansion. The variables typically
represent major or minor modes. When inactive, `fancy-dabbrev'
will fall back to running `dabbrev-expand'."
  :type '(repeat variable)
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-no-preview-for
  '(iedit-mode isearch-mode multiple-cursors-mode)
  "A list of variables which, if bound and non-nil, will
inactivate `fancy-dabbrev' preview. The variables typically
represent major or minor modes."
  :type '(repeat variable)
  :group 'fancy-dabbrev)

(defcustom fancy-dabbrev-commands
  '(fancy-dabbrev fancy-dabbrev-backward)
  "A list of commands after which `fancy-dabbrev' should continue
to the next expansion candidate. If you write a command of your
own that wraps `fancy-dabbrev' or `fancy-dabbrev-backward', add
it to this list to maintain `fancy-dabbrev' functionality."
  :type '(repeat function)
  :group 'fancy-dabbrev)

(defvar fancy-dabbrev--popup nil)
(defvar fancy-dabbrev--expansions nil)
(defvar fancy-dabbrev--selected-expansion nil)
(defvar fancy-dabbrev--entered-abbrev nil)
(defvar fancy-dabbrev--preview-overlay nil)
(defvar fancy-dabbrev--preview-timer nil)

(defface fancy-dabbrev--menu-face
  '((t (:inherit popup-face)))
  "Face for fancy-dabbrev menu.")

(defface fancy-dabbrev--selection-face
  '((t (:inherit popup-menu-selection-face)))
  "Face for the selected item in the fancy-dabbrev menu.")

(defface fancy-dabbrev--preview-face
  '((t (:foreground "#777" :underline t)))
  "Face for the preview.")

(defun fancy-dabbrev ()
  "Executes `dabbrev-expand' when called the first time.
Seqsequent calls will execute `dabbrev-expand' while showing a
popup menu with the expansion candidates."
  (interactive)
  (if (or (fancy-dabbrev--is-fancy-dabbrev-command last-command)
          (fancy-dabbrev--looking-at-possibly-expandable))
      (condition-case exit
          (progn (fancy-dabbrev--expand)
                 t)
        ('error nil))
    nil))

(defun fancy-dabbrev-backward ()
  "If run after `fancy-dabbrev', select the previous expansion
candidate in the menu."
  (interactive)
  (if (null fancy-dabbrev--expansions)
      (error "No previous expansion candidate")
    (fancy-dabbrev--expand-again nil)))

(defmacro fancy-dabbrev--with-suppressed-message (&rest body)
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(defun fancy-dabbrev--looking-at-possibly-expandable ()
  ;; TODO: check for symbol according to mode instead?
  (looking-back "[A-Za-z0-9_-]"))

(defun fancy-dabbrev--is-fancy-dabbrev-command (command)
  (memq command fancy-dabbrev-commands))

(defun fancy-dabbrev--any-bound-and-true (variables)
  (some (lambda (x) (and (boundp x) (symbol-value x))) variables))

(defun fancy-dabbrev--expand ()
  (if (fancy-dabbrev--any-bound-and-true fancy-dabbrev-no-expansion-for)
      (dabbrev-expand nil)
    (add-hook 'post-command-hook 'fancy-dabbrev--post-command-hook)
    (if (fancy-dabbrev--is-fancy-dabbrev-command last-command)
        (fancy-dabbrev--expand-again t)
      (fancy-dabbrev--expand-first-time))))

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
             (memq this-command '(self-insert-command)))
    (setq fancy-dabbrev--preview-timer
          (run-with-idle-timer
           fancy-dabbrev-preview-delay nil 'fancy-dabbrev--preview))))

(defun fancy-dabbrev--abbrev-start-location ()
  (- dabbrev--last-abbrev-location (length fancy-dabbrev--entered-abbrev)))

(defun fancy-dabbrev--insert-expansion (expansion)
  (delete-region (fancy-dabbrev--abbrev-start-location) (point))
  (insert expansion))

(defun fancy-dabbrev--get-first-expansion ()
  (dabbrev--reset-global-variables)
  (setq fancy-dabbrev--entered-abbrev (dabbrev--abbrev-at-point))
  (dabbrev--find-expansion
   fancy-dabbrev--entered-abbrev 0 dabbrev-case-fold-search))

(defun fancy-dabbrev--preview ()
  (when (and (fancy-dabbrev--looking-at-possibly-expandable)
             (looking-at "[[:space:]]*$"))
    (let ((expansion
           (fancy-dabbrev--with-suppressed-message
            (fancy-dabbrev--get-first-expansion))))
      (when expansion
        (let ((expansion
               (substring expansion (length fancy-dabbrev--entered-abbrev))))
          (setq fancy-dabbrev--preview-overlay (make-overlay (point) (point)))
          (add-text-properties 0 1 '(cursor 1) expansion)
          (add-text-properties
           0 (length expansion) '(face fancy-dabbrev--preview-face) expansion)
          (overlay-put
           fancy-dabbrev--preview-overlay 'after-string expansion))))))

(defun fancy-dabbrev--expand-first-time ()
  (let* ((expansion (fancy-dabbrev--get-first-expansion)))
    (if (null expansion)
        (error "No expansion found for \"%s\"" fancy-dabbrev--entered-abbrev)
      (setq fancy-dabbrev--expansions (list expansion))
      (fancy-dabbrev--insert-expansion expansion))))

(defun fancy-dabbrev--expand-again (next)
  (fancy-dabbrev--init-expansions)
  (unless fancy-dabbrev--popup
    (setq fancy-dabbrev--popup
          (popup-create (fancy-dabbrev--abbrev-start-location)
                        (apply 'max (mapcar 'length fancy-dabbrev--expansions))
                        fancy-dabbrev-menu-height
                        :around t
                        :face 'fancy-dabbrev--menu-face
                        :selection-face 'fancy-dabbrev--selection-face))
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
          expansion)
      (while (and (< i fancy-dabbrev-menu-height)
                  (setq expansion
                        (dabbrev--find-expansion
                         fancy-dabbrev--entered-abbrev
                         0
                         dabbrev-case-fold-search)))
        (setq fancy-dabbrev--expansions
              (cons expansion fancy-dabbrev--expansions))
        (setq i (1+ i))))
    (setq fancy-dabbrev--expansions (reverse fancy-dabbrev--expansions))
    (setq fancy-dabbrev--selected-expansion 0)))

(defun fancy-dabbrev--on-exit ()
  (when fancy-dabbrev--popup
    (popup-delete fancy-dabbrev--popup)
    (setq fancy-dabbrev--popup nil)
    (setq fancy-dabbrev--expansions nil)
    (setq fancy-dabbrev--selected-expansion nil)))

(defadvice keyboard-quit (before fancy-dabbrev--expand activate)
  (when (fancy-dabbrev--is-fancy-dabbrev-command last-command)
    (fancy-dabbrev--insert-expansion fancy-dabbrev--entered-abbrev)))

(define-minor-mode fancy-dabbrev-mode
  "fancy dabbrev mode"
  :lighter " FD"
  :init-value nil
  (if fancy-dabbrev-mode
      (progn
        (add-hook 'pre-command-hook 'fancy-dabbrev--pre-command-hook nil t)
        (add-hook 'post-command-hook 'fancy-dabbrev--post-command-hook nil t))
    (remove-hook 'pre-command-hook 'fancy-dabbrev--pre-command-hook t)
    (remove-hook 'post-command-hook 'fancy-dabbrev--post-command-hook t)))

(defun turn-on-fancy-dabbrev-mode ()
  (fancy-dabbrev-mode 1))

;;;###autoload
(define-globalized-minor-mode global-fancy-dabbrev-mode
  fancy-dabbrev-mode
  turn-on-fancy-dabbrev-mode)

(provide 'fancy-dabbrev)
