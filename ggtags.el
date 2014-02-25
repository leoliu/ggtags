;;; ggtags.el --- emacs frontend to GNU Global source code tagging system  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2014  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.7.11
;; Keywords: tools, convenience
;; Created: 2013-01-29
;; URL: https://github.com/leoliu/ggtags
;; Package-Requires: ((emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to integrate GNU Global source code tagging system
;; (http://www.gnu.org/software/global) with Emacs.
;;
;; Usage:
;;
;; Type `M-x ggtags-mode' to enable the minor mode, or as usual enable
;; it in your desired major mode hooks. When the mode is on the symbol
;; at point is underlined if it is a valid (definition) tag.
;;
;; `M-.' finds definition or references according to the context at
;; point, i.e. if point is at a definition tag find references and
;; vice versa. `M-]' finds references.
;;
;; If multiple matches are found, navigation mode is entered, the
;; mode-line lighter changed, and a navigation menu-bar entry
;; presented. In this mode, `M-n' and `M-p' moves to next and previous
;; match, `M-}' and `M-{' to next and previous file respectively.
;; `M-o' toggles between full and abbreviated displays of file names
;; in the auxiliary popup window. When you locate the right match,
;; press RET to finish which hides the auxiliary window and exits
;; navigation mode. You can continue the search using `M-,'. To abort
;; the search press `M-*'.
;;
;; Normally after a few searches a dozen buffers are created visiting
;; files tracked by GNU Global. `C-c M-k' helps clean them up.
;;
;; Check the menu-bar entry `Ggtags' for other useful commands.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'url-parse))

(require 'compile)
(require 'etags)
(require 'tabulated-list)               ;preloaded since 24.3

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  (defmacro* when-let ((var exp) &rest body)
    "A macro that combines `let' and `when'."
    (declare (indent 1) (debug ((sexp form) body)))
    `(let ((,var ,exp)) (when ,var ,@body))))

(eval-and-compile
  (or (fboundp 'user-error)
      (defalias 'user-error 'error)))

(defgroup ggtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defface ggtags-highlight '((t (:underline t)))
  "Face used to highlight a valid tag at point."
  :group 'ggtags)

(defface ggtags-global-line '((t (:inherit secondary-selection)))
  "Face used to highlight matched line in Global buffer."
  :group 'ggtags)

(defcustom ggtags-oversize-limit (* 10 1024 1024)
  "The over size limit for the  GTAGS file.
For large source trees, running 'global -u' can be expensive.
Thus when GTAGS file is larger than this limit, ggtags
automatically switches to 'global --single-update'."
  :safe 'numberp
  :type '(choice (const :tag "None" nil)
                 (const :tag "Always" t)
                 number)
  :group 'ggtags)

(defcustom ggtags-project-duration 600
  "Seconds to keep information of a project in memory."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-process-environment nil
  "Similar to `process-environment' with higher precedence.
Elements are run through `substitute-env-vars' before use.
GTAGSROOT will always be expanded to current project root
directory. This is intended for project-wise ggtags-specific
process environment settings. Note on remote hosts (e.g. tramp)
directory local variables is not enabled by default per
`enable-remote-dir-locals' (which see)."
  :safe 'ggtags-list-of-string-p
  :type '(repeat string)
  :group 'ggtags)

(defcustom ggtags-auto-jump-to-first-match t
  "Non-nil to automatically jump to the first match."
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-window-height 8 ; ggtags-global-mode
  "Number of lines for the 'global' popup window.
If nil, use Emacs default."
  :type '(choice (const :tag "Default" nil) integer)
  :group 'ggtags)

(defcustom ggtags-global-abbreviate-filename 35
  "Non-nil to display file names abbreviated e.g. \"/u/b/env\".
If an integer abbreviate only names longer than that number."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 integer)
  :group 'ggtags)

(defcustom ggtags-split-window-function split-window-preferred-function
  "A function to control how ggtags pops up the auxiliary window."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-use-idutils (and (executable-find "mkid") t)
  "Non-nil to also generate the idutils DB."
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-output-format 'grep
  "The output format for the 'global' command."
  :type '(choice (const path)
                 (const ctags)
                 (const ctags-x)
                 (const grep)
                 (const cscope))
  :group 'ggtags)

(defcustom ggtags-global-ignore-case nil
  "Non-nil if Global should ignore case."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-treat-text nil
  "Non-nil if Global should include matches from text files."
  :safe 'booleanp
  :type 'boolean
  :group 'ggtags)

(defcustom ggtags-global-large-output 1000
  "Number of lines in the Global buffer to indicate large output."
  :type 'number
  :group 'ggtags)

(defcustom ggtags-global-next-error-hook nil
  "Hook run immediately after finding a tag."
  :options '(reposition-window recenter)
  :type 'hook
  :group 'ggtags)

(defcustom ggtags-mode-prefix-key "\C-c"
  "Key binding used for `ggtags-mode-prefix-map'.
Users should change the value using `customize-variable' to
properly update `ggtags-mode-map'."
  :set (lambda (sym value)
         (when (bound-and-true-p ggtags-mode-map)
           (when-let (old (and (boundp sym) (symbol-value sym)))
             (define-key ggtags-mode-map old nil))
           (and value
                (bound-and-true-p ggtags-mode-prefix-map)
                (define-key ggtags-mode-map value ggtags-mode-prefix-map)))
         (set-default sym value))
  :type 'key-sequence
  :group 'ggtags)

(defcustom ggtags-completing-read-function completing-read-function
  "Ggtags specific `completing-read-function' (which see)."
  :type 'function
  :group 'ggtags)

(defcustom ggtags-highlight-tag-delay 0.25
  "Time in seconds before highlighting tag at point."
  :set (lambda (sym value)
         (when (bound-and-true-p ggtags-highlight-tag-timer)
           (timer-set-idle-time ggtags-highlight-tag-timer value t))
         (set-default sym value))
  :type 'number
  :group 'ggtags)

(defcustom ggtags-bounds-of-tag-function (lambda ()
                                           (bounds-of-thing-at-point 'symbol))
  "Function to get the start and end locations of the tag at point."
  :type 'function
  :group 'ggtags)

(defvar ggtags-bug-url "https://github.com/leoliu/ggtags/issues")

(defvar ggtags-global-last-buffer nil)

(defvar ggtags-current-tag-name nil)

(defvar ggtags-highlight-tag-overlay nil)

(defvar ggtags-highlight-tag-timer nil)

;; Used by ggtags-global-mode
(defvar ggtags-global-error "match"
  "Stem of message to print when no matches are found.")

(defmacro ggtags-ensure-global-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (or (and (buffer-live-p ggtags-global-last-buffer)
              (with-current-buffer ggtags-global-last-buffer
                (derived-mode-p 'ggtags-global-mode)))
         (error "No global buffer found"))
     (with-current-buffer ggtags-global-last-buffer ,@body)))

(defun ggtags-list-of-string-p (xs)
  "Return non-nil if XS is a list of strings."
  (if (null xs)
      t
    (and (stringp (car xs))
         (ggtags-list-of-string-p (cdr xs)))))

(defun ggtags-process-string (program &rest args)
  (with-temp-buffer
    (let ((exit (apply #'process-file program nil t nil args))
          (output (progn
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n")
                    (buffer-substring (point-min) (point)))))
      (or (zerop exit)
          (error "`%s' non-zero exit: %s" program output))
      output)))

(defun ggtags-tag-at-point ()
  (when-let (bounds (funcall ggtags-bounds-of-tag-function))
    (buffer-substring (car bounds) (cdr bounds))))

;;; Store for project info and settings

(defvar ggtags-projects (make-hash-table :size 7 :test #'equal))

(defstruct (ggtags-project (:constructor ggtags-project--make)
                           (:copier nil)
                           (:type vector)
                           :named)
  root tag-size has-refs has-path-style has-color dirty-p timestamp)

(defun ggtags-make-project (root)
  (check-type root string)
  (when-let (tag-size (nth 7 (file-attributes (expand-file-name "GTAGS" root))))
    (let* ((default-directory (file-name-as-directory root))
           (rtags-size (nth 7 (file-attributes "GRTAGS")))
           (has-refs
            (when rtags-size
              (and (or (> rtags-size (* 32 1024))
                       (with-demoted-errors
                         (not (equal "" (ggtags-process-string "global" "-crs")))))
                   'has-refs)))
           ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1518
           (has-path-style
            (with-demoted-errors        ; in case `global' not found
              (and (zerop (process-file "global" nil nil nil
                                        "--path-style" "shorter" "--help"))
                   'has-path-style)))
           ;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1542
           (has-color
            (with-demoted-errors
              (and (zerop (process-file "global" nil nil nil "--color" "--help"))
                   'has-color))))
      (puthash default-directory
               (ggtags-project--make :root default-directory
                                     :tag-size tag-size
                                     :has-refs has-refs
                                     :has-path-style has-path-style
                                     :has-color has-color
                                     :timestamp (float-time))
               ggtags-projects))))

(defun ggtags-project-expired-p (project)
  (or (< (ggtags-project-timestamp project) 0)
      (> (- (float-time)
            (ggtags-project-timestamp project))
         ggtags-project-duration)))

(defun ggtags-project-oversize-p (&optional project)
  (pcase ggtags-oversize-limit
    (`nil nil)
    (`t t)
    (size (when-let (project (or project (ggtags-find-project)))
            (> (ggtags-project-tag-size project) size)))))

(defvar-local ggtags-project-root 'unset
  "Internal variable for project root directory.")

;;;###autoload
(defun ggtags-find-project ()
  (let ((project (gethash ggtags-project-root ggtags-projects)))
    (if (ggtags-project-p project)
        (if (ggtags-project-expired-p project)
            (progn
              (remhash ggtags-project-root ggtags-projects)
              (ggtags-find-project))
          project)
      (setq ggtags-project-root
            (or (ignore-errors (file-name-as-directory
                                (concat (file-remote-p default-directory)
                                        ;; Resolves symbolic links
                                        (ggtags-process-string "global" "-pr"))))
                ;; 'global -pr' resolves symlinks before checking
                ;; the GTAGS file which could cause issues such as
                ;; https://github.com/leoliu/ggtags/issues/22, so
                ;; let's help it out.
                (when-let (gtags (locate-dominating-file
                                  default-directory
                                  (lambda (dir)
                                    (file-regular-p (expand-file-name "GTAGS" dir)))))
                  ;; `file-truename' may strip the trailing '/' on
                  ;; remote hosts, see http://debbugs.gnu.org/16851
                  (file-name-as-directory (file-truename gtags)))))
      (when ggtags-project-root
        (if (gethash ggtags-project-root ggtags-projects)
            (ggtags-find-project)
          (ggtags-make-project ggtags-project-root))))))

(defun ggtags-current-project-root ()
  (and (ggtags-find-project)
       (ggtags-project-root (ggtags-find-project))))

(defun ggtags-check-project ()
  (or (ggtags-find-project) (error "File GTAGS not found")))

(defun ggtags-ensure-project ()
  (or (ggtags-find-project)
      (when (or (yes-or-no-p "File GTAGS not found; run gtags? ")
                (user-error "Aborted"))
        (call-interactively #'ggtags-create-tags)
        ;; Need checking because `ggtags-create-tags' can create tags
        ;; in any directory.
        (ggtags-check-project))))

(defvar delete-trailing-lines)          ;new in 24.3

(defun ggtags-save-project-settings (&optional noconfirm)
  "Save Gnu Global's specific environment variables."
  (interactive "P")
  (ggtags-check-project)
  (let* ((inhibit-read-only t)          ; for `add-dir-local-variable'
         (default-directory (ggtags-current-project-root))
         ;; Not using `ggtags-with-current-project' to preserve
         ;; environment variables that may be present in
         ;; `ggtags-process-environment'.
         (process-environment
          (append ggtags-process-environment
                  process-environment
                  (and (not (ggtags-project-has-refs (ggtags-find-project)))
                       (list "GTAGSLABEL=ctags"))))
         (envlist (delete-dups
                   (loop for x in process-environment
                         when (string-match
                               "^\\(GTAGS[^=\n]*\\|MAKEOBJDIRPREFIX\\)=" x)
                         ;; May have duplicates thus `delete-dups'.
                         collect (concat (match-string 1 x)
                                         "="
                                         (getenv (match-string 1 x))))))
         (help-form (format "y: save\nn: don't save\n=: diff\n?: help\n")))
    (add-dir-local-variable nil 'ggtags-process-environment envlist)
    ;; Remove trailing newlines by `add-dir-local-variable'.
    (let ((delete-trailing-lines t)) (delete-trailing-whitespace))
    (or noconfirm
        (while (pcase (read-char-choice
                       (format "Save `%s'? (y/n/=/?) " buffer-file-name)
                       '(?y ?n ?= ??))
                 ;; ` required for 24.1 and 24.2
                 (`?n (user-error "Aborted"))
                 (`?y nil)
                 (`?= (diff-buffer-with-file) 'loop)
                 (`?? (help-form-show) 'loop))))
    (save-buffer)
    (kill-buffer)))

(defun ggtags-toggle-project-read-only ()
  (interactive)
  (ggtags-check-project)
  (let ((inhibit-read-only t)           ; for `add-dir-local-variable'
        (val (not buffer-read-only))
        (default-directory (ggtags-current-project-root)))
    (add-dir-local-variable nil 'buffer-read-only val)
    (save-buffer)
    (kill-buffer)
    (when buffer-file-name
      (setq buffer-read-only val))
    (when (called-interactively-p 'interactive)
      (message "Project read-only-mode is %s" (if val "on" "off")))
    val))

(defun ggtags-visit-project-root ()
  (interactive)
  (ggtags-check-project)
  (dired (ggtags-current-project-root)))

(defmacro ggtags-with-current-project (&rest body)
  "Eval BODY in current project's `process-environment'."
  (declare (debug t))
  (let ((gtagsroot (make-symbol "-gtagsroot-"))
        (root (make-symbol "-ggtags-project-root-")))
    `(let* ((,root ggtags-project-root)
            (,gtagsroot (when (ggtags-find-project)
                          (directory-file-name (ggtags-current-project-root))))
            (process-environment
             (append (let ((process-environment process-environment))
                       (and ,gtagsroot (setenv "GTAGSROOT" ,gtagsroot))
                       (mapcar #'substitute-env-vars ggtags-process-environment))
                     process-environment
                     (and ,gtagsroot (list (concat "GTAGSROOT=" ,gtagsroot)))
                     (and (ggtags-find-project)
                          (not (ggtags-project-has-refs (ggtags-find-project)))
                          (list "GTAGSLABEL=ctags")))))
       (unwind-protect (save-current-buffer ,@body)
         (setq ggtags-project-root ,root)))))

(defun ggtags-get-libpath ()
  (when-let (path (ggtags-with-current-project (getenv "GTAGSLIBPATH")))
    (mapcar (apply-partially #'concat (file-remote-p default-directory))
            (split-string path (regexp-quote path-separator) t))))

(defun ggtags-create-tags (root)
  "Run `gtags' in directory ROOT to create tag files."
  (interactive "DRoot directory: ")
  (let ((process-environment process-environment))
    (when (zerop (length root)) (error "No root directory provided"))
    (setenv "GTAGSROOT" (expand-file-name
                         (directory-file-name (file-name-as-directory root))))
    (ggtags-with-current-project
     (and (not (getenv "GTAGSLABEL"))
          (yes-or-no-p "Use `ctags' backend? ")
          (setenv "GTAGSLABEL" "ctags"))
     (with-temp-message "`gtags' in progress..."
       (let ((default-directory (file-name-as-directory root)))
         (condition-case err
             (apply #'ggtags-process-string
                    "gtags" (and ggtags-use-idutils '("--idutils")))
           (error (if (and ggtags-use-idutils
                           (stringp (cadr err))
                           (string-match-p "mkid not found" (cadr err)))
                      ;; Retry without mkid
                      (ggtags-process-string "gtags")
                    (signal (car err) (cdr err))))))))
    (message "GTAGS generated in `%s'" root)
    root))

(defun ggtags-update-tags (&optional force)
  "Update GNU Global tag database.
Do nothing if GTAGS exceeds the oversize limit unless FORCE is
non-nil."
  (interactive (progn
                 (ggtags-check-project)
                 ;; Mark project info expired.
                 (setf (ggtags-project-timestamp (ggtags-find-project)) -1)
                 (list t)))
  (when (or force (and (ggtags-find-project)
                       (not (ggtags-project-oversize-p))
                       (ggtags-project-dirty-p (ggtags-find-project))))
    (ggtags-with-current-project
     (with-temp-message "`global -u' in progress..."
       (ggtags-process-string "global" "-u")
       (setf (ggtags-project-dirty-p (ggtags-find-project)) nil)))))

(defvar-local ggtags-completion-cache nil)

(defvar ggtags-completion-table
  (completion-table-dynamic
   (lambda (prefix)
     (unless (equal prefix (car ggtags-completion-cache))
       (setq ggtags-completion-cache
             (cons prefix
                   (ggtags-with-current-project
                    (split-string
                     (apply #'ggtags-process-string
                            "global"
                            ;; Note -c alone returns only definitions
                            (if completion-ignore-case
                                (list "--ignore-case" "-Tc" prefix)
                              (list "-Tc" prefix)))
                     "\n" t)))))
     (cdr ggtags-completion-cache))))

(defun ggtags-completion-at-point ()
  "A function for `completion-at-point-functions'."
  (when-let (bounds (funcall ggtags-bounds-of-tag-function))
    (and (< (car bounds) (cdr bounds))
         (list (car bounds) (cdr bounds) ggtags-completion-table))))

(defun ggtags-read-tag ()
  (ggtags-ensure-project)
  (let ((default (ggtags-tag-at-point))
        (completing-read-function ggtags-completing-read-function))
    (setq ggtags-current-tag-name
          (cond (current-prefix-arg
                 (ggtags-update-tags)
                 (completing-read
                  (format (if default "Tag (default %s): " "Tag: ") default)
                  ggtags-completion-table nil t nil nil default))
                ((not default)
                 (user-error "No tag at point"))
                (t (substring-no-properties default))))))

(defun ggtags-global-build-command (cmd &rest args)
  ;; CMD can be definition, reference, symbol, grep, idutils
  (let ((xs (append (list "global" "-v"
                          (format "--result=%s" ggtags-global-output-format)
                          (and ggtags-global-ignore-case "--ignore-case")
                          (and (ggtags-find-project)
                               (ggtags-project-has-color (ggtags-find-project))
                               "--color")
                          (and (ggtags-find-project)
                               (ggtags-project-has-path-style (ggtags-find-project))
                               "--path-style=shorter")
                          (and ggtags-global-treat-text "--other")
                          (pcase cmd
                            ((pred stringp) cmd)
                            (`definition "") ;-d not supported by Global 5.7.1
                            (`reference "-r")
                            (`symbol "-s")
                            (`path "--path")
                            (`grep "--grep")
                            (`idutils "--idutils")))
                    args)))
    (mapconcat #'identity (delq nil xs) " ")))

;; takes three values: nil, t and a marker
(defvar ggtags-global-start-marker nil)

(defvar ggtags-global-exit-status 0)
(defvar ggtags-global-match-count 0)

(defvar ggtags-tag-ring-index nil)

(defun ggtags-global-save-start-marker ()
  (when (markerp ggtags-global-start-marker)
    (setq ggtags-tag-ring-index nil)
    (ring-insert find-tag-marker-ring ggtags-global-start-marker)
    (setq ggtags-global-start-marker t)))

(defun ggtags-global-start (command &optional root)
  (let* ((default-directory (or root (ggtags-current-project-root)))
         (split-window-preferred-function ggtags-split-window-function)
         ;; See http://debbugs.gnu.org/13594
         (display-buffer-overriding-action
          (if (and ggtags-auto-jump-to-first-match
                   ;; Appeared in emacs 24.4.
                   (fboundp 'display-buffer-no-window))
              (list #'display-buffer-no-window)
            display-buffer-overriding-action)))
    (setq ggtags-global-start-marker (point-marker))
    (ggtags-navigation-mode +1)
    (setq ggtags-global-exit-status 0
          ggtags-global-match-count 0)
    (ggtags-update-tags)
    (ggtags-with-current-project
     (setq ggtags-global-last-buffer
           (compilation-start command 'ggtags-global-mode)))))

(defun ggtags-find-tag-continue ()
  (interactive)
  (ggtags-ensure-global-buffer
    (ggtags-navigation-mode +1)
    (let ((split-window-preferred-function ggtags-split-window-function))
      (ignore-errors (compilation-next-error 1))
      (compile-goto-error))))

(defun ggtags-find-tag (cmd &rest args)
  (ggtags-check-project)
  (ggtags-global-start (apply #'ggtags-global-build-command cmd args)))

;;;###autoload
(defun ggtags-find-tag-dwim (name &optional definition)
  "Find definitions or references of tag NAME by context.
If point is at a definition tag, find references, and vice versa.
With a prefix arg (non-nil DEFINITION) always find definitions."
  (interactive (list (ggtags-read-tag) current-prefix-arg))
  (ggtags-check-project)     ; for `ggtags-current-project-root' below
  (if (or definition
          (not buffer-file-name)
          (and (ggtags-find-project)
               (not (ggtags-project-has-refs (ggtags-find-project)))))
      (ggtags-find-tag 'definition name)
    (ggtags-find-tag
     (format "--from-here=%d:%s"
             (line-number-at-pos)
             (shell-quote-argument
              ;; Note `ggtags-global-start' binds default-directory to
              ;; project root.
              (file-relative-name
               buffer-file-name
               (if (string-prefix-p (ggtags-current-project-root)
                                    buffer-file-name)
                   (ggtags-current-project-root)
                 (locate-dominating-file buffer-file-name "GTAGS")))))
     name)))

(defun ggtags-find-reference (name)
  (interactive (list (ggtags-read-tag)))
  (ggtags-find-tag 'reference name))

(defun ggtags-find-other-symbol (name)
  "Find tag NAME that is a reference without a definition."
  (interactive (list (ggtags-read-tag)))
  (ggtags-find-tag 'symbol name))

(defun ggtags-read-string (prompt)
  "Like `read-string' but handle default automatically."
  (ggtags-ensure-project)
  (let ((prompt (if (string-match ": *\\'" prompt)
                    (substring prompt 0 (match-beginning 0))
                  prompt))
        (default (ggtags-tag-at-point)))
    (read-string (format (if default "%s (default `%s'): " "%s: ")
                         prompt default)
                 nil nil default)))

(defun ggtags-quote-pattern (pattern)
  (prin1-to-string (substring-no-properties pattern)))

(defun ggtags-grep (pattern &optional invert-match)
  "Use `global --grep' to search for lines matching PATTERN.
Invert the match when called with a prefix arg \\[universal-argument]."
  (interactive (list (ggtags-read-string (if current-prefix-arg
                                             "Inverted grep pattern"
                                           "Grep pattern"))
                     current-prefix-arg))
  (ggtags-find-tag 'grep (and invert-match "--invert-match")
                   "--" (ggtags-quote-pattern pattern)))

(defun ggtags-idutils-query (pattern)
  (interactive (list (ggtags-read-string "ID query pattern")))
  (ggtags-find-tag 'idutils "--" (ggtags-quote-pattern pattern)))

(defun ggtags-find-file (pattern &optional invert-match)
  (interactive (list (ggtags-read-string (if current-prefix-arg
                                             "Inverted path pattern"
                                           "Path pattern"))
                     current-prefix-arg))
  (let ((ggtags-global-output-format 'path))
    (ggtags-find-tag 'path (and invert-match "--invert-match")
                     "--" (ggtags-quote-pattern pattern))))

;; NOTE: Coloured output in grep requested: http://goo.gl/Y9IcX
(defun ggtags-find-tag-regexp (regexp directory)
  "List tags matching REGEXP in DIRECTORY (default to project root)."
  (interactive
   (progn
     (ggtags-check-project)
     (list (ggtags-read-string "POSIX regexp")
           (if current-prefix-arg
               (read-directory-name "Directory: " nil nil t)
             (ggtags-current-project-root)))))
  (ggtags-check-project)
  (ggtags-global-start
   (ggtags-global-build-command nil nil "-l" "--" (ggtags-quote-pattern regexp))
   (file-name-as-directory directory)))

(defun ggtags-query-replace (from to &optional delimited)
  "Query replace FROM with TO on files in the Global buffer.
If not in navigation mode, do a grep on FROM first.

Note: the regular expression FROM must be supported by both
Global and Emacs."
  (interactive
   ;; Note: in 24.4 query-replace-read-args returns a list of 4 elements.
   (let ((args (query-replace-read-args "Query replace (regexp)" t t)))
     (list (nth 0 args) (nth 1 args) (nth 2 args))))
  (unless (bound-and-true-p ggtags-navigation-mode)
    (let ((ggtags-auto-jump-to-first-match nil))
      (ggtags-grep from)))
  (let ((file-form
         '(let ((files))
            (ggtags-ensure-global-buffer
              (with-temp-message "Waiting for Grep to finish..."
                (while (get-buffer-process (current-buffer))
                  (sit-for 0.2)))
              (goto-char (point-min))
              (while (ignore-errors (compilation-next-file 1) t)
                (let ((m (get-text-property (point) 'compilation-message)))
                  (push (expand-file-name
                         (caar (compilation--loc->file-struct
                                (compilation--message->loc m))))
                        files))))
            (ggtags-navigation-mode -1)
            (nreverse files))))
    (tags-query-replace from to delimited file-form)))

(defun ggtags-delete-tag-files ()
  "Delete the tag files generated by gtags."
  (interactive (ignore (ggtags-check-project)))
  (when (ggtags-current-project-root)
    (let* ((re (concat "\\`" (regexp-opt '("GPATH" "GRTAGS" "GTAGS" "ID")) "\\'"))
           (files (loop for file in (directory-files (ggtags-current-project-root) t re)
                        ;; Don't trust `directory-files'.
                        when (let ((case-fold-search nil))
                               (string-match-p re (file-name-nondirectory file)))
                        collect file))
           (buffer "*GTags File List*"))
      (or files (user-error "No tag files found"))
      (with-output-to-temp-buffer buffer
        (princ (mapconcat #'identity files "\n")))
      (let ((win (get-buffer-window buffer)))
        (unwind-protect
            (progn
              (fit-window-to-buffer win)
              (when (yes-or-no-p "Remove GNU Global tag files? ")
                (with-demoted-errors (mapc #'delete-file files))
                (remhash (ggtags-current-project-root) ggtags-projects)
                (and (overlayp ggtags-highlight-tag-overlay)
                     (delete-overlay ggtags-highlight-tag-overlay))))
          (when (window-live-p win)
            (quit-window t win)))))))

(defun ggtags-browse-file-as-hypertext (file line)
  "Browse FILE in hypertext (HTML) form."
  (interactive (if (or current-prefix-arg (not buffer-file-name))
                   (list (read-file-name "Browse file: " nil nil t)
                         (read-number "Line: " 1))
                 (list buffer-file-name (line-number-at-pos))))
  (check-type line integer)
  (or (and file (file-exists-p file)) (error "File `%s' doesn't exist" file))
  (ggtags-check-project)
  (or (file-exists-p (expand-file-name "HTML" (ggtags-current-project-root)))
      (if (yes-or-no-p "No hypertext form exists; run htags? ")
          (let ((default-directory (ggtags-current-project-root)))
            (ggtags-with-current-project (ggtags-process-string "htags")))
        (user-error "Aborted")))
  (let ((url (ggtags-process-string "gozilla" "-p" (format "+%d" line)
                                    (file-relative-name file))))
    (or (equal (file-name-extension
                (url-filename (url-generic-parse-url url))) "html")
        (user-error "No hypertext form for `%s'" file))
    (when (called-interactively-p 'interactive)
      (message "Browsing %s" url))
    (browse-url url)))

(defun ggtags-next-mark (&optional arg)
  "Move to the next (newer) mark in the tag marker ring."
  (interactive)
  (and (ring-empty-p find-tag-marker-ring) (user-error "Tag ring empty"))
  (setq ggtags-tag-ring-index
        ;; Note `ring-minus1' gets newer item.
        (funcall (if arg #'ring-plus1 #'ring-minus1)
                 (or ggtags-tag-ring-index
                     (progn
                       (ring-insert find-tag-marker-ring (point-marker))
                       0))
                 (ring-length find-tag-marker-ring)))
  (let ((m (ring-ref find-tag-marker-ring ggtags-tag-ring-index))
        (i (- (ring-length find-tag-marker-ring) ggtags-tag-ring-index))
        (message-log-max nil))
    (message "%d%s marker%s" i (pcase (mod i 10)
                                 ;; ` required for 24.1 and 24.2
                                 (`1 "st")
                                 (`2 "nd")
                                 (`3 "rd")
                                 (_ "th"))
             (if (marker-buffer m) "" " (dead)"))
    (if (not (marker-buffer m))
        (ding)
      (switch-to-buffer (marker-buffer m))
      (goto-char m))))

(defun ggtags-prev-mark ()
  "Move to the previous (older) mark in the tag marker ring."
  (interactive)
  (ggtags-next-mark 'previous))

(defun ggtags-view-tag-history ()
  (interactive)
  (and (ring-empty-p find-tag-marker-ring)
       (user-error "Tag ring empty"))
  (let ((split-window-preferred-function ggtags-split-window-function)
        (inhibit-read-only t))
    (pop-to-buffer "*Tag Ring*")
    (erase-buffer)
    (tabulated-list-mode)
    (setq tabulated-list-entries
          ;; Use a function so that revert can work properly.
          (lambda ()
            (let ((counter (ring-length find-tag-marker-ring))
                  (elements (or (ring-elements find-tag-marker-ring)
                                (user-error "Tag ring empty")))
                  (action
                   (lambda (button) (interactive)
                     (let ((m (button-get button 'marker)))
                       (or (markerp m) (user-error "Marker dead"))
                       (setq ggtags-tag-ring-index
                             (ring-member find-tag-marker-ring m))
                       (pop-to-buffer (marker-buffer m))
                       (goto-char (marker-position m)))))
                  (get-line
                   (lambda (m)
                     (with-current-buffer (marker-buffer m)
                       (save-excursion
                         (goto-char m)
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))))
              (setq tabulated-list-format
                    `[("ID" ,(max (1+ (floor (log counter 10))) 2)
                       (lambda (x y) (< (car x) (car y))))
                      ("Buffer" ,(max (loop for m in elements
                                            for b = (marker-buffer m)
                                            maximize
                                            (length (and b (buffer-name b))))
                                      6)
                       t :right-align t)
                      ("Position" ,(max (loop for m in elements
                                              for p = (or (marker-position m) 1)
                                              maximize (1+ (floor (log p 10))))
                                        8)
                       (lambda (x y)
                         (< (string-to-number (aref (cadr x) 2))
                            (string-to-number (aref (cadr y) 2))))
                       :right-align t)
                      ("Contents" 100 t)])
              (tabulated-list-init-header)
              (mapcar (lambda (x)
                        (prog1
                            (list counter
                                  (if (marker-buffer x)
                                      (vector (number-to-string counter)
                                              `(,(buffer-name (marker-buffer x))
                                                face link
                                                follow-link t
                                                marker ,x
                                                action ,action)
                                              (number-to-string (marker-position x))
                                              (funcall get-line x))
                                    (vector (number-to-string counter)
                                            "(dead)" "?" "?")))
                          (decf counter)))
                      elements))))
    (setq tabulated-list-sort-key '("ID" . t))
    (tabulated-list-print)
    (fit-window-to-buffer)))

(defun ggtags-global-exit-message-function (_process-status exit-status msg)
  (setq ggtags-global-exit-status exit-status)
  (pcase-let ((`(,count . ,db)
               (save-excursion
                 (goto-char (point-max))
                 (if (re-search-backward
                      "^\\w+ \\(not found\\)\\|^\\([0-9]+\\) \\w+ located" nil t)
                     (cons (or (and (match-string 1) 0)
                               (string-to-number (match-string 2)))
                           (when (re-search-forward
                                  "using \\(?:\\(idutils\\)\\|'[^']*/\\(\\w+\\)'\\)"
                                  (line-end-position)
                                  t)
                             (or (and (match-string 1) "ID")
                                 (match-string 2))))
                   (cons 0 nil)))))
    (setq ggtags-global-match-count count)
    ;; Clear the start marker in case of zero matches.
    (and (zerop count)
         (markerp ggtags-global-start-marker)
         (setq ggtags-global-start-marker nil))
    (cons (if (> exit-status 0)
              msg
            (format "found %d %s"
                    count
                    (funcall (if (= count 1) #'car #'cadr)
                             (pcase db
                               ;; ` required for 24.1 and 24.2
                               (`"GTAGS"  '("definition" "definitions"))
                               (`"GSYMS"  '("symbol"     "symbols"))
                               (`"GRTAGS" '("reference"  "references"))
                               (`"GPATH"  '("file"       "files"))
                               (`"ID"     '("identifier" "identifiers"))
                               (_         '("match"      "matches"))))))
          exit-status)))

(defun ggtags-global-column (start)
  ;; START is the beginning position of source text.
  (when-let (mbeg (text-property-any start (line-end-position) 'global-color t))
    (setq ggtags-current-tag-name nil)
    (- mbeg start)))

;;; NOTE: Must not match the 'Global started at Mon Jun 3 10:24:13'
;;; line or `compilation-auto-jump' will jump there and fail. See
;;; comments before the 'gnu' entry in
;;; `compilation-error-regexp-alist-alist'.
(defvar ggtags-global-error-regexp-alist-alist
  (append
   `((path "^\\(?:[^/\n]*/\\)?[^ )\t\n]+$" 0)
     ;; ACTIVE_ESCAPE	src/dialog.cc	172
     (ctags "^\\([^ \t\n]+\\)[ \t]+\\(.*?\\)[ \t]+\\([0-9]+\\)$"
            2 3 nil nil 2 (1 font-lock-function-name-face))
     ;; ACTIVE_ESCAPE     172 src/dialog.cc    #undef ACTIVE_ESCAPE
     (ctags-x "^\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(\\(?:[^/\n]*/\\)?[^ \t\n]+\\)"
              3 2 (,(lambda () (ggtags-global-column (1+ (match-end 0)))))
              nil 3 (1 font-lock-function-name-face))
     ;; src/dialog.cc:172:#undef ACTIVE_ESCAPE
     (grep "^\\(.+?\\):\\([0-9]+\\):\\(?:$\\|[^0-9\n]\\|[0-9][^0-9\n]\\|[0-9][0-9].\\)"
           1 2 (,(lambda () (ggtags-global-column (1+ (match-end 2))))) nil 1)
     ;; src/dialog.cc ACTIVE_ESCAPE 172 #undef ACTIVE_ESCAPE
     (cscope "^\\(.+?\\)[ \t]+\\([^ \t\n]+\\)[ \t]+\\([0-9]+\\).*\\(?:[^0-9\n]\\|[^0-9\n][0-9]\\|[^:\n][0-9][0-9]\\)$"
             1 3 nil nil 1 (2 font-lock-function-name-face)))
   compilation-error-regexp-alist-alist))

(defun ggtags-abbreviate-file (start end)
  (let ((inhibit-read-only t)
        (amount (if (numberp ggtags-global-abbreviate-filename)
                    (- (- end start) ggtags-global-abbreviate-filename)
                  999))
        (advance-word (lambda ()
                        "Return the length of the text made invisible."
                        (let ((wend (min end (progn (forward-word 1) (point))))
                              (wbeg (max start (progn (backward-word 1) (point)))))
                          (goto-char wend)
                          (if (<= (- wend wbeg) 1)
                              0
                            (put-text-property (1+ wbeg) wend 'invisible t)
                            (1- (- wend wbeg)))))))
    (goto-char start)
    (while (and (> amount 0) (> end (point)))
      (decf amount (funcall advance-word)))))

(defun ggtags-abbreviate-files (start end)
  (goto-char start)
  (let* ((error-re (cdr (assq ggtags-global-output-format
                              ggtags-global-error-regexp-alist-alist)))
         (sub (cadr error-re)))
    (when (and ggtags-global-abbreviate-filename error-re)
      (while (re-search-forward (car error-re) end t)
        (when (and (or (not (numberp ggtags-global-abbreviate-filename))
                       (> (length (match-string sub))
                          ggtags-global-abbreviate-filename))
                   ;; Ignore bogus file lines such as:
                   ;;     Global found 2 matches at Thu Jan 31 13:45:19
                   (get-text-property (match-beginning sub) 'compilation-message))
          (ggtags-abbreviate-file (match-beginning sub) (match-end sub)))))))

(defvar-local ggtags-global-output-lines 0)

(defun ggtags-global--display-buffer (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (unless (get-buffer-window buffer)
      (let* ((split-window-preferred-function ggtags-split-window-function)
             (w (display-buffer (current-buffer) '(nil (allow-no-window . t)))))
        (and w (compilation-set-window-height w))))))

(defun ggtags-global-filter ()
  "Called from `compilation-filter-hook' (which see)."
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (ansi-color-apply-overlay-face beg end face)
             (put-text-property beg end 'global-color t)))))
    (ansi-color-apply-on-region compilation-filter-start (point)))
  ;; Get rid of line "Using config file '/PATH/TO/.globalrc'." or
  ;; "Using default configuration."
  (when (re-search-backward
         "^ *Using \\(?:config file '.*\\|default configuration.\\)\n"
         compilation-filter-start t)
    (replace-match ""))
  (incf ggtags-global-output-lines
        (count-lines compilation-filter-start (point)))
  (when (> ggtags-global-output-lines 5)
    (ggtags-global--display-buffer))
  (make-local-variable 'ggtags-global-large-output)
  (when (> ggtags-global-output-lines ggtags-global-large-output)
    (incf ggtags-global-large-output 500)
    (let ((message-log-max nil))
      (message "Output %d lines (Type `C-c C-k' to cancel)"
               ggtags-global-output-lines))))

(defun ggtags-handle-single-match (buf _how)
  (if (not (zerop ggtags-global-exit-status))
      ;; If exit abnormally display the buffer for inspection.
      (ggtags-global--display-buffer)
    (when (and ggtags-auto-jump-to-first-match
               (save-excursion
                 (goto-char (point-min))
                 (not (ignore-errors
                        (goto-char (compilation-next-single-property-change
                                    (point) 'compilation-message))
                        (end-of-line)
                        (compilation-next-single-property-change
                         (point) 'compilation-message)))))
      ;; For the `compilation-auto-jump' in idle timer to run. See also:
      ;; http://debbugs.gnu.org/13829
      (sit-for 0)
      (ggtags-navigation-mode -1)
      (ggtags-navigation-mode-cleanup buf 0))))

(defvar ggtags-global-mode-font-lock-keywords
  '(("^Global \\(exited abnormally\\|interrupt\\|killed\\|terminated\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
     (1 'compilation-error)
     (2 'compilation-error nil t))
    ("^Global found \\([0-9]+\\)" (1 compilation-info-face))))

(defvar compilation-always-kill)        ;new in 24.3

(define-compilation-mode ggtags-global-mode "Global"
  "A mode for showing outputs from gnu global."
  ;; Make it buffer local for `ggtags-abbreviate-files'.
  (make-local-variable 'ggtags-global-output-format)
  (setq-local compilation-error-regexp-alist
              (list ggtags-global-output-format))
  (setq-local compilation-auto-jump-to-first-error
              ggtags-auto-jump-to-first-match)
  (setq-local compilation-scroll-output 'first-error)
  ;; See `compilation-move-to-column' for details.
  (setq-local compilation-first-column 0)
  (setq-local compilation-error-screen-columns nil)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local compilation-error-face 'compilation-info)
  (setq-local compilation-exit-message-function
              'ggtags-global-exit-message-function)
  ;; See: https://github.com/leoliu/ggtags/issues/26
  (setq-local find-file-suppress-same-file-warnings t)
  (setq-local truncate-lines t)
  (jit-lock-register #'ggtags-abbreviate-files)
  (add-hook 'compilation-filter-hook 'ggtags-global-filter nil 'local)
  (add-hook 'compilation-finish-functions 'ggtags-handle-single-match nil t)
  (add-hook 'kill-buffer-hook (lambda () (ggtags-navigation-mode -1)) nil t))

;; NOTE: Need this to avoid putting menu items in
;; `emulation-mode-map-alists', which creates double entries. See
;; http://i.imgur.com/VJJTzVc.png
(defvar ggtags-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'next-error)
    (define-key map "\M-p" 'previous-error)
    (define-key map "\M-}" 'ggtags-navigation-next-file)
    (define-key map "\M-{" 'ggtags-navigation-previous-file)
    (define-key map "\M->" 'ggtags-navigation-last-error)
    (define-key map "\M-<" 'ggtags-navigation-first-error)
    (define-key map "\C-c\C-k"
      (lambda () (interactive)
        (ggtags-ensure-global-buffer (kill-compilation))))
    (define-key map "\M-o" 'ggtags-navigation-visible-mode)
    (define-key map [return] 'ggtags-navigation-mode-done)
    (define-key map "\r" 'ggtags-navigation-mode-done)
    (define-key map [remap pop-tag-mark] 'ggtags-navigation-mode-abort)
    map))

(defvar ggtags-mode-map-alist
  `((ggtags-navigation-mode . ,ggtags-navigation-map)))

;; Higher priority for `ggtags-navigation-mode' to avoid being
;; hijacked by modes such as `view-mode'.
(add-to-list 'emulation-mode-map-alists 'ggtags-mode-map-alist)

(defvar ggtags-navigation-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "GG-Navigation")))
    ;; Menu items: (info "(elisp)Extended Menu Items")
    (define-key map [menu-bar ggtags-navigation] (cons "GG-Navigation" menu))
    ;; Ordered backwards
    (define-key menu [visible-mode]
      '(menu-item "Visible mode" ggtags-navigation-visible-mode
                  :button (:toggle . (ignore-errors
                                       (ggtags-ensure-global-buffer
                                         visible-mode)))))
    (define-key menu [done]
      '(menu-item "Finish navigation" ggtags-navigation-mode-done))
    (define-key menu [abort]
      '(menu-item "Abort" ggtags-navigation-mode-abort))
    (define-key menu [last-error]
      '(menu-item "Last error" ggtags-navigation-last-error))
    (define-key menu [fist-error]
      '(menu-item "Fist error" ggtags-navigation-first-error))
    (define-key menu [previous-file]
      '(menu-item "Previous file" ggtags-navigation-previous-file))
    (define-key menu [next-file]
      '(menu-item "Next file" ggtags-navigation-next-file))
    (define-key menu [previous]
      '(menu-item "Previous match" previous-error))
    (define-key menu [next]
      '(menu-item "Next match" next-error))
    map))

(defun ggtags-move-to-tag (&optional name)
  "Move to NAME tag in current line."
  (let ((orig (point))
        (tag (or name ggtags-current-tag-name)))
    (beginning-of-line)
    (if (and tag (re-search-forward
                  (concat "\\_<" (regexp-quote tag) "\\_>")
                  (line-end-position)
                  t))
        (goto-char (match-beginning 0))
      (goto-char orig))))

(defun ggtags-navigation-mode-cleanup (&optional buf time)
  (let ((buf (or buf ggtags-global-last-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (when (get-buffer-process (current-buffer))
             (kill-compilation))
           (when (and (derived-mode-p 'ggtags-global-mode)
                      (get-buffer-window))
             (quit-window nil (get-buffer-window)))
           (and time (run-with-idle-timer time nil #'kill-buffer buf))))))

(defun ggtags-navigation-mode-done ()
  (interactive)
  (ggtags-navigation-mode -1)
  (setq tags-loop-scan t
        tags-loop-operate '(ggtags-find-tag-continue))
  (ggtags-navigation-mode-cleanup))

(defun ggtags-navigation-mode-abort ()
  (interactive)
  (ggtags-navigation-mode -1)
  ;; Run after (ggtags-navigation-mode -1) or
  ;; ggtags-global-start-marker might not have been saved.
  (when (and ggtags-global-start-marker
             (not (markerp ggtags-global-start-marker)))
    (setq ggtags-global-start-marker nil)
    (pop-tag-mark))
  (ggtags-navigation-mode-cleanup nil 0))

(defun ggtags-navigation-next-file (n)
  (interactive "p")
  (ggtags-ensure-global-buffer
    (compilation-next-file n)
    (compile-goto-error)))

(defun ggtags-navigation-previous-file (n)
  (interactive "p")
  (ggtags-navigation-next-file (- n)))

(defun ggtags-navigation-first-error ()
  (interactive)
  (ggtags-ensure-global-buffer
    (goto-char (point-min))
    (compilation-next-error 1)
    (compile-goto-error)))

(defun ggtags-navigation-last-error ()
  (interactive)
  (ggtags-ensure-global-buffer
    (goto-char (point-max))
    (compilation-previous-error 1)
    (compile-goto-error)))

(defun ggtags-navigation-visible-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (ggtags-ensure-global-buffer
    (visible-mode arg)))

(defvar ggtags-global-line-overlay nil)

(defun ggtags-global-next-error-function ()
  (ggtags-move-to-tag)
  (ggtags-global-save-start-marker)
  (ignore-errors
    (ggtags-ensure-global-buffer
      (unless (overlayp ggtags-global-line-overlay)
        (setq ggtags-global-line-overlay (make-overlay (point) (point)))
        (overlay-put ggtags-global-line-overlay 'face 'ggtags-global-line))
      (move-overlay ggtags-global-line-overlay
                    (line-beginning-position) (line-end-position)
                    (current-buffer))))
  (run-hooks 'ggtags-global-next-error-hook))

(define-minor-mode ggtags-navigation-mode nil
  :lighter
  (" GG[" (:eval
           (ignore-errors
             (ggtags-ensure-global-buffer
              (let ((index (when (get-text-property (line-beginning-position)
                                                    'compilation-message)
                             ;; Assume the first match appears at line 5
                             (- (line-number-at-pos) 4))))
                `((:propertize ,(if index
                                    (number-to-string (max index 0))
                                  "?") face success) "/")))))
   (:propertize (:eval (number-to-string ggtags-global-match-count))
                face success)
   (:eval
    (unless (zerop ggtags-global-exit-status)
      `(":" (:propertize ,(number-to-string ggtags-global-exit-status)
                         face error))))
   "]")
  :global t
  (if ggtags-navigation-mode
      (progn
        (add-hook 'next-error-hook 'ggtags-global-next-error-function)
        (add-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function))
    (remove-hook 'next-error-hook 'ggtags-global-next-error-function)
    (remove-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function)))

(defun ggtags-minibuffer-setup-function ()
  ;; Disable ggtags-navigation-mode in minibuffer.
  (setq-local ggtags-navigation-mode nil))

(defun ggtags-kill-file-buffers (&optional interactive)
  "Kill all buffers visiting files in current project."
  (interactive "p")
  (ggtags-check-project)
  (let ((directories (cons (ggtags-current-project-root) (ggtags-get-libpath)))
        (count 0)
        (some (lambda (pred list)
                (loop for x in list when (funcall pred x) return it))))
    (dolist (buf (buffer-list))
      (let ((file (and (buffer-live-p buf)
                       (not (eq buf (current-buffer)))
                       (buffer-file-name buf))))
        (when (and file (funcall some
                                 (lambda (dir)
                                   ;; Don't use `file-in-directory-p'
                                   ;; to allow symbolic links.
                                   (string-prefix-p dir file))
                                 directories))
          (and (kill-buffer buf) (incf count)))))
    (and interactive
         (message "%d %s killed" count (if (= count 1) "buffer" "buffers")))))

(defun ggtags-after-save-function ()
  (when (ggtags-find-project)
    (setf (ggtags-project-dirty-p (ggtags-find-project)) t)
    ;; When oversize update on a per-save basis.
    (when (and buffer-file-name (ggtags-project-oversize-p))
      (ggtags-with-current-project
       (process-file "global" nil 0 nil "--single-update"
                     (file-relative-name buffer-file-name))))))

(defvar ggtags-mode-prefix-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-'" 'previous-error)
    (define-key m (kbd "M-DEL") 'ggtags-delete-tag-files)
    (define-key m "\M-p" 'ggtags-prev-mark)
    (define-key m "\M-n" 'ggtags-next-mark)
    (define-key m "\M-f" 'ggtags-find-file)
    (define-key m "\M-o" 'ggtags-find-other-symbol)
    (define-key m "\M-g" 'ggtags-grep)
    (define-key m "\M-i" 'ggtags-idutils-query)
    (define-key m "\M-b" 'ggtags-browse-file-as-hypertext)
    (define-key m "\M-k" 'ggtags-kill-file-buffers)
    (define-key m "\M-h" 'ggtags-view-tag-history)
    (define-key m "\M-j" 'ggtags-visit-project-root)
    (define-key m (kbd "M-%") 'ggtags-query-replace)
    m))

(defvar ggtags-mode-map
  (let ((map (make-sparse-keymap))
        (menu (make-sparse-keymap "Ggtags")))
    (define-key map "\M-." 'ggtags-find-tag-dwim)
    (define-key map (kbd "M-]") 'ggtags-find-reference)
    (define-key map (kbd "C-M-.") 'ggtags-find-tag-regexp)
    (define-key map ggtags-mode-prefix-key ggtags-mode-prefix-map)
    ;; Menu items
    (define-key map [menu-bar ggtags] (cons "Ggtags" menu))
    ;; Ordered backwards
    (define-key menu [report-bugs]
      `(menu-item "Report bugs"
                  (lambda () (interactive)
                    (browse-url ggtags-bug-url)
                    (message "Please visit %s" ggtags-bug-url))
                  :help ,(format "Visit %s" ggtags-bug-url)))
    (define-key menu [custom-ggtags]
      '(menu-item "Customize Ggtags"
                  (lambda () (interactive) (customize-group 'ggtags))))
    (define-key menu [save-project]
      '(menu-item "Save project settings" ggtags-save-project-settings))
    (define-key menu [toggle-read-only]
      '(menu-item "Toggle project read-only" ggtags-toggle-project-read-only
                  :button (:toggle . buffer-read-only)))
    (define-key menu [visit-project-root]
      '(menu-item "Visit project root" ggtags-visit-project-root))
    (define-key menu [sep2] menu-bar-separator)
    (define-key menu [browse-hypertext]
      '(menu-item "Browse as hypertext" ggtags-browse-file-as-hypertext
                  :enable (ggtags-find-project)))
    (define-key menu [delete-tags]
      '(menu-item "Delete tag files" ggtags-delete-tag-files
                  :enable (ggtags-find-project)))
    (define-key menu [kill-buffers]
      '(menu-item "Kill project file buffers" ggtags-kill-file-buffers
                  :enable (ggtags-find-project)))
    (define-key menu [view-tag]
      '(menu-item "View tag history" ggtags-view-tag-history))
    (define-key menu [pop-mark]
      '(menu-item "Pop mark" pop-tag-mark
                  :help "Pop to previous mark and destroy it"))
    (define-key menu [next-mark]
      '(menu-item "Next mark" ggtags-next-mark))
    (define-key menu [prev-mark]
      '(menu-item "Previous mark" ggtags-prev-mark))
    (define-key menu [sep1] menu-bar-separator)
    (define-key menu [previous-error]
      '(menu-item "Previous match" previous-error))
    (define-key menu [next-error]
      '(menu-item "Next match" next-error))
    (define-key menu [find-file]
      '(menu-item "Find files" ggtags-find-file))
    (define-key menu [query-replace]
      '(menu-item "Query replace" ggtags-query-replace))
    (define-key menu [idutils]
      '(menu-item "Query idutils DB" ggtags-idutils-query))
    (define-key menu [grep]
      '(menu-item "Grep" ggtags-grep))
    (define-key menu [find-symbol]
      '(menu-item "Find other symbol" ggtags-find-other-symbol))
    (define-key menu [find-tag-regexp]
      '(menu-item "Find tag matching regexp" ggtags-find-tag-regexp))
    (define-key menu [find-reference]
      '(menu-item "Find reference" ggtags-find-reference))
    (define-key menu [find-tag-continue]
      '(menu-item "Continue find tag" tags-loop-continue))
    (define-key menu [find-tag]
      '(menu-item "Find tag" ggtags-find-tag-dwim))
    (define-key menu [update-tags]
      '(menu-item "Update tag files" ggtags-update-tags
                  :visible (ggtags-find-project)))
    (define-key menu [run-gtags]
      '(menu-item "Run gtags" ggtags-create-tags
                  :visible (not (ggtags-find-project))))
    map))

;;;###autoload
(define-minor-mode ggtags-mode nil
  :lighter (:eval (if ggtags-navigation-mode "" " GG"))
  (unless (timerp ggtags-highlight-tag-timer)
    (setq ggtags-highlight-tag-timer
          (run-with-idle-timer
           ggtags-highlight-tag-delay t #'ggtags-highlight-tag-at-point)))
  (if ggtags-mode
      (progn
        (add-hook 'after-save-hook 'ggtags-after-save-function nil t)
        ;; Append to serve as a fallback method.
        (add-hook 'completion-at-point-functions
                  #'ggtags-completion-at-point t t))
    (remove-hook 'after-save-hook 'ggtags-after-save-function t)
    (remove-hook 'completion-at-point-functions #'ggtags-completion-at-point t)
    (and (overlayp ggtags-highlight-tag-overlay)
         (delete-overlay ggtags-highlight-tag-overlay))
    (setq ggtags-highlight-tag-overlay nil)))

(defvar ggtags-highlight-tag-map
  (let ((map (make-sparse-keymap)))
    ;; Bind down- events so that the global keymap won't ``shine
    ;; through''. See `mode-line-buffer-identification-keymap' for
    ;; similar workaround.
    (define-key map [S-mouse-1] 'ggtags-find-tag-dwim)
    (define-key map [S-down-mouse-1] 'ignore)
    (define-key map [S-mouse-3] 'ggtags-find-reference)
    (define-key map [S-down-mouse-3] 'ignore)
    map)
  "Keymap used for valid tag at point.")

(put 'ggtags-active-tag 'face 'ggtags-highlight)
(put 'ggtags-active-tag 'keymap ggtags-highlight-tag-map)
;; (put 'ggtags-active-tag 'mouse-face 'match)
(put 'ggtags-active-tag 'help-echo
     "S-mouse-1 for definitions\nS-mouse-3 for references")

(defun ggtags-highlight-tag-at-point ()
  (when (and ggtags-mode ggtags-project-root (ggtags-find-project))
    (unless (overlayp ggtags-highlight-tag-overlay)
      (setq ggtags-highlight-tag-overlay (make-overlay (point) (point) nil t))
      (overlay-put ggtags-highlight-tag-overlay 'modification-hooks
                   (list (lambda (o after &rest _args)
                           (and (not after) (delete-overlay o))))))
    (let ((bounds (funcall ggtags-bounds-of-tag-function))
          (o ggtags-highlight-tag-overlay))
      (cond
       ((and bounds
             (eq (overlay-buffer o) (current-buffer))
             (= (overlay-start o) (car bounds))
             (= (overlay-end o) (cdr bounds)))
        ;; Overlay matches current tag so do nothing.
        nil)
       ((and bounds (let ((completion-ignore-case nil))
                      (ignore-errors
                        ;; May throw: global: only name char is
                        ;; allowed with -c option
                        (test-completion
                         (buffer-substring (car bounds) (cdr bounds))
                         ggtags-completion-table))))
        (move-overlay o (car bounds) (cdr bounds) (current-buffer))
        (overlay-put o 'category 'ggtags-active-tag))
       (t (move-overlay o
                        (or (car bounds) (point))
                        (or (cdr bounds) (point))
                        (current-buffer))
          (overlay-put o 'category nil))))))

;;; imenu

(defun ggtags-goto-imenu-index (name line &rest _args)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- line))
    (ggtags-move-to-tag name)))

;;;###autoload
(defun ggtags-build-imenu-index ()
  "A function suitable for `imenu-create-index-function'."
  (when-let (file (and buffer-file-name (file-relative-name buffer-file-name)))
    (with-temp-buffer
      (when (with-demoted-errors
              (zerop (ggtags-with-current-project
                      (process-file "global" nil t nil "-x" "-f" file))))
        (goto-char (point-min))
        (loop while (re-search-forward
                     "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" nil t)
              collect (list (match-string 1)
                            (string-to-number (match-string 2))
                            'ggtags-goto-imenu-index))))))

;;; hippie-expand

;;;###autoload
(defun try-complete-ggtags-tag (old)
  "A function suitable for `hippie-expand-try-functions-list'."
  (with-no-warnings                     ; to avoid loading hippie-exp
    (unless old
      (he-init-string (if (looking-back "\\_<.*" (line-beginning-position))
                          (match-beginning 0)
                        (point))
                      (point))
      (setq he-expand-list
            (and (not (equal he-search-string ""))
                 (ggtags-find-project)
                 (sort (all-completions he-search-string
                                        ggtags-completion-table)
                       #'string-lessp))))
    (if (null he-expand-list)
        (progn
          (if old (he-reset-string))
          nil)
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun ggtags-reload (&optional force)
  (interactive "P")
  (unload-feature 'ggtags force)
  (require 'ggtags))

(defun ggtags-unload-function ()
  (setq emulation-mode-map-alists
        (delq 'ggtags-mode-map-alist emulation-mode-map-alists))
  nil)

(provide 'ggtags)
;;; ggtags.el ends here
