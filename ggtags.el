;;; ggtags.el --- GNU Global source code tagging system -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Free Software Foundation, Inc.

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.6
;; Keywords: tools, convenience
;; Created: 2013-01-29
;; URL: https://github.com/leoliu/ggtags

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

;; Use GNU Global source code tagging system in Emacs.
;; http://www.gnu.org/software/global

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'etags)                        ; for find-tag-marker-ring

(if (not (fboundp 'comment-string-strip))
    (autoload 'comment-string-strip "newcomment"))

(eval-when-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defgroup ggtags nil
  "GNU Global source code tagging system."
  :group 'tools)

(defface ggtags-highlight '((t (:underline t)))
  "Face used to highlight a valid tag at point.")

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
  "Non-nil to display file names abbreviated such as '/u/b/env'."
  :type '(choice (const :tag "No" nil)
                 (const :tag "Always" t)
                 integer)
  :group 'ggtags)

(defvar ggtags-cache nil)               ; (ROOT TABLE DIRTY TIMESTAMP)

(defvar ggtags-current-tag-name nil)

;; Used by ggtags-global-mode
(defvar ggtags-global-error "match"
  "Stem of message to print when no matches are found.")

(defmacro ggtags-ignore-file-error (&rest body)
  (declare (indent 0))
  `(condition-case nil
       (progn ,@body)
     (file-error nil)))

;; http://thread.gmane.org/gmane.comp.gnu.global.bugs/1518
(defvar ggtags-global-has-path-style    ; introduced in global 6.2.8
  (ggtags-ignore-file-error
    (and (string-match-p "^--path-style "
                         (shell-command-to-string "global --help"))
         t))
  "Non-nil if `global' supports --path-style switch.")

(defmacro ggtags-ensure-global-buffer (&rest body)
  (declare (indent 0))
  `(progn
     (or (and (buffer-live-p compilation-last-buffer)
              (with-current-buffer compilation-last-buffer
                (derived-mode-p 'ggtags-global-mode)))
         (error "No global buffer found"))
     (with-current-buffer compilation-last-buffer ,@body)))

(defun ggtags-get-timestamp (root)
  "Get the timestamp (float) of file GTAGS in ROOT directory.
Return -1 if it does not exist."
  (let ((file (expand-file-name "GTAGS" root)))
    (if (file-exists-p file)
        (float-time (nth 5 (file-attributes file)))
      -1)))

(defun ggtags-get-libpath ()
  (split-string (or (getenv "GTAGSLIBPATH") "")
                (regexp-quote path-separator) t))

(defun ggtags-cache-get (key)
  (assoc key ggtags-cache))

(defun ggtags-cache-set (key val &optional dirty)
  (let ((c (ggtags-cache-get key)))
    (if c
        (setcdr c (list val dirty (float-time)))
      (push (list key val dirty (float-time)) ggtags-cache))))

(defun ggtags-cache-mark-dirty (key flag)
  "Return non-nil if operation is successful."
  (let ((cache (ggtags-cache-get key)))
    (when cache
      (setcar (cddr cache) flag))))

(defun ggtags-cache-dirty-p (key)
  "Value is non-nil if 'global -u' is needed."
  (third (ggtags-cache-get key)))

(defun ggtags-cache-stale-p (key)
  "Value is non-nil if tags in cache needs to be rebuilt."
  (> (ggtags-get-timestamp key)
     (or (fourth (ggtags-cache-get key)) 0)))

;;;###autoload
(defun ggtags-root-directory ()
  (ggtags-ignore-file-error
    (with-temp-buffer
      (when (zerop (call-process "global" nil (list t nil) nil "-pr"))
        (file-name-as-directory
         (comment-string-strip (buffer-string) t t))))))

(defun ggtags-check-root-directory ()
  (or (ggtags-root-directory) (error "File GTAGS not found")))

(defun ggtags-ensure-root-directory ()
  (or (ggtags-root-directory)
      (if (yes-or-no-p "File GTAGS not found; run gtags? ")
          (let ((root (read-directory-name "Directory: " nil nil t)))
            (and (= (length root) 0) (error "No directory chosen"))
            (ggtags-ignore-file-error
              (with-temp-buffer
                (if (zerop (let ((default-directory
                                   (file-name-as-directory root)))
                             (call-process "gtags" nil t)))
                    (message "File GTAGS generated in `%s'"
                             (ggtags-root-directory))
                  (error "%s" (comment-string-strip (buffer-string) t t))))))
        (error "Aborted"))))

(defun ggtags-tag-names-1 (root &optional prefix)
  (when root
    (if (ggtags-cache-stale-p root)
        (let* ((default-directory (file-name-as-directory root))
               (tags (with-demoted-errors
                       (split-string
                        (with-output-to-string
                          (call-process "global" nil (list standard-output nil)
                                        nil "-c" (or prefix "")))))))
          (and tags (ggtags-cache-set root tags))
          tags)
      (cadr (ggtags-cache-get root)))))

;;;###autoload
(defun ggtags-tag-names (&optional prefix)
  "Get a list of tag names starting with PREFIX."
  (let ((root (ggtags-root-directory)))
    (when (and root (ggtags-cache-dirty-p root))
      (if (zerop (call-process "global" nil nil nil "-u"))
          (ggtags-cache-mark-dirty root nil)
        (message "ggtags: error running 'global -u'")))
    (apply 'append (mapcar (lambda (r)
                             (ggtags-tag-names-1 r prefix))
                           (cons root (ggtags-get-libpath))))))

(defun ggtags-read-tag (quick)
  (ggtags-ensure-root-directory)
  (let* ((tags (ggtags-tag-names))
         (sym (thing-at-point 'symbol))
         (default (and (member sym tags) sym)))
    (setq ggtags-current-tag-name
          (if quick (or default (error "No valid tag at point"))
            (completing-read
             (format (if default "Tag (default %s): " "Tag: ") default)
             tags nil t nil nil default)))))

(defvar ggtags-global-options
  (concat "-v --result=grep"
          (and ggtags-global-has-path-style " --path-style=shorter"))
  "Options (as a string) for running `global'.")

;;;###autoload
(defun ggtags-find-tag (name &optional verbose)
  "Find definitions or references to tag NAME by context.
If point is at a definition tag, find references, and vice versa.
When called with prefix, ask the name and kind of tag."
  (interactive (list (ggtags-read-tag (not current-prefix-arg))
                     current-prefix-arg))
  (ggtags-check-root-directory)
  (ggtags-navigation-mode +1)
  (ring-insert find-tag-marker-ring (point-marker))
  (let ((split-window-preferred-function
         (lambda (w) (split-window (frame-root-window w))))
        (default-directory (ggtags-root-directory)))
    (compilation-start
     (if verbose
         (format "global %s %s \"%s\""
                 ggtags-global-options
                 (if (y-or-n-p "Kind (y for definition n for reference)? ")
                     "" "-r")
                 name)
       (format "global %s --from-here=%d:%s \"%s\""
               ggtags-global-options
               (line-number-at-pos)
               (expand-file-name buffer-file-name)
               name))
     'ggtags-global-mode)))

(defun ggtags-find-tag-resume ()
  (interactive)
  (ggtags-ensure-global-buffer
    (ggtags-navigation-mode +1)
    (let ((split-window-preferred-function
           (lambda (w) (split-window (frame-root-window w)))))
      (compile-goto-error))))

(defun ggtags-global-exit-message-function (_process-status exit-status msg)
  (let ((count (save-excursion
                 (goto-char (point-max))
                 (if (re-search-backward "^\\([0-9]+\\) objects? located" nil t)
                     (string-to-number (match-string 1))
                   0))))
    (cons (if (> exit-status 0)
              msg
            (format "found %d %s" count (if (= count 1) "match" "matches")))
          exit-status)))

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
  (when ggtags-global-abbreviate-filename
    (while (re-search-forward "^\\([^:\n]+\\):[0-9]+:" end t)
      (when (and (or (not (numberp ggtags-global-abbreviate-filename))
                     (> (length (match-string 1))
                        ggtags-global-abbreviate-filename))
                 ;; Ignore bogus file lines such as:
                 ;;     Global found 2 matches at Thu Jan 31 13:45:19
                 (get-text-property (match-beginning 0) 'compilation-message))
        (ggtags-abbreviate-file (match-beginning 1) (match-end 1))))))

(defun ggtags-handle-single-match (buf _how)
  (unless (or (not ggtags-auto-jump-to-first-match)
              (save-excursion
                (goto-char (point-min))
                (ignore-errors
                  (goto-char (compilation-next-single-property-change
                              (point) 'compilation-message))
                  (end-of-line)
                  (compilation-next-single-property-change
                   (point) 'compilation-message))))
    (ggtags-navigation-mode -1)
    ;; 0.5s delay for `ggtags-auto-jump-to-first-match'
    (ggtags-navigation-mode-cleanup buf 0.5)))

(define-compilation-mode ggtags-global-mode "Global"
  "A mode for showing outputs from gnu global."
  (setq-local compilation-auto-jump-to-first-error
              ggtags-auto-jump-to-first-match)
  (setq-local compilation-scroll-output 'first-error)
  (setq-local compilation-disable-input t)
  (setq-local compilation-always-kill t)
  (setq-local compilation-error-face 'compilation-info)
  (setq-local compilation-exit-message-function
              'ggtags-global-exit-message-function)
  (setq-local truncate-lines t)
  (jit-lock-register #'ggtags-abbreviate-files)
  (add-hook 'compilation-finish-functions 'ggtags-handle-single-match nil t)
  (define-key ggtags-global-mode-map "o" 'visible-mode))

(defvar ggtags-navigation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-n" 'next-error)
    (define-key map "\M-p" 'previous-error)
    (define-key map "\M-}" 'ggtags-navigation-next-file)
    (define-key map "\M-{" 'ggtags-navigation-previous-file)
    (define-key map "\M-o" 'ggtags-navigation-visible-mode)
    (define-key map "\r" 'ggtags-navigation-mode-done)
    ;; Intercept M-. and M-* keys
    (define-key map [remap pop-tag-mark] 'ggtags-navigation-mode-abort)
    (define-key map [remap ggtags-find-tag] 'undefined)
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
  (let ((buf (or buf compilation-last-buffer)))
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (when (get-buffer-process (current-buffer))
             (kill-compilation))
           (when (and (derived-mode-p 'ggtags-global-mode)
                      (get-buffer-window))
             (delete-window (get-buffer-window)))
           (and time (run-with-idle-timer time nil
                                          'kill-buffer (current-buffer)))))))

(defun ggtags-navigation-mode-done ()
  (interactive)
  (ggtags-navigation-mode -1)
  (ggtags-navigation-mode-cleanup))

(defun ggtags-navigation-mode-abort ()
  (interactive)
  (pop-tag-mark)
  (ggtags-navigation-mode -1)
  (ggtags-navigation-mode-cleanup nil 0))

(defun ggtags-navigation-next-file (n)
  (interactive "p")
  (ggtags-ensure-global-buffer
    (compilation-next-file n)
    (compile-goto-error)))

(defun ggtags-navigation-previous-file (n)
  (interactive "p")
  (ggtags-navigation-next-file (- n)))

(defun ggtags-navigation-visible-mode (&optional arg)
  (interactive (list (or current-prefix-arg 'toggle)))
  (ggtags-ensure-global-buffer
    (visible-mode arg)))

(define-minor-mode ggtags-navigation-mode nil
  :lighter (" GG[" (:propertize "n" face error) "]")
  :global t
  (if ggtags-navigation-mode
      (progn
        (add-hook 'next-error-hook 'ggtags-move-to-tag)
        (add-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function))
    (remove-hook 'next-error-hook 'ggtags-move-to-tag)
    (remove-hook 'minibuffer-setup-hook 'ggtags-minibuffer-setup-function)))

(defun ggtags-minibuffer-setup-function ()
  ;; Disable ggtags-navigation-mode in minibuffer.
  (setq-local ggtags-navigation-mode nil))

(defun ggtags-kill-file-buffers (&optional interactive)
  "Kill all buffers visiting files in the root directory."
  (interactive "p")
  (ggtags-check-root-directory)
  (let ((root (ggtags-root-directory))
        (count 0)
        (some (lambda (pred list)
                (loop for x in list when (funcall pred x) return it))))
    (dolist (buf (buffer-list))
      (let ((file (and (buffer-live-p buf)
                       (not (eq buf (current-buffer)))
                       (buffer-file-name buf))))
        (when (and file (funcall some (apply-partially #'file-in-directory-p
                                                       (file-truename file))
                                 (cons root (ggtags-get-libpath))))
          (and (kill-buffer buf)
               (incf count)))))
    (and interactive
         (message "%d %s killed" count (if (= count 1) "buffer" "buffers")))))

(defun ggtags-after-save-function ()
  (let ((root (ggtags-root-directory)))
    (and root (ggtags-cache-mark-dirty root t))))

(defvar ggtags-tag-overlay nil)
(defvar ggtags-highlight-tag-timer nil)
(make-variable-buffer-local 'ggtags-tag-overlay)

(defun ggtags-highlight-tag-at-point (buffer)
  (when (eq buffer (current-buffer))
    (unless (overlayp ggtags-tag-overlay)
      (setq ggtags-tag-overlay (make-overlay (point) (point)))
      (overlay-put ggtags-tag-overlay 'ggtags t))
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (valid-tag (when bounds
                        (member (buffer-substring (car bounds) (cdr bounds))
                                (ggtags-tag-names))))
           (o ggtags-tag-overlay)
           (done-p (lambda ()
                     (and (memq o (overlays-at (car bounds)))
                          (= (overlay-start o) (car bounds))
                          (= (overlay-end o) (cdr bounds))
                          (or (and valid-tag (overlay-get o 'face))
                              (and (not valid-tag) (not (overlay-get o 'face))))))))
      (cond
       ((not bounds)
        (overlay-put ggtags-tag-overlay 'face nil)
        (move-overlay ggtags-tag-overlay (point) (point)))
       ((not (funcall done-p))
        (move-overlay o (car bounds) (cdr bounds))
        (overlay-put o 'face (and valid-tag 'ggtags-highlight)))))))

(defun ggtags-post-command-function ()
  (when (timerp ggtags-highlight-tag-timer)
    (cancel-timer ggtags-highlight-tag-timer))
  (setq ggtags-highlight-tag-timer
        (run-with-idle-timer 0.2 nil 'ggtags-highlight-tag-at-point
                             (current-buffer))))

(defvar ggtags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-." 'ggtags-find-tag)
    (define-key map "\M-," 'ggtags-find-tag-resume)
    (define-key map "\C-c\M-k" 'ggtags-kill-file-buffers)
    map))

;;;###autoload
(define-minor-mode ggtags-mode nil
  :lighter (:eval (if ggtags-navigation-mode "" " GG"))
  (if ggtags-mode
      (progn
        (or (ggtags-root-directory)
            (message "File GTAGS not found"))
        (add-hook 'after-save-hook 'ggtags-after-save-function nil t)
        (add-hook 'post-command-hook 'ggtags-post-command-function nil t))
    (remove-hook 'after-save-hook 'ggtags-after-save-function t)
    (remove-hook 'post-command-hook 'ggtags-post-command-function t)
    (and (overlayp ggtags-tag-overlay)
         (delete-overlay ggtags-tag-overlay))
    (setq ggtags-tag-overlay nil)))

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
  (when buffer-file-name
    (let ((file (file-truename buffer-file-name)))
      (ggtags-ignore-file-error
        (with-temp-buffer
          (when (zerop (call-process "global" nil t nil "-f" file))
            (goto-char (point-min))
            (loop while (re-search-forward
                         "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)" nil t)
                  collect (list (match-string 1)
                                (string-to-number (match-string 2))
                                'ggtags-goto-imenu-index))))))))

(provide 'ggtags)
;;; ggtags.el ends here
