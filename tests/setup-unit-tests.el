;; originally from https://github.com/FelipeLema/emacs-counsel-gtags
;; get needed packages for testing using straight.el
(let ((bootstrap-file ;; TODO: does anyone know how to load packages without writing to disk?
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 4))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; get extra needed packages (just for testing)
(straight-use-package 'dash)
(straight-use-package 's)

;; load ../ggtags.el
(load (concat
       (locate-dominating-file "./" ".git")
       "ggtags.el"))
