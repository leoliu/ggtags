;;; unit-tests.el --- Unit tests for ggtags   -*- lexical-binding: t; -*-

;; Copyright © 2018  Felipe Lema

;; Author: Felipe Lema <felipelema@mortemale.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; See http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
;;; See https://www.atlassian.com/continuous-delivery/continuous-integration-tutorial
;;; Because of an extra `find-file' and me running this interactively, you'll
;;; see a lot of (save-window-excursion …)
;;; This file is a modified version from https://github.com/FelipeLema/emacs-counsel-gtags


;;

;;; Code:

;; See https://emacs.stackexchange.com/a/19458/10785
(eval-when-compile (require 'cl))
(require 'dash)
(require 's)

(require 'ggtags)

;;;;;;;;;;;;;;;;;
;; Infrastructure
;;;;;;;;;;;;;;;;;
(defmacro ggtags--with-mock-project (&rest body)
  "Create mock project with source file and execute BODY.

You can access the project at `default-directory', non-empty source file and
header at `main-file-path' and `main-header-path'.

Note: sets  GTAGSLABEL envvar to \"default\" to avoid prompt in `ggtags-create-tags'"
  `(let* ((project-path (concat (locate-dominating-file "./" ".git")
				(file-name-as-directory "tests")))
	  (default-directory project-path))
     (save-window-excursion
       (setenv "GTAGSLABEL" "default")
       (ggtags-create-tags project-path)
       (let* ((__result (progn ,@body))
	      ;; clean up buffers that may have been opened in the process before
	      ;; returning
	      (buffers-&-paths
	       (--map (list it
			    (with-current-buffer it
			      (and buffer-file-name
				   (file-name-directory buffer-file-name))))
		      (buffer-list)))
	      (buffers-from-project (-filter
				     ;; use the buffer path as ref for removing
				     (-lambda ((buffer buffer-path))
				       (and (stringp buffer-path)
					    (string-equal buffer-path
							  project-path)))
				     buffers-&-paths)))
	 (-each buffers-from-project
	   (-lambda ((buffer buffer-path))
	     (kill-buffer buffer)))
	 ;; remove database files
	 (--each '("GPATH" "GRTAGS" "GTAGS")
	   (delete-file (concat
			 (file-name-as-directory project-path)
			 it)))
	 __result))))

(ert-deftest ggtags-00-can-create-db-and-read-symbols ()
  "For now, we can only create tags interactively"
  (ggtags--with-mock-project
   (should
    (string-equal (elt (ggtags-find-project) 1)
		  (file-truename (file-name-as-directory default-directory))))))

(ert-deftest ggtags-00-gtags-command-works ()
  (ggtags--with-mock-project
   (let* ((from-cmd (shell-command-to-string "global --result=grep another_global_fun"))
	  (clean (s-trim from-cmd)))
     (should (string-equal
	      "mymain.c:10:void another_global_fun(){"
	      clean)))))

(ert-deftest ggtags-00-can-query ()
  "For now, we can only create tags interactively"
  (ggtags--with-mock-project
   (should
    (string-equal (ggtags-current-project-root)
		  (file-truename (file-name-as-directory default-directory))))))
;;;;;;;;;;;;;;;;;
;; Actual testing
;;;;;;;;;;;;;;;;;

(provide 'ggtags-unit-tests)
;;; ggtags-unit-tests.el ends here
