;;; packages-update-interactive.el --- Simple tools for upgrading Emacs packages interactively
;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'package-vc nil t)
(require 'time-date)
(require 'diary-lib)

(defcustom package-refresh-interval 2
  "DAYS until `package-refresh-contents-maybe’."
  :type 'number
  :group 'package)

(defvar pui--is-package-vc (featurep 'package-vc)
  "Check Wether or not `package-vc' is supported.

`package-vc' is a new feature of Emacs 29.")

(defvar pui--archive-contents-file (concat package-user-dir "/archives/"
					   (car (car package-archives))
					   "/archive-contents")
  "Path to archive-contents file for checking freshness of packages.")

(defun pui--selected-p ()
  "Check if current line is selected."
  (equal ?S (char-after (line-beginning-position))))

(defun pui--select ()
  "Select package in current line."
  (if (null (pui--selected-p))
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char (line-beginning-position))
	  (delete-char 1)
	  (insert-char ?S)))))

(defun pui--unselect ()
  "Unselect package in current line."
  (if (pui--selected-p)
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char (line-beginning-position))
	  (delete-char 1)
	  (insert ?\ )))))

(defun pui--upgradeable-packages (&optional vc)
  "Get list of upgradable packages.

If VC is non-nil, upgradable vc package will also be included.
If package.el doesn’t support vc package, VC is non-nil, it will throw error.

Return the list of upgradable package-desc of installed package and of
`package-archive-contents' in list."
  (if (and vc (null pui--is-package-vc))
      (error "Trying to run `pui--upgradeable-packages' with vc but `package-vc' is not supported"))

  (remq nil (mapcar
	     (lambda (elem)
	       (let ((installed (cadr elem))
		     (available (cadr (assq (car elem) package-archive-contents))))
		 (if (and vc (package-vc-p installed))
		     (list installed nil)
		   (if (and available
			    (version-list-<
			     (package-desc-version installed)
			     (package-desc-version available)))
		       (list installed available)))))
	     package-alist)))

(defun pui--package-upgrade (old-pkg-desc new-pkg-desc)
  "Install NEW-PKG-DESC and delete OLD-PKG-DESC.

If OLD-PKG-DESC is a vc package, you SHOULD pass nil value to NEW-PKG-DESC."
  (if (and pui--is-package-vc
	   (package-vc-p old-pkg-desc))
      (package-vc-upgrade old-pkg-desc)
    (unwind-protect
	(package-install new-pkg-desc 'dont-select)
      (if (package-installed-p new-pkg-desc)
	  (package-delete old-pkg-desc 'force 'dont-unselect)))))

(defun pui--format-package-desc (pkgs)
  "Format PKGS to:
package-name (current-version) => (new-version)

If PKGS is a vc package:
package-name (current-version) (vc)

PKGS is a list composed of:
 (old-package-desc new-package-desc)."
  (let ((old (car pkgs))
	(new (cadr pkgs)))

    (format "%s (%s) %s"
	    (package-desc-name old)
	    (package-version-join (package-desc-version old))
	    (if (and pui--is-package-vc (package-vc-p old))
		"(vc)"
	      (format "=> (%s)" (package-version-join (package-desc-version new)))))))

(defun pui--package-upgrade-all (&optional vc)
  "Upgrade all packages non-interactively.

If VC is non-nil, upgrade vc package also.
If called interactively, VC will automatically be true if `package-vc' is
exist."
  (interactive `(,pui--is-package-vc))

  (if (and vc (null pui--is-package-vc))
      (error "Trying to upgrade all packages with vc but package-vc doesn’t exist"))

  (if-let ((upgradable-packages (pui--upgradeable-packages vc)))
      (mapcar
       (lambda (l)
	 (apply #'pui--package-upgrade l))
       upgradable-packages)))

(defun package-refresh-contents-maybe (&optional nostrict)
  "Refresh package only if `package-refresh-interval' days has passed.

If NOSTRICT is non-nil, ignore 1 day lapse, this to allows running
at specified TIME even if `package-refresh-interval' hasn’t passed
since last updated."
  (interactive)
  (if (null package-archives)
      (error "Null `package-archives'"))

  (if (and (cond ((null package-archive-contents)
		  (message "`package-archive-contents' does not exist."))

		 ((<= (if nostrict
			  (1- package-refresh-interval)
			package-refresh-interval)
		      (time-to-number-of-days
		       (time-since
			(file-attribute-modification-time
			 (file-attributes pui--archive-contents-file)))))
		  t))
	   (y-or-n-p "Refresh package contents now?"))
      (package-refresh-contents)))

(defun package-upgrade-interactively (&optional vc)
  "Upgrade packages interractively.

if VC is non-nil, vc package will also be included.
If called-interactively, VC will depend wheter or not there is `package-vc'."
  (interactive (list pui--is-package-vc))

  (if (and vc (not pui--is-package-vc))
      (error "Trying to run `package-upgrade-interactively' with vc but `package-vc' doesn’t exist"))

  (package-refresh-contents-maybe)

  (let ((upgradable-packages (pui--upgradeable-packages vc)))
    (if (null upgradable-packages)
	(message "All package are up-to-date")
      (if (and (equal (length upgradable-packages) 1)
	       (y-or-n-p (format "Upgrade %s now?" (pui--format-package-desc (car upgradable-packages)))))
	  (apply #'pui--package-upgrade (car upgradable-packages))

	(with-current-buffer (get-buffer-create "*upgrade-package-interactively*")
	  (save-selected-window
	    (let ((inhibit-read-only t))
	      (setq buffer-read-only t)
	      (erase-buffer)
	      (switch-to-buffer-other-window (current-buffer))
	      (set-window-dedicated-p (selected-window) t)

	      (insert "Package to Update:
s Select       C-S-s Select All       [RET] Confirm
u Unselect     C-S-u Unselect All     q     Quit\n\n")

	      (save-excursion
		(insert
		 (mapconcat
		  (lambda (elem)
		    (format "   %s"
			    (pui--format-package-desc elem)))
		  upgradable-packages "\n")))

	      (local-set-key
	       (kbd "s")
	       (lambda ()
		 "Select current line."
		 (interactive)
		 (if (> (line-number-at-pos) 4)
		     (pui--select))))

	      (local-set-key
	       (kbd "u")
	       (lambda ()
		 "Unselect current line."
		 (interactive)
		 (if (> (line-number-at-pos) 4)
		     (pui--unselect))))

	      (local-set-key
	       (kbd "C-S-s")
	       (lambda ()
		 "Select all."
		 (interactive)
		 (save-excursion
		   (save-restriction
		     (forward-line (- 5 (line-number-at-pos)))
		     (narrow-to-region (point) (point-max))
		     (while (not (eobp))
		       (pui--select)
		       (forward-line))))))

	      (local-set-key
	       (kbd "C-S-u")
	       (lambda ()
		 "Unselect all."
		 (interactive)
		 (save-excursion
		   (save-restriction
		     (forward-line (- 5 (line-number-at-pos)))
		     (narrow-to-region (point) (point-max))
		     (while (not (eobp))
		       (pui--unselect)
		       (forward-line))))))

	      (local-set-key
	       (kbd "RET")
	       `(lambda ()
		  "Upgrade all selected packages"
		  (interactive)
		  (save-excursion
		    (save-restriction
		      (forward-line (- 5 (line-number-at-pos)))
		      (narrow-to-region (point) (point-max))

		      (let (selected-indexes)
			(while (not (eobp))
			  (if (not (pui--selected-p))
			      (add-to-list 'selected-indexes
					   (- (line-number-at-pos) 1) t))
			  (forward-line))

			(if (null selected-indexes)
			    (message "Empty selection, press q to quit")

			  (unwind-protect
			      (mapcar
			       (lambda (i)
				 (apply #'pui--package-upgrade
					(nth i ,upgradable-packages)))
			       selected-indexes)
			    (kill-buffer-and-window))))))))

	      (local-set-key (kbd "q") 'kill-buffer-and-window))))))))

(defun pui--scheduler (time)
  "Run every TIME.

TIME MUST be in this format:
- 13:00
- 01:00pm"
  (if (equal (diary-entry-time time) diary-unknown-time)
      (error "Unrecognized time %s" time))

  (run-at-time time (* 60 60 24)
	       (lambda ()
		 (package-refresh-contents-maybe 'no-strict)
		 (package-upgrade-interactively pui--is-package-vc))))

(provide 'packages-update-interactive)
;;; packages-update-interactive.el ends here
