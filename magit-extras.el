;;; magit-extras.el --- Extra functionality for magit.

;; Copyright (C) 2014-2015 Kosyrev Serge

;; Author: Kosyrev Serge <_deepfire@feelingofgreen.ru>
;; Created: 6 Dec 2014
;; Keywords: git
;; Homepage: https://github.com/deepfire/magit-extras
;; Version: 0.0.1
;; Package-Requires: ((magit))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;;
;;; Lol.
;;;

(require 'cl)

;;;
;;; Mode-specific
;;;
(defcustom org-magit-review-mode-hook nil
  "Hook run when entering the Org Magit review mode."
  :options '()
  :type 'hook)

(defvar org-magit-review-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "C-c C-r C-c") 'org-magit-review-done-for-now)
    (define-key map (kbd "C-c C-r C-x") 'org-magit-review-to-mail-compose)
    map)
  "Keymap for `org-magit-review-mode'.")

(define-derived-mode org-magit-review-mode org-mode ""
  "Org Magit Review.

\\{org-magit-review-mode-map}")

(define-key magit-mode-map (kbd "M-RET") 'org-magit-review)

;;;
;;; Utils
;;;
(defun chomp-end (str)
  "Chomp tailing whitespace from STR."
  (replace-regexp-in-string (rx (* (any " \t\n")) eos)
			    ""
			    str))

(defun chomp-first-line (str)
  (let ((nlposn (position ?\n str)))
    (if nlposn
	(cl-subseq str (1+ nlposn))
	"")))

(defun magit-map-refs (regex fn)
  (let (ret)
    (dolist (upstream (magit-git-lines "show-ref"))
      (setq upstream (cadr (split-string upstream " ")))
      (when (and (not (string-match-p "HEAD$" upstream))
		 (string-match-p regex upstream))
	(push (funcall fn upstream) ret)))
    ret))

(defun magit-map-all-remote-refs (fn)
  (magit-map-refs "^refs/remotes/" fn))

(defun magit-map-remote-refs (remote-name fn)
  (magit-map-refs (format "^refs/remotes/%s/" remote-name) fn))

(defun magit-map-heads (fn)
  (magit-map-refs "^refs/heads/" fn))

(defun magit-item-text (item)
  (buffer-substring (magit-section-beginning hunk)
		    (magit-section-end hunk)))

(defun org-find-if-between (f begin end)
  (let (ret)
    (org-map-region (lambda ()
		      (let ((x (org-element-at-point)))
			(when (funcall f x)
			  (setf ret x))))
		    begin end)
    ret))

(defun org-find-if (f)
  (org-find-if-between f (point-min) (point-max)))

;;;
;;; Pure
;;;
(defun org-magit-review-file-name (repo-name branch-name)
  (expand-file-name (concat "~/org-magit-review-" repo-name "-" branch-name ".org")))

(defun org-magit-topdir-repo-name (topdir)
  (file-name-base (directory-file-name topdir)))

;;;
;;; Global state
;;;
;;  review buffer map :: Repo -> Branch -> Buffer
(defvar org-magit-review-buffers (make-hash-table :test 'equal))

(defun org-magit-create-review-buffer (repo branch)
  (let ((review-file (org-magit-review-file-name repo branch)))
    ;; (if (file-exists-p review-file)
    ;; 	(warn "Hmm, a review file already exists for repo=%s branch=%s." repo branch))
    (find-file review-file)))

(defun %org-magit-review-buffer (repo-name branch-name)
  (gethash (cons repo-name branch-name) org-magit-review-buffers))

(defun org-magit-review-ensure-buffer (repo-name branch-name)
  (let ((b (%org-magit-review-buffer repo-name branch-name)))
    (if (and b (buffer-live-p b))
	b
	(setf (gethash (cons repo-name branch-name) org-magit-review-buffers)
	      (org-magit-create-review-buffer repo-name branch-name)))))

;; commit branch map :: Topdir -> Commit-Id -> Branch
(defvar org-magit-review-commit-branches (make-hash-table :test 'equal))

(defvar org-magit-default-branches (make-hash-table :test 'equal))

;; XXX: currently only a single branch per repository is supported..
(defun org-magit-review-commit-determine-branch (topdir commit-id)
  (or (gethash topdir org-magit-default-branches)
      (setf (gethash topdir org-magit-default-branches)
	    (magit-read-rev "branch to review"))))

(defun org-magit-review-commit-branch (topdir commit-id)
  (or (gethash (cons topdir commit-id) org-magit-review-commit-branches)
      (setf (gethash (cons topdir commit-id) org-magit-review-commit-branches)
	    (org-magit-review-commit-determine-branch topdir commit-id))))

(defun org-magit-clear-ephemeral-maps ()
  (cl-clrhash org-magit-review-commit-branches)
  (cl-clrhash org-magit-default-branches))

;;;
;;; Magit commit buffer parsing
;;;
(defun org-magit-review-commit-buffer-commit-id ()
  (let ((commit-id (buffer-substring-no-properties 1 41)))
    (unless (string-match-p "[0-9a-f]*" commit-id)
      (error "Buffer '%s' isn't a valid Magit commit buffer: doesn't start with a commit-id."
	     (buffer-name)))
    commit-id))

(defun org-magit-review-commit-buffer-parse ()
  "Return commit-id, author, description as multiple values."
  (save-excursion
    (list (org-magit-review-commit-buffer-commit-id)
	  (progn
	    (beginning-of-buffer)
	    (beginning-of-line 2)
	    (re-search-forward "Author: \\(.*\\)$")
	    (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
	  (progn
	    (beginning-of-buffer)
	    (beginning-of-line 5)
	    (re-search-forward "    \\(.*\\)$")
	    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

;;;
;;; Intermediate layer
;;;
(defun org-magit-review-insert-commit-heading (commit-id commit-desc)
  (message "Adding new review entry for commit '%s'" commit-desc)
  (if (org-element-at-point)
      (org-insert-heading-after-current)
      (org-insert-heading))
  (insert (format "%s\n\n  " commit-desc))
  (org-entry-put nil "commit-id" commit-id)
  (org-element-at-point))

(defun org-magit-review-ensure-commit-heading (commit-id commit-desc)
  (or (org-find-if (lambda (x)
		     (equal commit-id (org-element-property :COMMIT-ID x))))
      (org-magit-review-insert-commit-heading commit-id commit-desc)))

(defvar org-magit-review-filename-extension-to-babel-mode-map (make-hash-table :test 'equal))

(dolist (kv '(("c" .   "c")
	      ("org" . "org")))
  (setf (gethash (car kv) org-magit-review-filename-extension-to-babel-mode-map)
	(cdr kv)))

(defun org-magit-review-filename-to-babel-lang (filename)
  (gethash (file-name-extension filename)
	   org-magit-review-filename-extension-to-babel-mode-map))

(defun org-magit-review-insert-review-entry (file line text)
  (insert-string (format "In %s, line %s:\n  " file line))
  (insert-string "<s")
  (org-try-structure-completion)
  (insert-string (format "%s" (org-magit-review-filename-to-babel-lang file)))
  (next-line)
  (insert-string text)
  (next-line)
  (newline-and-indent)
  (newline-and-indent))

(defun org-magit-review-add (repo branch commit-desc commit-id author file line selection)
  (message "branch=%s commit-desc=%s commit-id=%s"
	   repo branch commit-desc commit-id author file line selection)
  ;; (message "repo=%s branch=%s commit-desc=%s commit-id=%s author='%s' file=%s line=%s sel=%s"
  ;; 	   repo branch commit-desc commit-id author file line selection)
  (let ((buffer (org-magit-review-ensure-buffer repo branch)))
    (magit-mode-display-buffer buffer 'org-magit-review-mode 'pop-to-buffer)
    (with-current-buffer buffer
      (org-magit-review-mode)
      (let ((elt (org-magit-review-ensure-commit-heading commit-id commit-desc)))
	(org-magit-review-insert-review-entry file line selection)))))

;;; Commands:
;;;
;;;###autoload
(defun org-magit-review ()
  (interactive)
  (let ((topdir (magit-get-top-dir)))
    (unless topdir
      (user-error "Not inside a Git repository"))
    (destructuring-bind (commit-id author commit-desc) (org-magit-review-commit-buffer-parse)
      (let* ((repo (org-magit-topdir-repo-name topdir))
	     (branch (org-magit-review-commit-branch topdir commit-id)))
	(magit-section-action review (info parent-info)
	  (hunk ;; the assumption we're in the commit buffer is validated
	   (let* ((hunk it)
		  (diff (magit-section-parent hunk))
		  (file (magit-section-info diff))
		  (line (magit-hunk-item-target-line hunk))
		  (sel (chomp-end (if (use-region-p)
				      (buffer-substring (region-beginning) (region-end))
				      (chomp-first-line (magit-item-text hunk))))))
	     ;; (message "repo=%s branch=%s commit-desc=%s commit-id=%s author='%s' file=%s line=%s sel=%s"
	     ;; 	      repo branch commit-desc commit-id author file line sel)
	     (org-magit-review-add repo branch commit-desc commit-id author file line sel)
	     ))
	  (commit
	   (message "review'n commit %s.." it)))))))

(defun org-magit-review-done-for-now ()
  (interactive)
  (save-buffer)
  (magit-mode-quit-window))

(defun org-magit-forget-branch-cache ()
  (interactive)
  (org-magit-clear-ephemeral-maps))

;;; Code:
;;; magit-extras.el ends here
