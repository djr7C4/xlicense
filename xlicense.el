;;; xlicense.el --- Insert a pre-defined license text

;; Copyright (C) 2010  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: abbrev, convenience

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

;; $Id: xlicense.el,v 1.1 2010/12/16 08:26:26 cinsk Exp $

;;; Code:

(require 'cl)
(require 'skeleton)

(defvar license-types '((Copyright . "Empty")
                        (AGPL3 . "AGPL3")
                        (AGPL3-only . "AGPL3-only")
                        (Apache . "APACHE-2.0")
                        (Boost . "BOOST")
                        (BSD-4-clause . "BSD-4-clause")
                        (BSD-3-clause . "BSD-3-clause")
                        (BSD-2-clause . "BSD-2-clause")
                        (BSD . "BSD-3-clause")
                        (EPL . "EPL")
                        (FDL1.3 . "FDL1.3")
                        (FDL1.3-only . "FDL1.3-only")
                        (FDL1.2 . "FDL1.2")
                        (FDL1.2-only . "FDL1.2-only")
                        (FDL1.1 . "FDL1.1")
                        (FDL1.1-only . "FDL1.1-only")
                        (GPL3 . "GPL3")
                        (GPL3-only . "GPL3-only")
                        (GPL2 . "GPL2")
                        (GPL2-only . "GPL2-only")
                        (LGPL3 . "LGPL3")
                        (LGPL3-only . "LGPL3-only")
                        (LGPL2.1 . "LGPL2.1")
                        (LGPL2.1-only . "LGPL2.1-only")
                        (MIT . "MIT")
                        (Unlicense . "Unlicense"))
  "Alist of licenses.  CAR of each item is a symbol represents the license,
CDR of each item is a filename of the license template")

(defvar license--source-directory (file-name-directory (or buffer-file-name
                                                           load-file-name
                                                           (symbol-file 'license-types))))

(defvar license-template-directories (list (expand-file-name "license-templates" license--source-directory))
  "Directory for license templates")

(defvar license-file-directory (expand-file-name "license-files/" license--source-directory))

(defvar license-file-destination "LICENSE")

(defun copy-license-file (license)
  "Copy a license file to `license-file-name-destination' in the current directory."
  (interactive (list (completing-read "License: "
                                      (directory-files license-file-directory)
                                      (lambda (file)
                                        (let ((file (f-join license-file-directory file)))
                                          (and (not (f-hidden-p file 'last))
                                               (f-file-p file))))
                                      t)))
  (if (file-exists-p license-file-destination)
      (message "File %s already exists" license-file-destination)
    (copy-file (f-join djr-license-file-directory license)
               license-file-destination)
    (message "Created file %s (%s)" license-file-destination license)))

;; These are defined as separate variables so that they can easily be changed
;; just for licenses using local variables.
(defvar license-user-full-name user-full-name
  "The full name to use for licensing purposes.")

(defvar license-user-mail-address user-mail-address
  "The user mail address to use for licensing purposes.")

(defvar license-organization nil
  "The organization to use for licensing purposes.")

(defvar license-default-summary
  "Description: "
  "Short description of what it does.")

(defvar license-copyright-notice-function (lambda ()
                                            (insert (format "Copyright (C) %d %s" (nth 5 (decode-time)) auth))
                                            (if license-user-mail-address
                                                (insert (format " <%s>" license-user-mail-address))))
  "Function to insert the copyright notice.")

(defvar license-copyright-notice-position 'before
  "Indicates the position of the copyright notice relative to text of the license.  Valid values are 'before or 'after.")

(defvar license-choice nil
  "If non-nil, always use this license instead of prompting.")

(defvar license-eol-text "!@#$EOL!@#$"
  "Text to mark blank lines -- used internally")

(defun license-file (type)
  "Return the path of the given license file"
  (let ((tp (assoc type license-types)))
    (when tp
      (dolist (dir license-template-directories)
        (let ((file (expand-file-name (cdr tp) (file-name-as-directory dir))))
          (when (file-exists-p file)
            (return file)))))))

(defvar license-keywords-alist '(("@author@" . license-user-full-name)
                                 ("@email@" . license-user-mail-address)
                                 ("@year@" . (lambda ()
                                               (substring (current-time-string)
                                                          -4)))
                                 ("@organization@" . license-organization))
  "Keywords that need to be substituted by `license-substitute-keywords'.

The CAR of an item is a keyword and CDR is a replacement.  If the
CDR of an item is a function, the return value(string) is used as
a replacement.  If the returned value is nil, no substitution for
that keyword.")

(defun license-substitute-keywords (&optional record)
  "Substitute all occurences of keywords to their replacement and returns
the replacement positions in markers."
  (let (markers)
    (dolist (i license-keywords-alist)
      (let ((keyword (regexp-quote (car i)))
            (what (if (functionp (cdr i)) (funcall (cdr i)) (cdr i))))
        (if what
            (progn
              (goto-char (point-min))
              (while (re-search-forward keyword nil t)
                (if record
                    (setq markers (cons (point-marker) markers)))
                (replace-match what))))))
    markers))

(defun license-fill-paragraphs (lst)
  "Fill paragraphs at markers in LST."
  (dolist (i lst)
    (goto-char i)
    (fill-paragraph)))

(defun create-license (type &optional comments summary author)
  "Create a license paragraphs according to current buffer's major mode.

IF COMMENTS is non-nil, comment the license text.
If SUMMARY is non-nil, it is inserted as a header of the comment.
If AUTHOR is non-nil, all occurrence of the author keyword are
replaced to AUTHOR.

See `license-keywords-alist' for keywords and their meaning."
  (let ((desc (or (and summary (> (length summary) 0))
                  license-default-summary))
        (auth (or author license-user-full-name))
        (newlines t)
        (lfile (license-file type))
        (mode major-mode)
        (fill-points nil)
        ;; This is an ugly kludge that should be replaced with something better
        ;; but it works for now. Removing it would require significantly
        ;; refactoring this function.
        (local-vars (mapcar (lambda (entry)
                              ;; Ignore the 'eval variable. This isn't really
                              ;; correct behavior but it works well enough... An
                              ;; alternative would be to use
                              ;; (buffer-local-variables) instead of
                              ;; file-local-variables-alist but that is slow.
                              (if (eq (car entry) 'eval)
                                  (cons nil nil)
                                (cons (car entry)
                                      (symbol-value (car entry)))))
                            file-local-variables-alist)))
    (unless lfile
      (user-error "License file for %s not found" type))
    (with-temp-buffer
      ;; Restore file local variables from the buffer this was run in.
      (loop
       for (var . value) in local-vars
       do (when var
            (make-local-variable var)
            (set var value)))
      (when (eq license-copyright-notice-position 'before)
        (funcall license-copyright-notice-function)
        (insert "\n\n"))

      (let ((point (point-max)))
        (insert-file-contents lfile)
        (when (= point (point-max))
          (setq newlines nil)
          (when (eq license-copyright-notice-position 'before)
            (delete-char -2))))

      (goto-char (point-max))

      (when (eq license-copyright-notice-position 'after)
        (when newlines
          (insert "\n\n"))
        (funcall license-copyright-notice-function))

      (goto-char (point-min))
      (while (re-search-forward "^$" nil t)
        (replace-match license-eol-text))

      (let ((case-fold-search t)
            (markers (license-substitute-keywords t)))
        (funcall mode)
        (if (and comments comment-start)
            (let ((comment-style 'extra-line))
              (comment-region (point-min) (point-max))))

        (goto-char (point-min))

        (let ((re-eol (concat (regexp-quote license-eol-text) "$")))
          (while (re-search-forward re-eol nil t)
            (replace-match "")))

        (license-fill-paragraphs markers)

        ;; (print markers)
        ;; (pop-marker)
        (goto-char (point-max))
        (insert "\n"))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun insert-license (&optional type)
  "Insert a license template into the current buffer."
  (interactive (list license-choice))
  (let ((text (create-license
               (or type license-choice (intern (completing-read "Choose a license type: " license-types nil t)))
               t)))
    (insert text)))

(provide 'xlicense)
;;; xlicense.el ends here
