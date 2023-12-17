;;; mu4e-crypto.el --- Encrypt and decrypt emails for mu4e with GnuPG -*- lexical-binding: t -*-

;; Copyright (C) 2023 Meritamen <meritamen@sdf.org>

;; Author: Meritamen <meritamen@sdf.org>
;; URL: https://github.com/meritamen/mu4e-crypto
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;; Created: 17 December 2023
;; Keywords: mu4e mail crypto

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities that simplify encrypting or decrypting
;; emails for mu4e, instead of manually switching to your termial then
;; running the GnuPG with tedious and repetitive arguments.

;;; Code:

(require 'epa)

(require 'cl-lib)

(defun mu4e-crypto--gpg-exists-p ()
  "Check if GnuPG is installed and available in the system's PATH.
  Return the path of the GnuPG executable if found, otherwise nil."
  (or
   (executable-find "gpg")
   (executable-find "gpg2")))

(defun mu4e-crypto--message-p ()
  "Check if current buffer named `*mu4e-article*'"
  (string= (buffer-name) "*mu4e-article*"))

(defun mu4e-crypto--draft-p ()
  "Check if  current buffer is named `*mu4e-draft*` or `*mu4e-draft*<number>`."
  (string-match-p "^\\*mu4e-draft\\*\\(<[0-9]+>\\)?$" (buffer-name)))

(defmacro without-yes-or-no (&rest body)
  "Override `yes-or-no-p' & `y-or-n-p', not to prompt for input and return t."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    ,@body))

;;;###autoload
(defun mu4e-crypto--decrypt-message ()
  "Decrypt email content of current mu4e buffer."
  (interactive)
  (when (and
         (mu4e-crypto--message-p)
         (mu4e-crypto--gpg-exists-p))
    (mu4e-crypto--mark-pgp-encrypted-message)
    (let ((start (point-min))
          (end   (point-max)))
      (without-yes-or-no (epa-decrypt-region start end)))))

(defun mu4e-crypto--mark-pgp-encrypted-message ()
  (mu4e-crypto--mark-constraint
   "-----BEGIN PGP MESSAGE-----"
   "-----END PGP MESSAGE-----"))

(defun mu4e-crypto--mark-constraint (begin end)
  (goto-char (point-min))
  (when (search-forward begin nil t)
      (beginning-of-line)
      (push-mark (point) nil t)
      (search-forward end nil t)))

;;;###autoload
(defun mu4e-crypto--encrypt-message ()
  "Encrypt email content of current mu4e buffer."
  (interactive)
  (when (or (mu4e-crypto--message-p)
            (mu4e-crypto--draft-p))
    (goto-char (point-min))
    (when (search-forward "Date:" nil t)
      (beginning-of-line)
      (forward-line 1)
      (let* ((start (point))
             (end (point-max))
             (recipient (read-string "Enter recipient: ")))
        (shell-command-on-region
         start end
         (format "gpg --encrypt --armor -r %s"
                 (shell-quote-argument recipient))
         nil t)))))

(provide 'mu4e-crypto)
;;; mu4e-crypto.el ends here