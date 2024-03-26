;;; mu4e-crypto.el --- Encrypt and decrypt emails for mu4e with GnuPG -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Shira Filianore <meritamen@sdf.org>

;; Author: Shira Filianore <meritamen@sdf.org>
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
;; emails for mu4e, instead of manually switching to your terminal then
;; running the GnuPG with tedious and repetitive arguments.
;;
;; `mu4e-crypto-encrypt-message' replaces messages with ciphertext directly.
;;
;; `mu4e-crypto-decrypt-message' decrypts messages, and displays decrypted
;; messages in `*mu4e-decrypted*' buffer.

;;; Code:

(defconst mu4e-crypto--pgp-message-begin "-----BEGIN PGP MESSAGE-----")

(defconst mu4e-crypto--pgp-message-end "-----END PGP MESSAGE-----")

(defconst mu4e-crypto--decrypted-buffer-name "*mu4e-decrypted*")

(defun mu4e-crypto--gpg-exists-p ()
  "Check if GnuPG is installed and available in the system's PATH."
  (or
   (executable-find "gpg")
   (executable-find "gpg2")))

(defun mu4e-crypto--message-p ()
  "Check if current buffer named `*mu4e-article*'."
  (string= "*mu4e-article*" (buffer-name)))

(defun mu4e-crypto--draft-p ()
  "Check if current buffer is named `*mu4e-draft*' or `*mu4e-draft*<number>'."
  (string-match-p "^\\*mu4e-draft\\*\\(<[0-9]+>\\)?$" (buffer-name)))

(defun mu4e-crypto--pgp-message-exists-p ()
  "Check if any pgp-messages exist."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward mu4e-crypto--pgp-message-begin nil t)
         (search-forward mu4e-crypto--pgp-message-end nil t))))

;;;###autoload
(defun mu4e-crypto-decrypt-message ()
  "Decrypt email content of current mu4e buffer."
  (interactive)
  (when
      (and (mu4e-crypto--message-p)
           (mu4e-crypto--gpg-exists-p)
           (mu4e-crypto--pgp-message-exists-p))
    (mu4e-crypto--mark-pgp-encrypted-message)
    (let ((secret (buffer-substring-no-properties (region-beginning) (region-end))))
      (switch-to-buffer (get-buffer-create mu4e-crypto--decrypted-buffer-name))
      (insert secret)
      (unless (zerop (shell-command-on-region
                      (point-min) (point-max) "gpg --decrypt"
                      mu4e-crypto--decrypted-buffer-name t))
        (user-error "Decryption failed: gpg exited with a non-zero return code")))))

(defun mu4e-crypto--mark-pgp-encrypted-message ()
  "Search and mark region that is a PGP message."
  (mu4e-crypto--mark-constraint
   mu4e-crypto--pgp-message-begin
   mu4e-crypto--pgp-message-end))

(defun mu4e-crypto--mark-constraint (begin end)
  "Search and mark region closed by BEGIN and END."
  (goto-char (point-min))
  (if (not (search-forward begin nil t))
      (user-error "%s not matched" begin)
    (beginning-of-line)
    (push-mark (point) nil t)
    (if (not (search-forward end nil t))
        (user-error "%s not matched" end))))

(defun mu4e-crypto--check-email-headers ()
  "Check if all standard email headers found."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((headers '("From:" "To:" "Subject:" "Date:"))
          (all-found t))
      (dolist (header headers all-found)
        (unless (re-search-forward (concat "^" (regexp-quote header)) nil t)
          (setq all-found nil)
          (user-error "Email header '%s' not found" header)))
      (when all-found (message "All standard email headers found.")))))

;;;###autoload
(defun mu4e-crypto-encrypt-message ()
  "Encrypt email content of current mu4e buffer."
  (interactive)
  (save-excursion
    (when
        (and (mu4e-crypto--draft-p)
             (mu4e-crypto--check-email-headers)
             (mu4e-crypto--gpg-exists-p))
      (goto-char (point-min))
      (when (search-forward "Date:" nil t)
        (beginning-of-line)
        (forward-line 1)
        (let ((recipient (read-string "Enter recipient: ")))
          (shell-command-on-region
           (point)
           (point-max)
           (format "gpg --encrypt --armor -r %s"
                   (shell-quote-argument recipient))
           nil t))))))

(provide 'mu4e-crypto)
;;; mu4e-crypto.el ends here
