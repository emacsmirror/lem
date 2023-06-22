;;; lem.el --- A basic lemmy client -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus and mastodon.el authors
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Package-Requires: ((emacs "27.1") (fedi "0.1"))
;; Keywords: multimedia, comm, web, fediverse

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

;; Major mode for Lemmy, the federated link-aggregator and forum software. See <https://joinlemmy.org>.

;;; Code:

(require 'persist)
;; (require 'lem-request)
;; (require 'lem-ui)

;;; AUTOLOADS


;;; VARS
(persist-defvar lem-auth-token nil
                "A user auth token for a lemmy instance.")


;;; CUSTOMIZE


;;; MAP
(defvar lem-mode-map
  (let ((map (make-sparse-keymap)))
    ;;;
    map)
  "Keymap for `lem-mode'.")

;;; ENTRYPOINT
(defun lem ()
  "Open lem, a Lemmy client.
Load current user's instance posts."
  (interactive)
  (unless lem-auth-token
    (lem-login-set-token))
  (lem-ui-view-instance "Top" "All")) ; add customize defaults

(defun lem-login-set-token ()
  "Login for user NAME with PASSWORD."
  (interactive)
  (let* ((name (read-string "Username: "))
         (password (read-string "Password: "))
         (json (lem-login name password)))
    (setq lem-auth-token (alist-get 'jwt json)))) ;


(define-derived-mode lem-mode special-mode "lem"
  "Major mode for Lemmy, the federated link-aggregator and forum."
  :group 'lem
  (read-only-mode 1))

(provide 'lem)
;;; lem.el ends here
