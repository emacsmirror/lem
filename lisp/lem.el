;;; lem.el --- A basic lemmy client -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus and mastodon.el authors
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (fedi "0.1"))
;; URL: https://codeberg.org/martianh/lem
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

(require 'cl-lib)
(require 'lem-api)
(require 'lem-ui)

;;; AUTOLOADS


;;; VARS
(defvar lem-auth-token)

(defvar lem-user-id nil
  "The ID of the current user.")

(defvar lem-current-user nil
  "The name of the current user.")

;;; TYPES
(defconst lem-listing-types
  '("All" ; "Community" removed?
    "Local" "Subscribed"))

(defun lem-listing-type-p (str)
  "Non-nil if STR is in `lem-listing-types'."
  (cl-member str lem-listing-types :test 'equal))

(defconst lem-sort-types
  '("Active" "Hot" "New" "Old" "TopDay" "TopWeek" "TopMonth" "TopYear" "TopAll"
    "MostComments" "NewComments" "TopHour" "TopSixHour" "TopTwelveHour"))

(defun lem-sort-type-p (str)
  "Non-nil if STR is in `lem-sort-types'."
  (cl-member str lem-sort-types :test 'equal))

(defconst lem-comment-sort-types
  '("Hot" "Top" "New" "Old"))

(defun lem-comment-sort-type-p (str)
  "Non-nil if STR is in `lem-comment-sort-types'."
  (cl-member str lem-comment-sort-types :test 'equal))

(defconst lem-search-types
  '("All" "Comments" "Posts" "Communities" "Users" "Url"))

(defun lem-search-type-p (str)
  "Non-nil if STR is in `lem-search-types'."
  (cl-member str lem-search-types :test 'equal))

(defconst lem-user-view-types
  '("overview" "posts" "comments"))

(defun lem-user-view-type-p (str)
  "Return t if STR is in `lem-user-view-types'."
  (cl-member str lem-user-view-types :test 'equal))

;;; CUSTOMIZE

(defun lem-map-customize-options (list)
  "Return a choice/const list from LIST, for customize options."
  (append '(choice)
          (mapcar (lambda (x)
                    `(const ,x))
                  list)))

(defcustom lem-default-sort-type "Active"
  "The default sort type to use."
  :type
  (lem-map-customize-options lem-sort-types))

(defcustom lem-default-comment-sort-type "Hot"
  "The default comment sort type to use."
  :type
  (lem-map-customize-options lem-comment-sort-types))

(defcustom lem-default-listing-type "All"
  "The default listing type to use."
  :type
  (lem-map-customize-options lem-listing-types))

;;; MAP

(defvar lem-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'lem-ui-cycle-listing-type)
    (define-key map (kbd "C-c C-s") #'lem-ui-cycle-sort)
    (define-key map (kbd "n") #'lem-next-item)
    (define-key map (kbd "p") #'lem-prev-item)
    (define-key map (kbd "RET") #'lem-ui-view-thing-at-point)
    (define-key map (kbd "C") #'lem-ui-view-community-at-point)
    (define-key map (kbd "s") #'lem-ui-jump-to-subscribed)
    (define-key map (kbd "S") #'lem-ui-subscribe-to-community-at-point)
    ;;;
    map)
  "Keymap for `lem-mode'.")

;;; ENTRYPOINT

;;;###autoload
(defun lem ()
  "Open lem, a Lemmy client.
Load current user's instance posts."
  (interactive)
  (unless lem-auth-token
    (lem-login-set-token))
  (lem-ui-view-instance lem-default-listing-type lem-default-sort-type))

(defcustom lem-auth-file (concat user-emacs-directory "lem.plstore")
  "File path where Lemmy access tokens are stored."
  :group 'mastodon
  :type 'file)

(defun lem-auth-store-token (username token)
  "Store lemmy jwt TOKEN for USERNAME."
  (let ((plstore (plstore-open lem-auth-file))
        (print-length nil)
        (print-level nil))
    (plstore-put plstore username nil `(:jwt ,token))
    (plstore-save plstore)
    (plstore-close plstore)))

(defun lem-auth-fetch-token (username)
  "Return jwt token for USERNAME."
  (let* ((plstore (plstore-open lem-auth-file))
         (print-length nil)
         (print-level nil)
         (entry (plstore-get plstore username))
         (token (plist-get (cdr entry) :jwt)))
    (plstore-close plstore)
    token))

(defun lem-login-set-token ()
  "Login and set current user details."
  (interactive)
  (let* ((name (read-string "Username: ")))
    ;; if we have stored token, just set vars:
    (if-let ((token (lem-auth-fetch-token name)))
        (setq lem-auth-token token
              lem-current-user name)
      ;; else login manually, store token, and set var:
      (let ((password (read-string "Password: "))
            (login-response (lem-login name password))
            (token (alist-get 'jwt login-response)))
        (lem-auth-store-token name token)
        (setq lem-auth-token token))
      (setq lem-current-user name))))

(defun lem-set-user-id (username)
  "Set `lem-user-id' to that of USERNAME."
  (let* ((user (lem-api-get-person-by-name username))
         (person (alist-get 'person_view user))
         (id (alist-get 'id (alist-get 'person person))))
    (setq lem-user-id id)))

(define-derived-mode lem-mode special-mode "lem"
  "Major mode for Lemmy, the federated link-aggregator and forum."
  :group 'lem
  (read-only-mode 1)
  (emojify-mode 1))

(provide 'lem)
;;; lem.el ends here
