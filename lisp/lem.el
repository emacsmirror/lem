;;; lem.el --- A lemmy client -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus and mastodon.el authors
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.6
;; Package-Requires: ((emacs "29.1") (fedi "0.2") (markdown-mode "2.5"))
;; URL: https://codeberg.org/martianh/lem.el
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

;; Lem.el is a client library and interface for Lemmy, the federated
;; link-aggregator. For information about Lemmy, see <https://joinlemmy.org>.

;; lem-api.el is the API requests layer. All functions return parsed JSON
;; data. This can be used to make other kinds of interfaces, such as for gnus or
;; notmuch.

;; lem-ui.el and lem-post.el contain the main parts of the interface.

;; For set up and usage details, see the readme at <https://codeberg.org/martianh/lem.el>.

;;; Code:

(require 'cl-lib)
(require 'plstore)
(require 'lem-api)
(require 'lem-ui)
(require 'lem-post)

;;; AUTOLOADS


;;; VARS
(defvar lem-api-version "v3")

(defvar lem-instance-url "https://lemmy.ml")

(defvar lem-use-totp nil) ;; will ask for TOTP in login if non-nil

(defvar lem-auth-token)

(defvar lem-user-id nil
  "The ID of the current user.")

(defvar lem-current-user nil
  "The name of the current user.")

;;; TYPES
;; FIXME: make these all lists of symbols, so checks are faster and easier

(defconst lem-listing-types
  '("All" "Local" "Subscribed" "ModeratorView"))

(defun lem-listing-type-p (str)
  "Non-nil if STR is in `lem-listing-types'."
  (cl-member str lem-listing-types :test 'equal))

(defconst lem-search-listing-types
  '("All" "Local"))

(defconst lem-sort-types
  '("Active" "Hot" "New" "Old" "Controversial" "Scaled"
    "TopDay" "TopWeek" "TopMonth" "TopYear" "TopAll"
    "MostComments" "NewComments" "TopHour" "TopSixHour"
    "TopTwelveHour" "TopThreeMonths" "TopSixMonths" "TopNineMonths"))

(defun lem-sort-type-p (str)
  "Non-nil if STR is in `lem-sort-types'."
  (cl-member str lem-sort-types :test 'equal))

(defconst lem-comment-sort-types
  '("Hot" "Top" "New" "Old" "Controversial"))

(defun lem-comment-sort-type-p (str)
  "Non-nil if STR is in `lem-comment-sort-types'."
  (cl-member str lem-comment-sort-types :test 'equal))

;; as per web UI:
;; the API shows that users and seach views have normal sort types,
;; but the web UI gives these options
;; (API may not be up to date?)
(defconst lem-user-view-sort-types
  '("New" "Old" "Controversial"
    "TopDay" "TopWeek" "TopMonth" "TopYear" "TopAll"
    "MostComments" "NewComments" "TopHour" "TopSixHour"
    "TopTwelveHour" "TopThreeMonths" "TopSixMonths" "TopNineMonths"))

(defun lem-user-view-sort-type-p (str)
  "Non-nil if STR is in `lem-user-view-sort-types'."
  (cl-member str lem-user-view-sort-types :test 'equal))

;; FIXME same as `lem-comment-sort-types' but diff order?
(defconst lem-inbox-sort-types
  '("New" "Hot" "Top" "Old" "Controversial"))

(defun lem-inbox-sort-type-p (str)
  "Non-nil if STR is in `lem-inbox-sort-types'."
  (cl-member str lem-inbox-sort-types :test 'equal))

(defconst lem-search-types
  '("All" "Comments" "Posts" "Communities" "Users" "Url"))

(defconst lem-search-types-implemented
  '("Comments" "Posts" "Communities" "Users"))

(defun lem-search-type-p (str)
  "Non-nil if STR is in `lem-search-types'."
  (cl-member str lem-search-types :test 'equal))

(defconst lem-items-types
  '("posts" "comments"))

(defconst lem-user-items-types
  '("overview" "posts" "comments"))

(defun lem-user-view-type-p (str)
  "Non-nil if STR is in `lem-user-items-types'."
  (cl-member str lem-user-items-types :test 'equal))

(defconst lem-inbox-types
  '(all replies mentions private-messages))

;;; CUSTOMIZE

(defgroup lem nil
  "Lemmy client."
  :prefix "lem-"
  :group 'external)

(defcustom lem-auth-file (concat user-emacs-directory "lem.plstore")
  "File path where Lemmy access tokens are stored."
  :type 'file)

(defun lem-map-customize-options (list)
  "Return a choice/const list from LIST, for customize options."
  (append '(choice)
          (mapcar (lambda (x)
                    `(const ,x))
                  list)))

(defcustom lem-default-sort-type "Active"
  "The default sort type to use."
  :type (lem-map-customize-options lem-sort-types))

(defcustom lem-default-comment-sort-type "Hot"
  "The default comment sort type to use."
  :type (lem-map-customize-options lem-comment-sort-types))

(defcustom lem-default-communities-sort-type "TopMonth"
  "The default sort type for `lem-ui-browse-communities'."
  :type (lem-map-customize-options lem-sort-types))

(defcustom lem-default-listing-type "All"
  "The default listing type to use."
  :type (lem-map-customize-options lem-listing-types))

(defcustom lem-default-items-type "posts"
  "The default item (posts or comments) for community and instance views."
  :type (lem-map-customize-options lem-items-types))

(defcustom lem-default-user-items-type "overview"
  "The default item (overview, posts, or comments) for user views."
  :type (lem-map-customize-options lem-user-items-types))

(defcustom lem-use-emojify nil
  "Whether to enable `emojify-mode' in lem.el buffers."
  :type 'boolean)

(defcustom lem-enable-relative-timestamps t
  "Whether to show relative (to the current time) timestamps.
This will require periodic updates of a timeline buffer to
keep the timestamps current as time progresses."
  :type '(boolean :tag "Enable relative timestamps and background updater task"))

(defcustom lem-encrypt-auth-tokens nil
  "Whether to encrypt the user's authentication token in the plstore.
If you set this to non-nil, you also likely need to set
`plstore-encrypt-to' to your GPG key ID for decryption.
If you change the value of this variable, you need to also delete
the file ~/.emacs.d/lem.plstore and log in again."
  :type 'boolean)

(defcustom lem-highlight-current-item t
  "Whether to highlight the item at point.
Uses `cursor-face' special property. Highlights entire comment,
and post title."
  :type '(boolean))

;;; MAP

(defvar lem-mode-map
  (let ((map (make-sparse-keymap)))
    ;; nav/sort:
    (define-key map (kbd "C-c C-c") #'lem-ui-cycle-listing-type)
    (define-key map (kbd "C-c C-s") #'lem-ui-cycle-sort)
    (define-key map (kbd "C-c C-v") #'lem-ui-cycle-items)
    (define-key map (kbd "C-c C-h") #'lem-ui-cycle-search)
    (define-key map (kbd "o") #'lem-ui-choose-sort)
    (define-key map (kbd "n") #'lem-next-item)
    (define-key map (kbd "p") #'lem-prev-item)
    (define-key map (kbd "M-n") #'lem-ui-next-same-level)
    (define-key map (kbd "M-p") #'lem-ui-prev-same-level)
    (define-key map (kbd "M-u") #'lem-ui-branch-top-level)
    (define-key map (kbd "SPC") #'lem-ui-scroll-up-command)
    (define-key map (kbd "TAB") #'lem-ui-next-tab-item)
    (define-key map (kbd "<backtab>") #'lem-ui-prev-tab-item)
    (define-key map (kbd "RET") #'lem-ui-view-thing-at-point)
    ;; views:
    (define-key map (kbd "I") #'lem-ui-view-instance)
    (define-key map (kbd "C") #'lem-ui-browse-communities)
    (define-key map (kbd "S") #'lem-ui-jump-to-subscribed)
    ;; (define-key map (kbd "P") #'lem-ui-view-item-user)
    (define-key map (kbd "O") #'lem-ui-view-own-profile)
    (define-key map (kbd "A") #'lem-ui-view-saved-items)
    (define-key map (kbd "h") #'lem-ui-search)
    (define-key map (kbd "B") #'lem-ui-view-inbox)
    (define-key map (kbd "c") #'lem-ui-view-item-community)
    (define-key map (kbd "u") #'lem-ui-view-item-user)
    ;; actions:
    (define-key map (kbd "s") #'lem-ui-subscribe-to-community-at-point)
    (define-key map (kbd "a") #'lem-ui-save-item-toggle)
    (define-key map (kbd "r") #'lem-post-comment) ; Reply
    (define-key map (kbd "N") #'lem-post-compose) ; New
    (define-key map (kbd "l") #'lem-ui-like-item-toggle)
    (define-key map (kbd "d") #'lem-ui-delete-post-or-comment)
    (define-key map (kbd "e") #'lem-post-edit-post-or-comment)
    (define-key map (kbd "/") #'lem-switch-to-buffer)
    (define-key map (kbd "g") #'lem-ui-reload-view)
    (define-key map (kbd "f") #'lem-ui-comment-tree-fold)
    (define-key map (kbd "F") #'lem-ui-comment-fold-toggle)
    (define-key map (kbd "M-f") #'lem-ui-fold-current-branch)
    (define-key map (kbd "C-M-f") #'lem-ui-fold-all-toggle)
    (define-key map (kbd "M-C-q") #'lem-kill-all-buffers)
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

(defun lem-auth-store-token (username token)
  "Store lemmy jwt TOKEN for USERNAME."
  (let ((plstore (plstore-open lem-auth-file))
        (print-length nil)
        (print-level nil))
    (if lem-encrypt-auth-tokens
        (plstore-put plstore username nil `(:jwt ,token))
      (plstore-put plstore username `(:jwt ,token) nil))
    (plstore-save plstore)
    (plstore-close plstore)))

;;;###autoload
(defun lem-auth-fetch-token (&optional username)
  "Return jwt token for USERNAME."
  (let* ((plstore (plstore-open lem-auth-file))
         (print-length nil)
         (print-level nil)
         (entry (plstore-get plstore (or username lem-current-user)))
         (token (plist-get (cdr entry) :jwt)))
    (plstore-close plstore)
    token))

(defun lem-login-set-token ()
  "Login and set current user details."
  (interactive)
  (let* ((name (or lem-current-user
                   (read-string "Username: "))))
    ;; if we have stored token, just set vars:
    (if-let ((token (lem-auth-fetch-token name)))
        (progn (setq lem-auth-token token
                     lem-current-user name)
               (lem-set-user-id name))
      ;; else check site is a site:
      (when (lem-check-site)
        ;; then login manually, store token, and set var:
        (let* ((password (read-passwd
                          (format "Password [%s on %s]: "
                                  name (url-host
                                        (url-generic-parse-url
                                         lem-instance-url)))))
	       (totp (when lem-use-totp
                        (read-string "TOTP: ")))
               (login-response (lem-login name password totp))
               (token (alist-get 'jwt login-response)))
          (lem-auth-store-token name token)
          (setq lem-auth-token token
                lem-current-user name)
          (lem-set-user-id name))))))

(defun lem-check-site ()
  "Check that the site is a lemmy instance.
Actually check that the `actor-id' returned by `lem-get-site' is
equal to `lem-instance-url'."
  (let* ((site (lem-get-site))
         (ap-id (alist-get 'actor_id
                           (alist-get 'site
                                      (alist-get 'site_view site)))))
    (equal (concat lem-instance-url "/") ap-id)))

(defun lem-set-user-id (username)
  "Set `lem-user-id' to that of USERNAME."
  (let* ((user (lem-api-get-person-by-name username))
         (person (alist-get 'person_view user))
         (id (alist-get 'id (alist-get 'person person))))
    (setq lem-user-id id)))

(defun lem-kill-all-buffers ()
  "Kill all lem.el buffers."
  (interactive)
  (fedi-kill-all-buffers "*lem-"))

(defun lem-switch-to-buffer ()
  "Switch to a live lem.el buffer."
  (interactive)
  (fedi-switch-to-buffer "*lem-"))

(define-derived-mode lem-mode special-mode "lem"
  "Major mode for Lemmy, the federated link-aggregator and forum."
  :group 'lem
  (read-only-mode 1)
  (when (and lem-use-emojify
             (require 'emojify nil :no-error))
    (declare-function emojify-mode nil)
    (emojify-mode 1))
  (when lem-highlight-current-item
    (cursor-face-highlight-mode)))

(provide 'lem)
;;; lem.el ends here
