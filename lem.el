;;; lem.el --- Basics for a lemmy client -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Package-Requires: ((emacs "27.1") (fedi-http "0.1"))
;; Keywords: multimedia, multimedia

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

;; basics for a lemmy client library. These functions just return plain JSON.

;;; Code:

(require 'fedi-http)

(setq fedi-http--api-version "v3")

;; (defvar lem-instance-url "https://lemmy.ml")
(defvar fedi-instance-url "https://lemmy.ml")

(defun lem-site ()
  "Return detauls about instance at `lem-instance-url'."
  (let ((url (fedi-http--api "site")))
    (fedi-http--get-json url)))

(defun lem-list-communities ()
  "List communities on the instance."
  (let ((url (fedi-http--api "community/list")))
    (fedi-http--get-json url)))

(defun lem-list-instance-posts ()
  "Get the instance's list of posts, like its home page."
  (let ((url (fedi-http--api "post/list")))
    (fedi-http--get-json url)))

(defun lem-community (id)
  "Get community with ID. Returns community_view object."
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "community")))
    (fedi-http--get-json url params)))

(defun lem-get-post (id) ; &optional auth
  "Get post with ID, for display.
Returns post_view, community_view and moderators objects."
  ;; post_view contains: post, creator, community,
  ;; creator_banned_from_community, counts, etc.
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "post")))
    (fedi-http--get-json url params)))

(defun lem-like-post (id)
  "Like post with ID. Requires auth."
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "post/like")))
    (fedi-http--get-json url params)))

(defun lem-report-post (id reason auth)
  "Report post with ID, providing REASON, using AUTH."
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "post/report")))
    (fedi-http--get-json url params)))

(defun lem-get-post-comments (post-id)
  "Get comments for POST-ID.
Returns comments,a list of comment objects, for display."
  (let ((params `(("post_id" . ,post-id)))
        (url (fedi-http--api "comment/list")))
    (fedi-http--get-json url params)))

(defun lem-community-posts (id ) ; limit page &optional auth
  "Get posts for community with ID.
Returns posts, for listing not viewing."
  (let* ((params `(("community_id" . ,id)))
         (url (fedi-http--api "post/list")))
    (fedi-http--get-json url params)))

(defun lem-search (query)
  "Search for QUERY.
Returns comments, posts, communities objects."
  (let ((params `(("q" . ,query)))
        (url (fedi-http--api "search")))
    (fedi-http--get-json url params)))

(defun lem-map-community-ids-names (communities)
  "Return an alist of id and name for each item in COMMUNITIES."
  ;; communities can have the same name and title, so to disambig we need
  ;; description, and to display it in completing-read.
  (mapcar (lambda (x)
            (let ((comm (alist-get 'community x)))
              (cons (number-to-string (alist-get 'id comm))
                    (alist-get 'name comm))))
          communities))

(defun lem-community-search ()
  "Search for a term, then community from list of matches."
  (let* ((query (read-string "Community search: "))
         (communities (alist-get 'communities (lem-search query)))
         (choice (completing-read "Community: "
                                  (lem-map-community-ids-names communities)))) ;
    ;; (lem-get-community choice))) ; returns community_view, its own info
    (lem-community-posts choice))) ; returns community's posts

(defun lem-get-person-by-id (id) ; &optional auth
  "Return details for user with ID.
Returns person_view, which has person, comments, posts, and moderates."
  (let ((params `(("person_id" . ,id)))
        (url (fedi-http--api "user")))
    (fedi-http--get-json url params)))

(defun lem-get-person-by-name (name) ; &optional auth
  "Return details for user with NAME.
Returns person_view, which has person, comments, posts, and moderates."
  (let ((params `(("username" . ,name)))
        (url (fedi-http--api "user")))
    (fedi-http--get-json url params)))

(provide 'lem)
;;; lem.el ends here
