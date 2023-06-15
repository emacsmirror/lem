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

(defvar lem-instance-url "https://lemmy.ml")

(defun lem-get-site ()
  "Return detauls about instance at `lem-instance-url'."
  (let ((url (fedi-http--api "site")))
    (fedi-http--get-json url)))

(defun lem-list-communities ()
  "List communities on the instnance."
  (let ((url (fedi-http--api "community/list")))
    (fedi-http--get-json url)))

(defun lem-list-instance-posts ()
  "Get the instance's list of posts, like its home page."
  (let ((url (fedi-http--api "post/list")))
    (fedi-http--get-json url)))

(defun lem-get-community (id)
  "Get community with ID."
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "community")))
    (fedi-http--get-json url params)))

(defun lem-get-post (id)
  "Post with ID."
  (let* ((params `(("id" . ,id)))
         (url (fedi-http--api "post/list")))
    (fedi-http--get-json url params)))

(defun lem-get-community-posts (id)
  "Posts for community with ID."
  (let* ((params `(("community_id" . ,id)))
         (url (fedi-http--api "post/list")))
    (fedi-http--get-json url params)))

(defun lem-get-search (query)
  "Search for QUERY."
  (let ((params `(("q" . ,query)))
        (url (fedi-http--api "search")))
    (fedi-http--get-json url params)))

(provide 'lem)
;;; lem.el ends here
