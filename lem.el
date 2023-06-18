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

;; Basic functions for a Lemmy client library.

;; All functions return parsed JSON.

;;; Code:

(require 'fedi-http)
(require 'persist)

(setq fedi-http--api-version "v3")

(defvar fedi-instance-url "https://lemmy.ml")

(persist-defvar lem-auth-token nil
                "A user auth token for a lemmy instance.")

;;; MACRO
(defmacro lem-def-request (method name endpoint &optional args params json)
  "Create http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function, PARAMS is an alist of form parameters.
JSON means to send params as a JSON payload."
  (declare (debug t)
           (indent 1))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat "lem-" name)) ,args
       (let* ((url (fedi-http--api ,endpoint))
              (response
               (cond ((equal ,method "post")
                      (funcall #',req-fun url ,params nil :unauthed ,json))
                     ((equal ,method "get")
                      (funcall #',req-fun url ,params)))))
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                (fedi-http--process-json))))))))

;;; INSTANCE
(lem-def-request "get" "instance" "site")

;; (lem-instance)

(lem-def-request "get" "get-instance-posts" "post/list")

;; (lem-get-instance-posts)

;;; SEARCH
(lem-def-request "get"
  "search" "search"
  (query)
  `(("q" . ,query)))

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
    (lem-get-community choice))) ; returns community_view, its own info
;; (lem-community-posts choice))) ; returns community's posts

;;; AUTH
(lem-def-request "post"
  "login" "user/login"
  (name password)
  `(("username_or_email" . ,name)
    ("password" . ,password))
  :json)

(defun lem-login-set-token (name password)
  "Login for user NAME with PASSWORD."
  (interactive)
  (let ((json (lem-login name password)))
    (setq lem-auth-token (alist-get 'jwt json))))

;;; USERS
(lem-def-request "get"
  "get-person-by-id" "user"
  (id)
  `(("person_id" . ,id)))

;; (lem-get-person-by-id "8511")

(lem-def-request "get"
  "get-person-by-name" "user"
  (name)
  `(("username" . ,name)))

;; (lem-get-person-by-name "blawsybogsy")

;;; NOTIFS
(lem-def-request "get"
  "get-mentions" "user/mention"
  () ; (&optional unread-only)
  `(("auth" . ,lem-auth-token)
    ("unread_only" . "true")))

;; (lem-get-mentions)

(lem-def-request "get"
  "get-replies" "user/replies"
  () ; (&optional unread-only)
  `(("auth" . ,lem-auth-token)
    ("unread_only" . "true")))

;; (lem-get-replies)

;;; COMMUNITIES
(lem-def-request "get"
  "get-community" "community"
  (id)
  `(("id" . ,id)
    ("auth" . ,lem-auth-token)))

;; (lem-get-community "96200")

(lem-def-request "get" "get-communities" "community/list")

;; (lem-get-communities)

(lem-def-request "post"
  "follow-community" "community/follow"
  (community-id)
  `(("community_id" . ,community-id)
    ("auth" . ,lem-auth-token)
    ("follow" . t))
  :json)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comm (alist-get 'community (car json)))
;;        (subed (alist-get 'subscribed (car json)))
;;        (name (alist-get 'name comm))
;;        (desc (alist-get 'description comm)))
;;   (when (equal subed "Subscribed")
;;     (format "Subscribed to %s [%s]" name desc)))


;; TODO: CreateCommunity
;; TODO: DeleteCommunity

;;; POSTS
(lem-def-request "get"
  "get-post" "post"
  (id)
  `(("id" . ,id)
    ("auth" . ,lem-auth-token)))

;; (lem-get-post "1341246")

(lem-def-request "get"
  "list-posts" "post/list"
  (community-id) ; &optional limit page sort type
  `(("community_id" . ,community-id)
    ("auth" . ,lem-auth-token)))
;; ("limit" . ,limit)
;; ("page" . ,page)))

;; (lem-list-posts "96200")

(lem-def-request "post"
  "create-post" "post"
  (name community-id &optional body url nsfw honeypot) ; lang-id
  `(("community_id" . ,community-id)
    ("auth" . ,lem-auth-token)
    ("name" . ,name)
    ("body" . ,body)
    ("url" . ,url)
    ("nsfw" . ,nsfw)
    ("honeypot" . ,honeypot))
  :json)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (post (alist-get 'post (car json)))
;;        (name (alist-get 'name post)))
;;   (when name
;;     (format "Post created: %s" name)))

(lem-def-request "get"
  "like-post" "post/like"
  (id)
  `(("id" . ,id)
    ("auth" . ,lem-auth-token)))

;; (lem-like-post "1341264")
;; (lem-like-post "1341246")
;; (lem-like-post "765662")

;; (defun lem-like-post (id)
;;   "Like post with ID. Requires auth."
;;   (let ((params `(("id" . ,id)
;;                   ("auth" . ,lem-auth-token)))
;;         (url (fedi-http--api "post/like")))
;;     (fedi-http--get-json url params)))

;; TODO: edit post

(lem-def-request "post"
  "report-post" "post/report"
  (id reason)
  `(("post_id" . ,id)
    ("reason" . ,reason)
    ("auth" . ,lem-auth-token))
  :json)

;;; COMMENTS
(lem-def-request "get"
  "get-comment" "comment"
  (id)
  `(("id" . ,id)
    ("auth" . ,lem-auth-token)))

;; (lem-get-comment "765662")

(lem-def-request "post"
  "create-comment" "comment"
  (post-id content &optional parent-id)
  `(("post_id" . ,post-id)
    ("auth" . ,lem-auth-token)
    ("content" . ,content)
    ("parent_id" . ,parent-id))
  :json)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comment (alist-get 'comment (car json))))
;;   (when comment
;;     (format "Comment created: %s" comment)))

(lem-def-request "get"
  "get-post-comments" "comment/list"
  (post-id)
  `(("post_id" . ,post-id)
    ("auth" . ,lem-auth-token)))

;; (lem-get-post-comments "1341246")

;; TODO: edit comment
;; TODO: report comment
;; TODO: list community comments



;; (lem-create-comment 1235982 "test" :json)
;; (setq lem-post-comments (lem-get-post-comments "1235982"))
;; (setq lem-comm (lem-community-posts "14856"))

;; eg ids:
;; emacs community: 14856
;; a post: 1235982 (emacs lemmy client?)
;; a comment on above post: 763989
;; lem.el test community: 96200
;; lem.el test community post: 1341246

;; (lem-create-comment 1341243 "comment")
;; (lem-create-post "title" 96200 "body text")
;; (lem-create-comment 1341246 "another body text 2")

(provide 'lem)
;;; lem.el ends here
