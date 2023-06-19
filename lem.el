;;; lem.el --- Basics for a lemmy client library -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Package-Requires: ((emacs "27.1") (fedi "0.1"))
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

;; Currently, POST and PUT requests submit JSON payloads, while GET requests
;; submit form parameters. This means that parameters in POST and PUT calls
;; need to respect types (numbers, boolean, etc.), while GET parameters can
;; all be strings. See the commented example calls under the definitions
;; below. This should probably be amended for consistency.

;;; Code:

(require 'fedi)
(require 'persist)

(defvar fedi-http--api-version)
(setq fedi-http--api-version "v3")

(defvar fedi-instance-url)
(setq fedi-instance-url "https://lemmy.ml")

(persist-defvar lem-auth-token nil
                "A user auth token for a lemmy instance.")

(setq fedi-package-prefix "lem")

;;; MACRO
(defmacro lem-request
    (method name endpoint
            &optional args docstring params json headers unauthorized)
  "Create http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function.
PARAMS is an alist of form parameters to send with the request.
AUTHORIZED means submit an auth alist to params.
JSON means to encode params as a JSON payload.
HEADERS is an alist that will be bound as `url-request-extra-headers'.
To use this macro, you first need to set `fedi-package-prefix' to
the name of your package.
See `fedi-request'."
  (declare (debug t)
           (indent 2))
  `(fedi-request ,method
       ,name ,endpoint ,args ,docstring ,params
       (unless ,unauthorized `(("auth" . ,lem-auth-token))) ,json ,headers))

;;; INSTANCE
(lem-request "get" "instance" "site"
             nil
             "Returns a site_view.")

;; (lem-instance)

(lem-request "get" "get-instance-posts" "post/list"
             nil
             "Returns a list of posts.")

;; (lem-get-instance-posts)

;;; SEARCH
(lem-request "get" "search"
  "search" (query)
  "Make a GET request to /search.
Returns comments, posts, communities and users lists."
  `(("q" . ,query)))

;; (lem-search "emacs")

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

;; (lem-community-search)

;;; AUTH
(lem-request "post" "login"
  "user/login" (name password)
  "Log in to `fedi-instance-url' with NAME and PASSWORD."
  `(("username_or_email" . ,name)
    ("password" . ,password))
  :json nil :unauthed)

(defun lem-login-set-token (name password)
  "Login for user NAME with PASSWORD."
  (interactive)
  (let ((json (lem-login name password)))
    (setq lem-auth-token (alist-get 'jwt json))))

;;; USERS
(lem-request "get" "get-person-by-id"
  "user" (id)
  "Get person with ID.
Returns a person_view."
  `(("person_id" . ,id)))

;; (lem-get-person-by-id "8511")

(lem-request "get" "get-person-by-name"
  "user" (name)
  "Get person with NAME.
Returns a person_view."
  `(("username" . ,name)))

;; (lem-get-person-by-name "blawsybogsy")

;;; NOTIFS
(lem-request "get" "get-mentions"
  "user/mention" () ; (&optional unread-only)
  "Get mentions for the current user.
Returns a mentions list."
  `(("unread_only" . "true")))

;; (lem-get-mentions)

(lem-request "get" "get-replies"
  "user/replies" () ; (&optional unread-only)
  "Get replies for the current user.
Returns a replies list."
  `(("unread_only" . "true")))

;; (lem-get-replies)

;;; COMMUNITIES
(lem-request "get" "get-community-by-id"
  "community" (id)
  "Get community with ID.
Returns a community_view."
  `(("id" . ,id)))

;; (lem-get-community-by-id "96200")

(lem-request "get" "get-community-by-name"
  "community" (name)
  "Get community with NAME.
Returns a community_view."
  `(("name" . ,name)))

;; (lem-get-community-by-name "lemel")

(lem-request "get" "get-communities" "community/list"
             nil "Returns a list of communities.")

;; (lem-get-communities)

(lem-request "post" "follow-community"
  "community/follow" (community-id)
  "Follow a community with COMMUNITY-ID.
Returns a community_view."
  `(("community_id" . ,community-id)
    ("follow" . t))
  :json)

;; (lem-follow-community 14711)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comm (alist-get 'community (car json)))
;;        (subed (alist-get 'subscribed (car json)))
;;        (name (alist-get 'name comm))
;;        (desc (alist-get 'description comm)))
;;   (when (equal subed "Subscribed")
;;     (format "Subscribed to %s [%s]" name desc)))

(lem-request "post" "create-community"
  "community" (name)
  "Create a community with NAME."
  `(("name" . ,name)
    ("title" . ,name)))

;; (lem-create-community "created-comm-une-ity") ; broken

;; TODO: DeleteCommunity

;;; POSTS
(lem-request "get" "get-post"
  "post" (id)
  "Get post with ID.
Returns a post_view."
  `(("id" . ,id)))

;; (lem-get-post "1341246")

(lem-request "get" "list-posts"
  "post/list" (community-id) ; &optional limit page sort type
  "Get posts of community with COMMUNITY_ID.
Retuns a posts list."
  `(("community_id" . ,community-id)))
;; ("limit" . ,limit)
;; ("page" . ,page)))

;; (lem-list-posts "96200")

(lem-request "post" "create-post"
  "post"
  (name community-id &optional body url nsfw honeypot) ; lang-id
  "Create a new post with NAME, on community with COMMUNITY-ID.
BODY is the post's content. URL is its link.
NSFW and HONEYPOT not yet implemented."
  `(("community_id" . ,community-id)
    ("name" . ,name)
    ("body" . ,body)
    ("url" . ,url)
    ("nsfw" . ,nsfw)
    ("honeypot" . ,honeypot))
  :json)

;; (lem-create-post "tootle on" 96200 "hooley-dooley") ; broken! always cross-posts

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (post (alist-get 'post (car json)))
;;        (name (alist-get 'name post)))
;;   (when name
;;     (format "Post created: %s" name)))

(lem-request "post" "like-post"
  "post/like" (post-id score)
  "Like post with POST-ID.
SCORE.
Returns a post_view."
  `(("post_id" . ,post-id)
    ("score" . ,score))
  :json)

;; (lem-like-post 1341246 1) ; dunno how scoring works

;; TODO: edit post
(lem-request "put" "edit-post"
  "post" (id new-name &optional new-body new-url) ; nsfw url lang-id
  "Edit post with ID, giving it a NEW-NAME, and NEW-BODY and NEW-URL.
Returns a post_view."
  `(("post_id" . ,id)
    ("name" . ,new-name)
    ("body" . ,new-body)
    ("url" . ,new-url))
  :json)

;; (lem-edit-post 1341246 "blaodh" "trep")

(lem-request "post" "report-post"
  "post/report" (id reason)
  "Report post with ID to instance moderator, giving REASON, a string.
Returns ????"
  `(("post_id" . ,id)
    ("reason" . ,reason))
  :json)

;;; COMMENTS
(lem-request "get" "get-comment"
  "comment" (id)
  "Get comment with ID.
Returns a comment_view."
  `(("id" . ,id)))

;; (lem-get-comment "765662")

(lem-request "post" "create-comment"
  "comment" (post-id content &optional parent-id)
  `(("post_id" . ,post-id)
    ("content" . ,content)
    ("parent_id" . ,parent-id))
  :json)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comment (alist-get 'comment (car json))))
;;   (when comment
;;     (format "Comment created: %s" comment)))

(lem-request "get" "get-post-comments"
  "comment/list" (post-id)
  "Get the comments of post with POST-ID.
Returns a list of comments."
  `(("post_id" . ,post-id)))

;; (lem-get-post-comments "1341246")

(lem-request "get" "get-community-comments"
  "comment/list" (community-id) ; &optional sort limit
  "Get comments for community with COMMUNITY-ID.
Returns a list of comments."
  `(("comminuty_id" . ,community-id)))

;; (lem-get-community-comments "96200")

;; TODO: edit comment
(lem-request "put" "edit-comment"
  "comment" (id new-str)
  "Edit comment with ID, providing content NEW-STR.
Returns a comment_view."
  `(("comment_id" . ,id)
    ("content" . ,new-str))
  :json)

;; (lem-edit-comment 765662 "tasdfl;k")

(lem-request "post" "report-comment"
  "comment/report" (id reason)
  "Report comment with ID to instance moderator, giving REASON, a string.
Returns ????"
  `(("comment_id" . ,id)
    ("reason" . ,reason))
  :json)

;; (lem-report-comment 765662 "test")

;;; PRIVATE MESSAGES
(lem-request "get" "get-private-messages"
  "private_message/list" ()
  "Get private messages for the current user."
  `(("unread_only" . "true")))

;; (lem-get-private-messages)

(lem-request "post" "send-private-message"
  "private_message" (content recipient-id)
  "Sent a private message CONTENT to user with RECIPIENT-ID."
  `(("content" . ,content)
    ("recipient_id" . ,recipient-id))
  :json)

;; (lem-send-private-message "test" 8551)


;; (lem-create-comment 1235982 "test" :json)
;; (setq lem-post-comments (lem-get-post-comments "1235982"))
;; (setq lem-comm (lem-community-posts "14856"))

;; eg ids:
;; emacs community: 14856
;; a post: 1235982 (emacs lemmy client?)
;; a comment on above post: 763989
;; lem.el test community: 96200
;; lem.el test community post: 1341246
;; user: blawsybogsy, 8511
;; (lem-create-comment 1341243 "comment")
;; (lem-create-post "title" 96200 "body text")
;; (lem-create-comment 1341246 "another body text 2")

(provide 'lem)
;;; lem.el ends here
