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

;; Code hacked up together roughly, using:
;; <https://join-lemmy.org/api/classes/LemmyHttp.html>
;; <https://github.com/LemmyNet/lemmyBB/tree/main/src/api>
;; <https://github.com/LemmyNet/lemmy/blob/main/src/api_routes_http.rs>

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
(lem-request "get" "instance"
  "site" nil
  "Get instance details.
Returns a site_view, admins list, online count, version, my_user,
federated_instances, all_languages, discussion_languages, and
taglines.")

;; (lem-instance)

(lem-request "get" "get-instance-posts" "post/list"
             nil
             "Returns a list of posts.")

;; (lem-get-instance-posts)

(defun lem-get-federated-instances ()
  "Return a list of federated instances of the current instance.
Returns a list of linked, list of allowed, list of blocked."
  (let ((inst (setq lem-inst (lem-instance))))
    (alist-get 'federated_instances inst)))

;; (lem-get-federated-instances)

;;; SEARCH
(lem-request "get" "search"
  "search"
  (query &optional type community-name) ; community-id  creator-id
  ;; listing-type limit page sort)
  "Make a GET request to /search.
If TYPE is nil, returns comments, posts, communities and users
lists.
TYPE can be one of \"All\" \"Comments\" \"Communities\" \"Posts\"
\"Url\" or \"Users\".
COMMUNITY-ID and CREATOR-ID are numbers.
LISTING-TYPE is one of \"all\" \"community\" \"local\" or
\"subscribed\".
LIMIT and PAGE are numbers."
  `(("q" . ,query)
    ("type_" . ,(or type "All")) ; default
    ("community_name" . ,community-name)))
;; ("community_id" . ,community-id)
;; ("creator_id" . ,creator-id)
;; ("listing-type" . ,listing-type)))

;; (lem-search "emacs" nil "emacs")

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
Returns a person_view, comments, posts, moderates objects."
  `(("person_id" . ,id)))

;; (lem-get-person-by-id "8511")
;; (lem-get-person-by-id "899775")

(lem-request "get" "get-person-by-name"
  "user" (name)
  "Get person with NAME.
Returns a person_view, comments, posts, moderates objects."
  `(("username" . ,name)))

;; (lem-get-person-by-name "blawsybogsy")

;; TODO: block user

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
Returns a community_view, site, moderators, online count,
discussion_languages, default_post_language."
  `(("id" . ,id)))

;; (lem-get-community-by-id "96200")

(lem-request "get" "get-community-by-name"
  "community" (name)
  "Get community with NAME.
Returns a community_view, site, moderators, online count,
discussion_languages, default_post_language."
  `(("name" . ,name)))

;; (lem-get-community-by-name "lemel")

(lem-request "get" "get-communities" "community/list"
             nil "Returns a list of community objects.")

;; (lem-get-communities)

(lem-request "post" "follow-community"
  "community/follow" (community-id)
  "Follow a community with COMMUNITY-ID.
Returns a community_view and discussion_languages."
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
  "community"
  (name title &optional banner description discussion-languages
        icon nsfw mods-only-post)
  "Create a community with NAME.
Returns a community_view and discussion_languages."
  `(("name" . ,name)
    ("title" . ,title)
    ("banner" . ,banner)
    ("description" . ,description)
    ("discussion_languages" . ,discussion-languages)
    ("nsfw" . ,nsfw)
    ("icon" . ,icon)
    ("posting_restricted_to_mods" . ,mods-only-post))
  :json)

;; (lem-create-community "communeity" "com")

(lem-request "post" "delete-community"
  "community/delete" (community-id)
  "Delete community with COMMUNITY-ID, a number.
Returns a community_view and discussion_languages."
  `(("community_id" . ,community-id)
    ("deleted" . t))
  :json)

;; (lem-delete-community 98302)

;; TODO: block community
;; TODO: hide community

;;; POSTS
(lem-request "get" "get-post"
  "post" (id)
  "Get post with ID.
Returns a post_view, a community_view, moderators, and online count."
  `(("id" . ,id)))

;; (setq lem-test-post (lem-get-post "1341246"))

(lem-request "get" "list-posts"
  "post/list" (community-id &optional sort limit) ;page type
  "Get posts of community with COMMUNITY_ID.
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\".
LIMIT is the amount of results to return.
Retuns a list of posts objects."
  ;;  `Active`, `Hot`, `New`, `Old`, `TopDay`, `TopWeek`, `TopMonth`,
  ;;  `TopYear`, `TopAll`, `MostComments`, `NewComments`"
  `(("community_id" . ,community-id)
    ,(when sort `("sort" . ,sort))
    ,(when limit `("limit" . ,limit))))
;; ("page" . ,page)))

;; (setq lem-test-posts (lem-list-posts "14856"))

;; https://join-lemmy.org/api/interfaces/CreatePost.html
(lem-request "post" "create-post"
  "post"
  (name community-id &optional body url nsfw honeypot language-id)
  "Create a new post with NAME, on community with COMMUNITY-ID.
BODY is the post's content. URL is its link.
NSFW and HONEYPOT not yet implemented.
Returns a post_view."
  `(("community_id" . ,community-id)
    ("name" . ,name)
    ("body" . ,body)
    ("url" . ,url)
    ("nsfw" . ,nsfw)
    ("honeypot" . ,honeypot)
    ("language_id" . ,language-id))
  :json)

;; (lem-create-post "tootle on" 96200 "hooley-dooley") ; always cross-posts?

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (post (alist-get 'post (car json)))
;;        (name (alist-get 'name post)))
;;   (when name
;;     (format "Post created: %s" name)))

(lem-request "post" "like-post"
  "post/like" (post-id score)
  "Like post with POST-ID.
SCORE is a number, either 0, 1 to upvote, and -1 to downvote.
Returns a post_view."
  `(("post_id" . ,post-id)
    ("score" . ,score))
  :json)

;; (lem-like-post 1341246 1)

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
Returns a post_report_view."
  `(("post_id" . ,id)
    ("reason" . ,reason))
  :json)

;;; COMMENTS
(lem-request "get" "get-comment"
  "comment" (id)
  "Get comment with ID.
Returns a comment_view, recipient_ids, and form_id."
  `(("id" . ,id)))

;; (lem-get-comment "765662")

(lem-request "post" "create-comment"
  "comment" (post-id content &optional parent-id)
  "Create a comment on post POST-ID, with CONTENT.
PARENT-ID is the parent comment to reply to.
Returns a comment_view, recipient_ids, and form_id."
  `(("post_id" . ,post-id)
    ("content" . ,content)
    ("parent_id" . ,parent-id))
  :json)

;; (lem-create-comment 1341246 "replying via lem.el")

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comment (alist-get 'comment (car json))))
;;   (when comment
;;     (format "Comment created: %s" comment)))

(lem-request "get" "get-post-comments"
  "comment/list" (post-id &optional parent-id sort)
  ;; limit max_depth page saved_only type_
  "Get the comments of post with POST-ID.
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\".
Returns a list of comment objects."
  `(("post_id" . ,post-id)
    ("sort" . ,sort)
    ,(when parent-id `("parent_id" . ,parent-id))))

;; (setq lem-test-comments (lem-get-post-comments "1341246"))
;; (setq lem-test-comments (lem-get-post-comments "1235982"))
;; (lem-get-post-comments "1235982" "651145") ; nil first arg breaks


(lem-request "get" "get-community-comments"
  "comment/list" (&optional community-id community-name sort) ; limit
  ;; max_depth page saved_only type_
  "Get comments for community with COMMUNITY-ID.
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\".
Returns a list of comment objects."
  `(("comminuty_id" . ,community-id)
    ("sort" . ,sort)
    ("community_name" . ,community-name)))

;; (lem-get-community-comments "96200")
;; (lem-get-community-comments nil "emacs") ; nil first arg works

(lem-request "put" "edit-comment"
  "comment" (id new-str)
  "Edit comment with ID, providing content NEW-STR.
To get the old text for editing, you first need to fetch the comment.
  Returns a comment_view, recipient_ids, and form_id."
  `(("comment_id" . ,id)
    ("content" . ,new-str))
  :json)

;; (lem-edit-comment 765662 "tasdfl;k")

(lem-request "post" "report-comment"
  "comment/report" (id reason)
  "Report comment with ID to instance moderator, giving REASON, a string.
Returns comment_report_view."
  `(("comment_id" . ,id)
    ("reason" . ,reason))
  :json)

;; (lem-report-comment 765662 "test")

;;; PRIVATE MESSAGES
(lem-request "get" "get-private-messages"
  "private_message/list" ()
  "Get private messages for the current user.
Returns private_messages."
  `(("unread_only" . "true")))

;; (lem-get-private-messages)

(lem-request "post" "send-private-message"
  "private_message" (content recipient-id)
  "Sent a private message CONTENT to user with RECIPIENT-ID.
Returns a private_message_view."
  `(("content" . ,content)
    ("recipient_id" . ,recipient-id))
  :json)

;; (lem-send-private-message "test" 899775)


;; (lem-create-comment 1235982 "test" :json)
;; (setq lem-post-comments (lem-get-post-comments "1235982"))
;; (setq lem-comm (lem-community-posts "14856"))

;; eg ids:
;; emacs community: 14856
;; a post: 1235982 (emacs lemmy client?)
;; my first comment on 1235982: 651145
;; a comment on above post: 763989
;; lem.el test community: 96200
;; lem.el test community post: 1341246
;; user: blawsybogsy, 8511 on lemmy.ml
;; user: martian, 899775 on sh.itjust.works
;; (lem-create-comment 1341243 "comment")
;; (lem-create-post "title" 96200 "body text")
;; (lem-create-comment 1341246 "another body text 2")

(provide 'lem)
;;; lem.el ends here
