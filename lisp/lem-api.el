;;; lem-api.el --- Basics for a lemmy client library -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Keywords: multimedia, multimedia

;; This file is not part of GNU Emacs.

;; This file is part of lem.el.

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

;; Basic API functions for a Lemmy client library.

;; All functions return parsed JSON.

;; Because POST and PUT requests submit JSON payloads, while GET requests
;; submit form parameters, parameters in POST and PUT calls need to respect
;; types (numbers, boolean, etc.), while GET parameters can all be strings.
;; See the commented example calls under the definitions below. This should
;; probably be amended for consistency.

;; Code hacked up together roughly, using:
;; <https://join-lemmy.org/api/classes/LemmyHttp.html>
;; <https://github.com/LemmyNet/lemmyBB/tree/main/src/api>
;; <https://github.com/LemmyNet/lemmy/blob/main/src/api_routes_http.rs>

;; TODO: consider returning only the value of the objects returned, else
;; probably every request has to be followed by an (alist-get 'object object)
;; call. But some return a list of objects.

;; Lemmy API methods list:
;; addAdmin
;; addModToCommunity
;; approveRegistrationApplication
;; banFromCommunity
;; banPerson
;; blockCommunity DONE
;; blockPerson
;; changePassword
;; createComment DONE
;; createCommentReport
;; createCommunity DONE
;; createCustomEmoji
;; createPost DONE
;; createPostReport
;; createPrivateMessage DONE
;; createPrivateMessageReport
;; createSite
;; deleteAccount
;; deleteComment DONE
;; deleteCommunity DONE
;; deleteCustomEmoji
;; deletePost DONE
;; deletePrivateMessage
;; distinguishComment
;; editComment DONE
;; editCommunity TODO
;; editCustomEmoji
;; editPost DONE
;; editPrivateMessage TODO
;; editSite
;; featurePost
;; followCommunity DONE
;; getBannedPersons
;; getCaptcha
;; getComment DONE
;; getComments DONE
;; getCommunity DONE
;; getFederatedInstances DONE
;; getModlog
;; getPersonDetails DONE
;; getPersonMentions
;; getPost DONE
;; getPosts DONE
;; getPrivateMessages DONE
;; getReplies DONE
;; getReportCount
;; getSite
;; getSiteMetadata DONE
;; getUnreadCount
;; getUnreadRegistrationApplicationCount
;; leaveAdmin
;; likeComment DONE
;; likePost DONE
;; listCommentReports
;; listCommunities DONE
;; listPostReports
;; listPrivateMessageReports
;; listRegistrationApplications
;; lockPost
;; login DONE
;; markAllAsRead
;; markCommentReplyAsRead
;; markPersonMentionAsRead
;; markPostAsRead
;; markPrivateMessageAsRead
;; passwordChangeAfterReset
;; passwordReset
;; purgeComment
;; purgeCommunity
;; purgePerson
;; purgePost
;; register
;; removeComment
;; removeCommunity
;; removePost
;; resolveCommentReport
;; resolveObject
;; resolvePostReport
;; resolvePrivateMessageReport
;; saveComment
;; savePost
;; saveUserSettings
;; search DONE
;; transferCommunity
;; uploadImage
;; verifyEmail

;;; Code:

(require 'fedi)

(defvar fedi-http--api-version)
(setq fedi-http--api-version "v3")

(defvar fedi-instance-url)
(setq fedi-instance-url "https://lemmy.ml")

(setq fedi-package-prefix "lem")

;;; MACRO
(defmacro lem-request
    (method name endpoint
            &optional args docstring params man-params json headers unauthorized)
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
           (indent 3))
  `(fedi-request ,method ,name ,endpoint
     ,args ,docstring ,params
     ;; add auth param to manual-params:
     ,(unless unauthorized
        (append `(("auth" . ,lem-auth-token))
                man-params))
     ,json ,headers))

;;; INSTANCES
(lem-request "get" "get-instance" "site"
  ()
  "Get instance details.
Returns a site_view, admins list, online count, version, my_user,
federated_instances, all_languages, discussion_languages, and
taglines.")

;; (lem-instance)

(lem-request "get" "get-site-metadata" "post/site_metadata"
  (url)
  "Get site metadata for URL, any Lemmy instance."
  (url))

;; (lem-get-site-metadata "https://lemmy.world")

(defun lem-api-get-instance-posts (&optional type sort limit)
  "List posts for the current instance.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-posts type sort limit))

;; (setq lem-test-inst-posts (lem-get-instance-posts "Subscribed"))

(lem-request "get" "get-federated-instances" "federated_instances")

;; (lem-get-federated-instances)

;;; SEARCH
(lem-request "get" "search" "search"
  (q &optional type- listing-type community-name community-id) ;  creator-id
  ;; limit page sort)
  "Search for QUERY.
TYPE must be a member of `lem-search-types'. Defaults to All.
COMMUNITY-ID and CREATOR-ID are numbers.
LISTING-TYPE must be a member of `lem-listing-types'.
LIMIT and PAGE are numbers."
  (q type- listing-type community-name community-id)) ;  creator-id

;; (lem-search "emacs" "Posts")

;;; AUTH
(lem-request "post" "login" "user/login"
  (username-or-email password)
  "Log in to `fedi-instance-url' with NAME and PASSWORD."
  (username-or-email password)
  nil
  :json nil :unauthed)

;;; USERS / PERSON
(lem-request "get" "get-person" "user"
  (&optional username person-id community-id sort limit page) ; saved_only
  "Get person with ID.
Returns a person_view, comments, posts, moderates objects."
  (username person-id community-id sort limit page))

(defun lem-api-get-person-by-id (person-id) ; &optionals
  ""
  (lem-get-person nil person-id))

(defun lem-api-get-person-by-name (username)
  ""
  (lem-get-person username))

;; (lem-get-person-by-id "8511")
;; (lem-get-person-by-id "899775")

;; (setq lem-user-me (lem-get-person-by-name "blawsybogsy"))

;; TODO: block user

;;; NOTIFS
(lem-request "get" "get-mentions" "user/mention"
  (&optional unread-only)
  "Get mentions for the current user.
Returns a mentions list."
  (unread-only))

;; (lem-get-mentions "true")
;; (lem-get-mentions)

(lem-request "get" "get-replies" "user/replies"
  (&optional unread-only)
  "Get replies for the current user.
Returns a replies list."
  (unread-only))

;; (lem-get-replies "true")

;;; COMMUNITIES
(lem-request "get" "get-community" "community"
  (&optional id name)
  "Get community with ID or NAME.
Returns a community_view, site, moderators, online count,
discussion_languages, default_post_language."
  (id name))

;; (lem-get-community nil "96200")

(lem-request "get" "list-communities" "community/list"
  (&optional type- sort limit page)
  "Returns a list of community objects."
  (type- sort limit page))

;; (lem-list-communities "Subscribed")

(lem-request "post" "follow-community" "community/follow"
  (community-id)
  "Follow a community with COMMUNITY-ID.
Returns a community_view and discussion_languages."
  (community-id)
  (("follow" . t))
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

(lem-request "post" "create-community" "community"
  (name title &optional banner description discussion-languages
        icon nsfw mods-only-post)
  "Create a community with NAME.
Returns a community_view and discussion_languages."
  (name title banner description discussion-languages
        icon nsfw posting-restricted-to-mods)
  nil
  :json)

;; (lem-create-community "communeity" "com")

(lem-request "post" "delete-community" "community/delete"
  (community-id)
  "Delete community with COMMUNITY-ID, a number.
Returns a community_view and discussion_languages."
  (community-id)
  (("deleted" . t))
  :json)

;; (lem-delete-community 98302)


;; TODO: block community
(lem-request "post" "block-community" "community/block"
  (community-id)
  "Block community with COMMUNITY-ID"
  (community-id)
  (("block" . t))
  :json)

;; (lem-block-community 96200)

;; TODO: hide community

;;; POSTS
(lem-request "get" "get-post" "post"
  (id)
  "Get post with ID.
Returns a post_view, a community_view, moderators, and online count."
  (id))

;; (setq lem-test-post (lem-get-post "1341246"))

(lem-request "get" "get-posts" "post/list"
  (&optional type- sort limit community-id community-name) ; page saved_only
  "List posts for the args provided.
TYPE- must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
COMMUNITY-ID and COMMUNITY-NAME are the community to get posts from.
Without either arg, get instance posts."
  (type- sort limit community-id community-name))

;; (lem-get-posts "All")
;; (lem-get-posts "Subscribed" "Active")
;; (lem-get-posts "Local" "Hot" "2")

(defun lem-api-list-posts-community-by-id (community-id
                                           &optional type sort limit)
  "List posts for COMMUNITY-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-posts type sort limit community-id))


(defun lem-api-list-posts-community-by-name (community-name
                                             &optional type sort limit)
  "List posts for COMMUNITY-NAME.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-posts type sort limit nil community-name))

;; https://join-lemmy.org/api/interfaces/CreatePost.html
(lem-request "post" "create-post" "post"
  (name community-id &optional body url nsfw honeypot language-id)
  "Create a new post with NAME, on community with COMMUNITY-ID.
BODY is the post's content. URL is its link.
NSFW and HONEYPOT not yet implemented.
Returns a post_view."
  (name community-id body url nsfw honeypot language-id)
  nil
  :json)

;; (lem-create-post "tootle on" 96200 "hooley-dooley") ; always cross-posts?

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (post (alist-get 'post (car json)))
;;        (name (alist-get 'name post)))
;;   (when name
;;     (format "Post created: %s" name)))

(lem-request "post" "like-post" "post/like"
  (post-id score)
  "Like post with POST-ID.
SCORE is a number, either 0, 1 to upvote, and -1 to downvote.
Returns a post_view."
  (post-id score)
  nil
  :json)

;; (lem-like-post 1341246 1)

(lem-request "put" "edit-post" "post"
  (post-id name &optional body url) ; nsfw url lang-id
  "Edit post with ID, giving it a NEW-NAME, and NEW-BODY and NEW-URL.
Returns a post_view."
  (post-id name body url) ; nsfw url lang-id
  nil
  :json)

;; (lem-edit-post 1341246 "blaodh" "trep")

(lem-request "post" "delete-post" "post/delete"
  (post-id)
  ""
  (post-id)
  (("deleted" . t))
  :json)

;; (lem-delete-post 1341246)

(lem-request "post" "report-post" "post/report"
  (post-id reason)
  "Report post with ID to instance moderator, giving REASON, a string.
Returns a post_report_view."
  (post-id reason)
  nil
  :json)

;;; COMMENTS
;; <https://join-lemmy.org/api/interfaces/GetComments.html>
;; To get posts for a federated community by name, use name@instance.tld .

(lem-request "get" "get-comment" "comment"
  (id)
  "Get comment with ID.
Returns a comment_view, recipient_ids, and form_id."
  (id))

;; (lem-get-comment "765662")

(lem-request "post" "create-comment" "comment"
  (post-id content &optional parent-id)
  "Create a comment on post POST-ID, with CONTENT.
PARENT-ID is the parent comment to reply to.
Returns a comment_view, recipient_ids, and form_id."
  (post-id content parent-id)
  nil
  :json)

;; (lem-create-comment 1341246 "replying via lem.el")

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comment (alist-get 'comment (car json))))
;;   (when comment
;;     (format "Comment created: %s" comment)))

(lem-request "get" "get-comments" "comment/list"
  (&optional post-id parent-id type- sort limit
             ;; page saved_only
             community-id community-name)
  "SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
LIMIT is the amount of results to return.
COMMUNITY-ID and COMMUNITY-NAME are the community to get posts from.
Without any id or name, get instance comments."
  (post-id parent-id type- sort limit
           ;; page saved_only
           community-id community-name))

(defun lem-api-get-post-comments (post-id &optional type sort limit) ; page saved_only
  "Get comments for POST-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-comments post-id nil type sort limit))

;; (lem-get-post-comments "1235982" "All")

(defun lem-api-get-comment-children (parent-id &optional type sort limit) ; page saved_only
  "Get comments for PARENT-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-comments nil parent-id type sort limit))

(defun lem-api-get-community-comments-by-id (community-id &optional type sort limit) ; page saved_only
  "Get comments for COMMUNITY-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-comments nil nil type sort limit community-id))

(defun lem-api-get-community-comments-by-name
    (community-name &optional type sort limit) ; page saved_only
  "Get comments for COMMUNITY-NAME.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (lem-get-comments nil nil type sort limit nil community-name))

;; (lem-get-community-comments-by-id "96200")
;; (lem-get-community-comments-by-name "emacs")

(lem-request "put" "edit-comment" "comment"
  (comment-id content)
  "Edit comment with ID, providing content NEW-STR.
To get the old text for editing, you first need to fetch the comment.
  Returns a comment_view, recipient_ids, and form_id."
  (comment-id content)
  nil
  :json)

;; (lem-edit-comment 765662 "tasdfl;k")

(lem-request "post" "like-comment" "comment/like"
  (comment-id score)
  "Like comment with COMMENT-ID.
SCORE is a number, either 0, 1 to upvote, and -1 to downvote.
Returns a comment_view."
  (comment-id score)
  nil
  :json)

;; (lem-like-comment 765662 1)

(lem-request "post" "delete-comment" "comment/delete"
  (comment-id)
  ""
  (comment-id)
  (("deleted" . t))
  :json)

;; (lem-delete-comment 765662)

(lem-request "post" "report-comment" "comment/report"
  (comment-id reason)
  "Report comment with ID to instance moderator, giving REASON, a string.
Returns comment_report_view."
  (comment-id reason)
  nil
  :json)

;; (lem-report-comment 765662 "test") ; broken

;;; PRIVATE MESSAGES
(lem-request "get" "get-private-messages" "private_message/list"
  (&optional unread-only)
  "Get private messages for the current user.
Returns private_messages."
  (unread-only))

;; (lem-get-private-messages "true")

(lem-request "post" "send-private-message" "private_message"
  (content recipient-id)
  "Sent a private message CONTENT to user with RECIPIENT-ID.
Returns a private_message_view."
  (content recipient-id)
  nil
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

(provide 'lem-api)
;;; lem-api.el ends here
