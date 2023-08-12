;;; lem-api.el --- Basics for a lemmy client library -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem.el
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

;; Code hacked together using:
;; <https://join-lemmy.org/api/classes/LemmyHttp.html>
;; <https://github.com/LemmyNet/lemmyBB/tree/main/src/api>
;; <https://github.com/LemmyNet/lemmy/blob/main/src/api_routes_http.rs>

;; The signature of the functions aims to be like so:
;; (function main-arg [secondary-arg] &optional type sort limit page [tertiary args])
;; so that we can reliably handle type, sort, limit, page params

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
;; getSite DONE
;; getSiteMetadata DONE
;; getUnreadCount DONE
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
;; markAllAsRead DONE
;; markCommentReplyAsRead DONE
;; markPersonMentionAsRead TODO
;; markPostAsRead TODO
;; markPrivateMessageAsRead TODO
;; passwordChangeAfterReset
;; passwordReset
;; purgeComment
;; purgeCommunity
;; purgePerson
;; purgePost
;; register
;; removeComment TODO
;; removeCommunity TODO
;; removePost TODO
;; resolveCommentReport
;; resolveObject DONE
;; resolvePostReport
;; resolvePrivateMessageReport
;; saveComment DONE
;; savePost DONE
;; saveUserSettings
;; search DONE
;; transferCommunity
;; uploadImage TODO
;; verifyEmail

;;; Code:

(require 'fedi)

(defvar fedi-http--api-version)
(setq fedi-http--api-version "v3")

(setq fedi-package-prefix "lem")

(defvar lem-instance-url)
(setq lem-instance-url "https://lemmy.ml")

;;;###autoload
(defvar lem-auth-token nil
  "A user auth token for a lemmy instance.
Logging in will set this. You can also save it in your init.el.")

(autoload 'lem-auth-fetch-token "lem")

;;; MACRO
(defmacro lem-def-request
    (method name endpoint
            &optional args docstring params man-params headers
            unauthorized)
  "Create a http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function.

PARAMS is a plain list of elements from which to build an alist
of form parameters to send with the request. The name of the
corresponding arg must match the key of the parameter (i.e. if
the API parameter is \"person_id\", the corresponding arg, and
param in the request must both be \"person-id\").

MAN-PARAMS is an alist, to append to the one created from PARAMS.
They are manual, meaning that that the key and arg don't have to
be the same. This can be used for boolean parameters. If the
request sends encoded JSON data (ie POST or PUT), MAN-PARAMS
should be formatted as plain Emacs Lisp: \='((\"boolean\" . t))',
if the request sends query string parameters (GET, etc.), then
MAN-PARAMS should be formatted as strings only: \='((\"boolean\" .
\"true\"))'.

HEADERS is an alist that will be bound as `url-request-extra-headers'.

This macro is designed to generate functions for fetching data
from JSON APIs.

To use it, you first need to set `fedi-package-prefix' to the
name of your package, and set `lem-instance-url' to the URL of
an instance of your fedi service.

The name of functions generated with this will be the result of:
\(concat fedi-package-prefix \"-\" NAME).

The full URL for the endpoint is constructed by `fedi-http--api',
which see. ENDPOINT does not require a preceding slash.

For example, to define a GET request, called PKG-search to endpoint /search:

\(fedi-request \"get\" \"search\" \"search\"
  (q)
  \"Make a GET request.
Q is the search query.\"
  \\=(q))."
  (declare (debug t)
           (indent 3))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat fedi-package-prefix "-" name)) ,args
       ,docstring
       (let* ((req-url (fedi-http--api ,endpoint lem-instance-url))
              (url-request-method ,(upcase method))
              (url-request-extra-headers ,headers)
              (auth `(("auth" . ,(or lem-auth-token
                                     (lem-auth-fetch-token)))))
              (params-alist (remove nil
                                    (list ,@(fedi-make-params-alist
                                             params #'fedi-arg-when-expr))))
              (params (if ,man-params
                          (append ,man-params params-alist)
                        params-alist))
              (params (if ,unauthorized
                          params
                        (append auth params)))
              (response
               (cond ((or (equal ,method "post")
                          (equal ,method "put"))
                      ;; FIXME: deal with headers nil arg here:
                      (funcall #',req-fun req-url params nil :json))
                     (t
                      (funcall #',req-fun req-url params)))))
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                (fedi-http--process-json))))))))

;;; INSTANCES
(lem-def-request "get" "get-instance" "site"
  ()
  "Get instance details.
Returns a site_view, admins list, online count, version, my_user,
federated_instances, all_languages, discussion_languages, and
taglines.")

;; (lem-get-instance)

(declare-function lem-get-instance nil)

(defun lem-api-get-current-user ()
  "Get data for the current user, from the site endpoint.
Returns a local_user_view, containing local_user object, person
object, counts object, follows list containing community objects,
moderates list of community objects, community_blocks,
person_blocks, and discussion_languages."
  (let ((site (lem-get-instance)))
    (alist-get 'my_user site)))

(defun lem-api-get-subscribed-communities ()
  "Return the data of the current user's subscribed communities.
Returns follows data, from under my_user, from the site endpoint."
  (let* ((current-user (lem-api-get-current-user))
         (fols (alist-get 'follows current-user)))
    fols))

;; no auth: because we call this before sending the instance our creds:
(lem-def-request "get" "get-site" "site")

;; (lem-get-site)

(lem-def-request "get" "get-site-metadata" "post/site_metadata"
  (url)
  "Get site metadata for URL, any Lemmy instance."
  (url))

;; (lem-get-site-metadata "https://lemmy.world")

(defun lem-api-get-instance-posts (&optional type sort limit page)
  "List posts for the current instance.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1."
  (lem-get-posts type sort limit page))

;; (setq lem-test-inst-posts (lem-api-get-instance-posts "Subscribed"))

(lem-def-request "get" "get-federated-instances" "federated_instances")

;; (lem-get-federated-instances)

;;; SEARCH
(lem-def-request "get" "search" "search"
  (q &optional type- listing-type sort limit page community-name community-id) ;  creator-id
  "Search for QUERY.
TYPE- must be a member of `lem-search-types'. Defaults to All.
COMMUNITY-ID and CREATOR-ID are numbers.
LISTING-TYPE must be a member of `lem-listing-types'.
LIMIT and PAGE are numbers."
  (q type- listing-type sort limit page community-name community-id)) ;  creator-id

(declare-function lem-search nil)

;; (lem-search "emacs" "Posts")
(defun lem-api-search (q type)
  "Search for Q.
TYPE must be a member of `lem-search-types'. Defaults to All."
  (lem-search q type))

(defun lem-api-search-users
    (q &optional listing-type sort limit page community-name community-id) ;  creator-id
  "Search for Q, returning users.
LISTING-TYPE, SORT, LIMIT, PAGE, COMMUNITY-NAME, and COMMUNITY-ID
are for `lem-search'."
  (lem-search q "Users" listing-type sort limit page community-name community-id))

(defun lem-api-search-posts
    (q &optional listing-type sort limit page community-name community-id) ;  creator-id
  "Search for Q, returning posts.
LISTING-TYPE, SORT, LIMIT, PAGE, COMMUNITY-NAME, and COMMUNITY-ID
are for `lem-search'."
  (lem-search q "Posts" listing-type sort limit page community-name community-id))

(defun lem-api-search-communities
    (q &optional listing-type sort limit page community-name community-id) ;  creator-id
  "Search for Q, returning communities.
LISTING-TYPE, SORT, LIMIT, PAGE, COMMUNITY-NAME, and COMMUNITY-ID
are for `lem-search'."
  (lem-search q "Communities" listing-type sort limit page community-name community-id))

(defun lem-api-search-comments
    (q &optional listing-type sort limit page community-name community-id) ;  creator-id
  "Search for Q, returning comments.
LISTING-TYPE, SORT, LIMIT, PAGE, COMMUNITY-NAME, and COMMUNITY-ID
are for `lem-search'."
  (lem-search q "Comments" listing-type sort limit page community-name community-id))

(lem-def-request "get" "resolve-object" "resolve_object"
  (q)
  "Do a webfinger lookup for query Q."
  (q))

;; (lem-resolve-object "https://lemmy.ml/u/blawsybogsy")
;; (lem-resolve-object "https://lemmy.ml/c/canada@lemmy.ca") ; foreign instance fails
;; (lem-resolve-object "https://lemmy.ml/c/canada")

;;; AUTH
(lem-def-request "post" "login" "user/login"
  (username-or-email password)
  "Log in to `lem-instance-url' with NAME and PASSWORD."
  (username-or-email password)
  nil nil :unauthed)

;;; USERS / PERSON
(lem-def-request "get" "get-person" "user"
  (&optional username person-id sort limit page community-id saved-only)
  "Get person with ID.
Returns a person_view, comments, posts, moderates objects."
  (username person-id sort limit page community-id)
  ;; FIXME: this requires json string, while other params require plain lisp:
  (when saved-only
    '(("saved_only" . "true"))))

(declare-function lem-get-person nil)

;; (lem-get-person nil "8511" nil nil nil nil)
;; (lem-get-person nil "8511" nil nil nil nil :saved-only)

(defun lem-api-get-person-saved-only (person-id &optional sort limit page)
  "Get person with PERSON-ID, saved only.
SORT, LIMIT, PAGE are all for `lem-get-person'."
  (lem-get-person nil person-id sort limit page nil :saved-only))

;; (setq lem-saved-only-test (lem-api-get-person-saved-only "8511"))

(defun lem-api-get-person-by-id (person-id &optional sort limit page)
  "Get person with PERSON-ID.
SORT, LIMIT, PAGE are all for `lem-get-person'."
  (lem-get-person nil person-id sort limit page))

(defun lem-api-get-person-by-name (username &optional sort limit page)
  "Get person with USERNAME.
SORT, LIMIT, PAGE are all for `lem-get-person'."
  (lem-get-person username nil sort limit page))

(defun lem-api-get-person-posts (person-id &optional sort limit page)
  "Get the posts of person with PERSON-ID.
SORT, LIMIT, PAGE are all for `lem-get-person'."
  (let ((person (lem-api-get-person-by-id person-id sort limit page)))
    (list (assoc 'posts person))))

(defun lem-api-get-person-comments (person-id &optional sort limit page)
  "Get the comments of person with PERSON-ID.
SORT, LIMIT, PAGE are all for `lem-get-person'."
  (let ((person (lem-api-get-person-by-id person-id sort limit page)))
    (list (assoc 'comments person))))

;; (lem-api-get-person-by-id "8511")
;; (lem-api-get-person-by-id "899775")

;; (setq lem-user-me (lem-api-get-person-by-name "blawsybogsy"))

(lem-def-request "post" "block-user" "user/block"
  (person-id block)
  "Block user with PERSON-ID.
Returns a person_view plus a blocked boolean."
  (person-id)
  `(("block" . ,block)))

;; (lem-block-person ??)

;;; NOTIFS

(lem-def-request "get" "get-mentions" "user/mention"
  (&optional unread-only)
  "Get mentions for the current user.
Returns a mentions list.
UNREAD-ONLY means to only return unread items."
  nil
  (when unread-only
    '(("unread_only" . "true"))))

;; (lem-get-mentions :unread)
;; (lem-get-mentions)

(lem-def-request "get" "get-replies" "user/replies"
  (&optional unread-only)
  "Get replies for the current user.
Returns a list of comment_reply objects.
UNREAD-ONLY means to only return unread items."
  nil
  (when unread-only
    '(("unread_only" . "true"))))

;; (lem-get-replies :unread)
;; (lem-get-replies)

(lem-def-request "post" "mark-comment-reply-read" "comment/mark_as_read"
  (comment-reply-id)
  "Mark comment reply with COMMENT-REPLY-ID as read."
  (comment-reply-id)
  '(("read" . t)))

;; (lem-mark-comment-reply-read 433366)

(lem-def-request "post" "mark-all-read" "user/mark_all_as_read"
  ()
  "Mark all replies(, mentions and private messages?) as read.")

;; (lem-mark-all-read) ; returns replies, maybe only marks them read?

;;; COMMUNITIES
(lem-def-request "get" "get-community" "community"
  (&optional id name)
  "Get community with ID or NAME.
Returns a community_view, site, moderators, online count,
discussion_languages, default_post_language."
  (id name))

;; (lem-get-community "96200" nil)
;; (lem-get-community nil "revanced@lemmy.world")

(lem-def-request "get" "list-communities" "community/list"
  (&optional type- sort limit page)
  "Returns a list of community objects."
  (type- sort limit page))

;; (lem-list-communities "All")
;; (lem-list-communities "Subscribed")
;; (lem-list-communities "Local")

(lem-def-request "post" "follow-community" "community/follow"
  (community-id follow)
  "Follow a community with COMMUNITY-ID.
Returns a community_view and discussion_languages."
  (community-id)
  `(("follow" . ,follow)))

;; (lem-follow-community 14711 t)
;; (lem-follow-community 88259 t)
;; (lem-follow-community 88259 :json-false)

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comm (alist-get 'community (car json)))
;;        (subed (alist-get 'subscribed (car json)))
;;        (name (alist-get 'name comm))
;;        (desc (alist-get 'description comm)))
;;   (when (equal subed "Subscribed")
;;     (format "Subscribed to %s [%s]" name desc)))

(lem-def-request "post" "create-community" "community"
  (name title &optional banner description discussion-languages
        icon nsfw posting-restricted-to-mods)
  "Create a community with NAME.
Returns a community_view and discussion_languages."
  (name title banner description discussion-languages
        icon nsfw posting-restricted-to-mods))

;; (lem-create-community "communeity" "com")

(lem-def-request "post" "delete-community" "community/delete"
  (community-id delete)
  "Delete community with COMMUNITY-ID, a number.
Returns a community_view and discussion_languages."
  (community-id)
  `(("deleted" . ,delete)))

;; (lem-delete-community 98302 t)
;; (lem-delete-community 98302 :json-false)

(lem-def-request "post" "block-community" "community/block"
  (community-id block)
  "Block community with COMMUNITY-ID.
Returns a community_view plus a blocked boolean."
  (community-id)
  `(("block" . ,block)))

;; (lem-block-community 96200 t)
;; (lem-block-community 96200 :json-false)

;; TODO: hide community

;;; POSTS
(lem-def-request "get" "get-post" "post"
  (id)
  "Get post with ID.
Returns a post_view, a community_view, moderators, and online count."
  (id))

;; (setq lem-test-post (lem-get-post "1341246"))

(lem-def-request "get" "get-posts" "post/list"
  (&optional type- sort limit page community-id community-name saved-only)
  "List posts for the args provided.
TYPE- must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
COMMUNITY-ID and COMMUNITY-NAME are the community to get posts from.
Without either arg, get instance posts."
  (type- sort limit page community-id community-name)
  (when saved-only
    '(("saved_only" . "true"))))

;; (lem-get-posts "All")
;; (lem-get-posts "Subscribed" "Active")
;; (lem-get-posts "Subscribed" "Hot" "2")
;; (lem-get-posts "Local" "Hot" "2")
;; (lem-get-posts nil nil nil "86881" nil "2")
;; (lem-get-posts "All" nil nil nil nil nil :saved)

(declare-function lem-get-posts nil)

(defun lem-api-get-community-posts-by-id (community-id
                                          &optional type sort limit page)
  "List posts for COMMUNITY-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed to 1."
  (lem-get-posts type sort limit page community-id))

;; (lem-api-list-posts-community-by-id "14856")

(defun lem-api-get-community-posts-by-name (community-name
                                            &optional type sort limit page)
  "List posts for COMMUNITY-NAME.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1."
  (lem-get-posts type sort limit page nil community-name))

;; https://join-lemmy.org/api/interfaces/CreatePost.html
(lem-def-request "post" "create-post" "post"
  (name community-id &optional body url nsfw honeypot language-id)
  "Create a new post with NAME, on community with COMMUNITY-ID.
BODY is the post's content. URL is its link.
NSFW and HONEYPOT not yet implemented.
Returns a post_view."
  (name community-id body url nsfw honeypot language-id))

;; (lem-create-post "tootle on" 96200 "hooley-dooley") ; always cross-posts?

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (post (alist-get 'post (car json)))
;;        (name (alist-get 'name post)))
;;   (when name
;;     (format "Post created: %s" name)))

(lem-def-request "post" "like-post" "post/like"
  (post-id score)
  "Like post with POST-ID.
SCORE is a number, either 0, 1 to upvote, and -1 to downvote.
Returns a post_view."
  (post-id score))

;; (lem-like-post 1341246 1)

(lem-def-request "put" "edit-post" "post"
  (post-id name &optional body url) ; nsfw url lang-id
  "Edit post with ID, giving it a NEW-NAME, and NEW-BODY and NEW-URL.
Returns a post_view."
  (post-id name body url)) ; nsfw url lang-id

;; (lem-edit-post 1341246 "blaodh" "trep")

(lem-def-request "post" "delete-post" "post/delete"
  (post-id deleted)
  ""
  (post-id)
  `(("deleted" . ,deleted)))

;; (lem-delete-post 1635706 t)
;; (lem-delete-post 1635706 :json-false)

(lem-def-request "post" "report-post" "post/report"
  (post-id reason)
  "Report post with ID to instance moderator, giving REASON, a string.
Returns a post_report_view."
  (post-id reason))

;;; COMMENTS
;; <https://join-lemmy.org/api/interfaces/GetComments.html>
;; To get posts for a federated community by name, use name@instance.tld .

(lem-def-request "get" "get-comment" "comment"
  (id)
  "Get comment with ID.
Returns a comment_view, recipient_ids, and form_id."
  (id))

;; (lem-get-comment "765662")

(lem-def-request "post" "create-comment" "comment"
  (post-id content &optional parent-id)
  "Create a comment on post POST-ID, with CONTENT.
PARENT-ID is the parent comment to reply to.
Returns a comment_view, recipient_ids, and form_id."
  (post-id content parent-id))

;; (lem-create-comment 1367490 "toot toot")
;; (lem-create-comment 1341246 "replying via lem.el")

;; cb:
;; (let* ((json (fedi-http--process-json))
;;        (comment (alist-get 'comment (car json))))
;;   (when comment
;;     (format "Comment created: %s" comment)))

(lem-def-request "get" "get-comments" "comment/list"
  (&optional post-id parent-id type- sort limit page
             community-id community-name saved-only)
  "SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
LIMIT is the amount of results to return.
COMMUNITY-ID and COMMUNITY-NAME are the community to get posts from.
Without any id or name, get instance comments."
  (post-id parent-id type- sort limit page
           community-id community-name)
  (when saved-only
    '(("saved_only" . "true"))))

(declare-function lem-get-comments nil)

;; (lem-get-comments "1694468")

(defun lem-api-get-community-comments (community-id
                                       &optional type sort limit page)
  "Get comments of community with COMMUNITY-ID.
TYPE, SORT, LIMIT and PAGE are all for `lem-get-comments'."
  (lem-get-comments nil nil type sort limit page community-id))

(defun lem-api-get-post-comments (post-id &optional type sort limit page saved-only)
  "Get comments for POST-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1.
SAVED-ONLY means to only return saved items."
  (lem-get-comments post-id nil type sort limit page nil nil saved-only))

;; (lem-get-post-comments "1485706" "All")
;; (lem-api-get-post-comments "44280" "All")
;; (lem-get-post-comments "1235982" "All")
;; (lem-api-get-post-comments "1865094" "All" nil "50" 2)

(defun lem-api-get-comment-children (parent-id
                                     &optional type sort limit page saved-only)
  "Get comments for PARENT-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1.
SAVED-ONLY means to only return saved items."
  (lem-get-comments nil parent-id type sort limit page nil nil saved-only))

(defun lem-api-get-community-comments-by-id (community-id
                                             &optional type sort limit page saved-only)
  "Get comments for COMMUNITY-ID.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1.
SAVED-ONLY means to only return saved items."
  (lem-get-comments nil nil type sort limit page community-id nil saved-only))

(defun lem-api-get-community-comments-by-name
    (community-name &optional type sort limit page saved-only)
  "Get comments for COMMUNITY-NAME.
TYPE must be member of `lem-listing-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is a number, indexed at 1.
SAVED-ONLY means to only return saved items."
  (lem-get-comments nil nil type sort limit page nil community-name saved-only))

;; (lem-get-community-comments-by-id "96200")
;; (lem-get-community-comments-by-name "emacs")

(lem-def-request "put" "edit-comment" "comment"
  (comment-id content)
  "Edit comment with COMMENT-ID, providing content NEW-STR.
To get the old text for editing, you first need to fetch the comment.
Returns a comment_view, recipient_ids, and form_id."
  (comment-id content))

;; (lem-edit-comment 765662 "tasdfl;k")

(lem-def-request "post" "like-comment" "comment/like"
  (comment-id score)
  "Like comment with COMMENT-ID.
SCORE is a number, either 0, 1 to upvote, and -1 to downvote.
Returns a comment_view."
  (comment-id score))

;; (lem-like-comment 765662 1)

(lem-def-request "post" "delete-comment" "comment/delete"
  (comment-id deleted)
  ""
  (comment-id)
  `(("deleted" . ,deleted)))

;; (lem-delete-comment 765662 t)
;; (lem-delete-comment 765662 :json-false)

(lem-def-request "post" "report-comment" "comment/report"
  (comment-id reason)
  "Report comment with COMMENT-ID to instance moderator, giving REASON, a string.
Returns comment_report_view."
  (comment-id reason))

;; (lem-report-comment 765662 "test") ; broken

;;; PRIVATE MESSAGES
(lem-def-request "get" "get-private-messages" "private_message/list"
  (&optional unread-only)
  "Get private messages for the current user.
UNREAD-ONLY means only return unread messages.
Returns private_messages."
  (unread-only))

;; (lem-get-private-messages "true")
;; (lem-get-private-messages)

(lem-def-request "get" "get-unread-count" "user/unread_count")

;; (lem-get-unread-count)

(lem-def-request "post" "send-private-message" "private_message"
  (content recipient-id)
  "Sent a private message CONTENT to user with RECIPIENT-ID.
Returns a private_message_view."
  (content recipient-id))

;; (lem-send-private-message "test" 899775)

(lem-def-request "post" "mark-private-message-read"
                 "private_message/mark_as_read"
  (private-message-id)
  "Mark private message with id PRIVATE-MESSAGE-ID as read."
  (private-message-id)
  '(("read" . t)))

;; (lem-create-comment 1235982 "test")
;; (setq lem-post-comments (lem-get-post-comments "1235982"))
;; (setq lem-comm (lem-community-posts "14856"))

;;; SAVING

(lem-def-request "put" "save-post" "post/save"
  (post-id save)
  "Save post with POST-ID, a number."
  (post-id)
  `(("save" . ,save)))

(lem-def-request "put" "save-comment" "comment/save"
  (comment-id save)
  "Save comment with COMMENT-ID, a number."
  (comment-id)
  `(("save" . ,save)))

;;; ASYNC

(defalias 'lem-api-get-async #'fedi-http--get-async)
(defalias 'lem-api-get-json-async #'fedi-http--get-json-async)

(defalias 'lem-api-post-async #'fedi-http--post-async)

;; (lem-api-get-json-async (fedi-http--api "site")
;;                         nil (lambda (status)
;;                               (message status)))


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
