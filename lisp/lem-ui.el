;;; lem-ui.el --- Basics for a lemmy interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Keywords: multimedia, comm, web, fediverse

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

;; Some simple, unadorned, primitive, humble, basic, dashed-off functions for an interface to Lemmy, the federated link-aggregator and forum software. See <https://joinlemmy.org>.

;;; Code:

(require 'hierarchy)
(require 'lem-api)

(defvar lem-ui-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

;;; UTILITIES

(defcustom lem-ui-symbols
  '((reply     . ("💬" . "R"))
    (boost     . ("🔁" . "B"))
    (favourite . ("⭐" . "F"))
    (bookmark  . ("🔖" . "K"))
    (media     . ("📹" . "[media]"))
    (verified  . ("" . "V"))
    (locked    . ("🔒" . "[locked]"))
    (private   . ("🔒" . "[followers]"))
    (direct    . ("✉" . "[direct]"))
    (edited    . ("✍" . "[edited]"))
    (upvote    . ("⬆" . "[upvotes]"))
    (person    . ("👤" . "[people]"))
    (pinned    . ("📌" . "[pinned]"))
    (replied   . ("⬇" . "↓"))
    (community . ("👪" . "[community]"))
    (reply-bar . ("┃" . "|")))
  "A set of symbols (and fallback strings) to be used in timeline.
If a symbol does not look right (tofu), it means your
font settings do not support it."
  :type '(alist :key-type symbol :value-type string))

(defun lem-ui-symbol (name)
  "Return the unicode symbol (as a string) corresponding to NAME.
If symbol is not displayable, an ASCII equivalent is returned. If
NAME is not part of the symbol table, '?' is returned."
  (if-let* ((symbol (alist-get name lem-ui-symbols)))
      (if (char-displayable-p (string-to-char (car symbol)))
          (car symbol)
        (cdr symbol))
    "?"))

(defun lem-ui-font-lock-comment (&rest strs)
  "Font lock comment face STR."
  (propertize (mapconcat #'identity strs "")
              'face font-lock-comment-face))

(defun lem-ui-thing-json ()
  "Get json of thing at point, comment, post, community or user."
  (get-text-property (point) 'json))

(defun lem-ui--property (prop)
  "Get text property PROP from item at point."
  (get-text-property (point) prop))

(defun lem-ui--item-type ()
  "Return the type property of item at point."
  (lem-ui--property 'type))

(defun lem-ui--get-id (&optional string type)
  "Return id as a string, from alist KEY in JSON.
SLOT is a symbol, either post, comment, user, or community.
STRING means return as string, else return number.
TYPE is the name of the ID property to get."
  (let ((id (lem-ui--property (or type 'id))))
    (if string
        (number-to-string id)
      id)))

;;; MACROS
(defmacro lem-ui-with-buffer (buffer mode-fun other-window &rest body)
  "Evaluate BODY in a new or existing buffer called BUFFER.
MODE-FUN is called to set the major mode.
OTHER-WINDOW means call `switch-to-buffer-other-window' rather
than `switch-to-buffer'."
  (declare (debug t)
           (indent 3))
  `(with-current-buffer (get-buffer-create ,buffer)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (funcall ,mode-fun)
       (if ,other-window
           (switch-to-buffer-other-window ,buffer)
         (switch-to-buffer ,buffer))
       ,@body
       (goto-char (point-min)))))

(defmacro lem-ui-with-id (body &optional number)
  "Call BODY after fetching ID of thing (at point).
Thing can be anything handled by `lem-ui-thing-json', currently:
comment, post, community or person.
Within this macro call, args JSON and ID are available."
  (declare (debug 'body)
           (indent 1))
  `(let* ((json (lem-ui-thing-json))
          (id (lem-ui--get-id (if ,number nil :string))))
     ,body))

;;; BUFFER DETAILS
(defvar-local lem-ui-buffer-spec nil
  "A plist containing details about the current lem buffer.")

(defun lem-ui-set-buffer-spec (&optional listing-type sort view-fun item page)
  "Set `lem-ui-buffer-spec' for the current buffer.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
ITEM is a symbol, either posts or comments."
  (setq lem-ui-buffer-spec
        `(:sort ,sort :listing-type ,listing-type :view-fun ,view-fun
                :item-type ,item :page ,page)))

(defun lem-ui-get-buffer-spec (key)
  "Return value of KEY in `lem-ui-buffer-spec'."
  (plist-get lem-ui-buffer-spec key))

;;; NAV

(defun lem--goto-pos (fun &optional refresh pos)
  "Search for item with FUN.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (let* ((npos (funcall fun
                        (or pos (point))
                        'byline-top
                        (current-buffer))))
    (if npos
        (if (not (get-text-property npos 'byline-top))
            (lem--goto-pos fun refresh npos)
          (goto-char npos))
      ;; force display of help-echo on moving to a toot byline:
      ;; (lem--message-help-echo))
      (funcall refresh))))

(defun lem-next-item ()
  "Move to next item."
  (interactive)
  (lem--goto-pos #'next-single-property-change))

(defun lem-prev-item ()
  "Move to prev item."
  (interactive)
  (lem--goto-pos #'previous-single-property-change))

(defun lem-ui-view-thing-at-point ()
  "View post, community or user at point."
  (interactive)
  (let ((type (lem-ui--item-type)))
    (cond ((eq type 'post)
           (lem-ui-view-post-at-point))
          ((eq type 'community)
           (lem-ui-view-community-at-point))
          ((eq type 'comment)
           (lem-ui-view-comment-post))
          ((eq type 'user)
           (lem-ui-view-user-at-point)))))

(defun lem-ui-view-item-user-at-point ()
  "."
  (let ((id (get-text-property (point) 'creator-id)))
    (if id
        (lem-ui-view-user id 'overview)
      (message "No item at point?"))))

;;; INSTANCE

;; TODO: toggle posts or comments
(defun lem-ui-view-instance (&optional type sort limit page)
  "View posts of current user's home instance.
SORT must be a member of `lem-sort-types'.
TYPE must be member of `lem-listing-types'.
LIMIT is the amount of results to return."
  (let* ((instance (lem-get-instance))
         (posts (lem-get-posts type sort limit nil nil page)) ; sort here too?
         (posts (alist-get 'posts posts))
         (buf (get-buffer-create "*lem-instance*")))
    (lem-ui-with-buffer buf 'lem-mode nil
      (lem-ui-render-instance instance :stats)
      (lem-ui-render-posts posts sort :community :trim)
      (lem-ui-set-buffer-spec type sort #'lem-ui-view-instance page)
      (goto-char (point-min)))))

(defun lem-ui-view-instance-full (args)
  "View view instance details."
  ;; TODO: full instance info: sidebar, full desc,
  ;; trending communities, stats, admins
  )

(defun lem-ui-view-modlog (args)
  "docstring"
  ;; TODO
  )

(defun lem-ui-render-instance (instance &optional stats)
  "INSTANCE.
STATS."
  (let ((inst (alist-get 'site_view instance)))
    (let-alist inst
      (insert
       (propertize
        (concat
         (propertize .site.name
                     'face '(:weight bold))
         " | "
         (lem-ui-font-lock-comment .site.actor_id)
         (lem-ui-font-lock-comment " created: " .site.published)
         "\n"
         .site.description
         "\n"
         lem-ui-horiz-bar
         "\n")
        'json instance
        'byline-top t ; next/prev hack
        'id .site.id
        'type 'instance)))
    ;; stats:
    (when stats
      (let-alist (alist-get 'counts inst)
        (lem-ui-render-stats .users
                             .posts
                             .comments
                             .communities)))
    ;; admins:
    ;; TODO: refactor mods/admins display:
    (let* ((admins-list (alist-get 'admins instance))
           (admins (mapcar (lambda (x)
                             (let-alist (alist-get 'person x)
                               (list (number-to-string .id)
                                     (or .display_name .name) .actor_id)))
                           admins-list)))
      (when admins
        (insert "admins: "
                (mapconcat (lambda (x)
                             (mapconcat #'identity x " "))
                           admins " | ")
                "\n"
                lem-ui-horiz-bar
                "\n")))
    (insert "\n")))

;;; VIEWS SORTING AND TYPES

(defun lem-ui-get-view-id ()
  "Get id of the view item, a post or user."
  (save-excursion
    (goto-char (point-min))
    (lem-ui--get-id :string)))

(defun lem-ui-cycle-funcall (fun type sort call-type &optional id post-p)
  "Cal FUN with args TYPE SORT and ID.
CALL-TYPE is listing or sort.
POST-P means we are cycling a post view (which has no type)."
  (let ((str (if (eq call-type 'listing) type sort)))
    (if id
        (progn
          (if post-p
              (funcall fun id sort)
            (funcall fun id type sort))
          (message "%s: %s" call-type str))
      (funcall fun type sort)
      (message "%s: %s" call-type str))))

(defun lem-ui-cycle-listing-type ()
  "Cycle view between `lem-listing-types'.
For a user view, cycle between overview, posts and comments.
For a community view, cycle between posts and comments."
  ;; FIXME: remove posts/comments from this logic/binding
  (interactive)
  (let* ((type (lem-ui-get-buffer-spec :listing-type))
         (item-type (lem-ui-get-buffer-spec :item-type))
         (sort (lem-ui-get-buffer-spec :sort))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         (id (lem-ui-get-view-id))
         (user-p (eq view-fun #'lem-ui-view-user))
         (post-p (eq view-fun #'lem-ui-view-post))
         (community-p (eq view-fun #'lem-ui-view-community)))
    ;; TODO: refactor
    (cond (user-p
           (cond ((equal type "overview")
                  (lem-ui-cycle-funcall view-fun
                                        "posts" sort 'listing id))
                 ((equal type "posts")
                  (lem-ui-cycle-funcall view-fun
                                        "comments" sort 'listing id))
                 (t ; comments or nil
                  (lem-ui-cycle-funcall view-fun
                                        "overview" sort 'listing id))))
          (community-p
           (if (not (eq item-type 'comments))
               (lem-ui-cycle-funcall view-fun 'comments sort 'listing id)
             (lem-ui-cycle-funcall view-fun 'posts sort 'listing id)))
          (post-p
           (message "Post views don't have listing type."))
          (t
           (if (or (equal type (car (last lem-listing-types)))
                   (null type))
               (lem-ui-cycle-funcall
                view-fun (car lem-listing-types) sort 'listing)
             (lem-ui-cycle-funcall
              view-fun (cadr (member type lem-listing-types)) sort 'listing))))))

(defun lem-ui-cycle-sort ()
  "Cycle view between some `lem-sort-types'.
For post view, use `lem-comment-sort-types'."
  (interactive)
  (let* ((type (lem-ui-get-buffer-spec :listing-type))
         (sort (lem-ui-get-buffer-spec :sort))
         (sort-rest (member sort lem-sort-types))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         (id (lem-ui-get-view-id))
         (post-p (eq view-fun #'lem-ui-view-post))
         (user-p (eq view-fun #'lem-ui-view-user))
         (community-p (eq view-fun #'lem-ui-view-community)))
    (cond ((or user-p community-p)
           (cond ((or (equal sort (car (last lem-sort-types)))
                      (null sort))
                  (lem-ui-cycle-funcall
                   view-fun type (car lem-sort-types) 'sort id))
                 (t
                  (lem-ui-cycle-funcall view-fun type
                                        (cadr sort-rest) 'sort id))))
          (post-p
           (cond ((or (equal sort (car (last lem-comment-sort-types)))
                      (null sort))
                  (lem-ui-cycle-funcall
                   view-fun type (car lem-comment-sort-types) 'sort id :post))
                 (t
                  (lem-ui-cycle-funcall
                   view-fun type (cadr (member sort lem-comment-sort-types))
                   'sort id :post))))
          (t
           (cond ((or (equal sort (car (last lem-sort-types)))
                      (null sort))
                  (lem-ui-cycle-funcall
                   view-fun type (car lem-sort-types) 'sort))
                 (t
                  (lem-ui-cycle-funcall
                   view-fun type (cadr sort-rest) 'sort)))))))

(defun lem-ui-sort-or-type (sort-or-type view-fun &optional id)
  "Reload current view, setting SORT-OR-TYPE, with VIEW-FUN.
ID is the main view item's id."
  (let* ((type (lem-ui-get-buffer-spec :listing-type))
         (sort (lem-ui-get-buffer-spec :sort))
         (post-p (eq view-fun #'lem-ui-view-post))
         (user-p (eq view-fun #'lem-ui-view-user))
         (sort-list (if post-p lem-comment-sort-types
                      lem-sort-types))
         (type-list (if user-p
                        lem-user-view-types
                      lem-listing-types))
         (list (if (eq sort-or-type 'type)
                   type-list
                 sort-list))
         (choice (completing-read (format "View by %s" sort-or-type)
                                  list nil :match)))
    (if id
        (if (eq sort-or-type 'type)
            (funcall view-fun id choice sort)
          (funcall view-fun id type choice))
      (if (eq sort-or-type 'type)
          (funcall view-fun choice sort)
        (funcall view-fun type choice)))))

(defun lem-ui-call-sort-or-type (sort-or-type)
  "Call `lem-ui-call-or-type', with id arg if needed.
SORT-OR-TYPE is either sort or type."
  (let ((view-fun (lem-ui-get-buffer-spec :view-fun))
        (id (lem-ui-get-view-id)))
    (if (and (eq view-fun #'lem-ui-view-post)
             (eq sort-or-type 'type))
        (message "Post views don't have listing type.")
      (if (or (eq view-fun #'lem-ui-view-post)
              (eq view-fun #'lem-ui-view-user)
              (eq view-fun #'lem-ui-view-community))
          (lem-ui-sort-or-type sort-or-type view-fun id)
        (lem-ui-sort-or-type sort-or-type view-fun)))))

(defun lem-ui-choose-type ()
  "Read a listing type and load it."
  (interactive)
  (lem-ui-call-sort-or-type 'type))

(defun lem-ui-choose-sort ()
  "Read a sort type and load it."
  (interactive)
  (lem-ui-call-sort-or-type 'sort))

(defun lem-ui-read-type (prompt types-list)
  "Read a choice from TYPES-LIST using PROMPT."
  (completing-read prompt
                   types-list nil :match))

(defun lem-ui-search (&optional limit)
  "Do a search for one of the types in `lem-search-types'."
  (interactive)
  (let* ((type (lem-ui-read-type "Search type: " lem-search-types))
         ;; LISTING/SORT doesn't make sense for all search types, eg users!:
         (listing-type (lem-ui-read-type "Listing type: " lem-listing-types))
         (sort (lem-ui-read-type "Sort by: " lem-sort-types))
         (limit (or limit (read-string "Results [max 50]: ")))
         (query (read-string "Query: "))
         ;; TODO: handle all search args: community, page, limit
         (response (lem-search query type listing-type sort limit)))
    ;; TODO: render other responses:
    ;; ("All" "Comments" "Posts" "Communities" "Users" "Url")
    (cond ((equal type "Users")
           (let ((users (alist-get 'users response))
                 (buf (get-buffer-create "*lem-users*")))
             (lem-ui-with-buffer buf 'lem-mode nil
               (lem-ui-render-users users))))
          ((equal type "Communities")
           (let ((communities (alist-get 'communities response))
                 (buf (get-buffer-create "*lem-communities*")))
             (lem-ui-with-buffer buf 'lem-mode nil
               (lem-ui-render-communities communities))))
          ((equal type "Posts")
           (let ((posts (alist-get 'posts response))
                 (buf (get-buffer-create "*lem-posts*")))
             (lem-ui-with-buffer buf 'lem-mode nil
               (lem-ui-render-posts posts)))))))

;;; POSTS

(defun lem-ui-view-post-at-point ()
  "."
  (interactive)
  (lem-ui-with-id
      (lem-ui-view-post id)))

(defun lem-ui-view-post (id &optional sort limit)
  "View post with ID.
SORT must be a member of `lem-comment-sort-types.'
LIMIT."
  (let* ((post-view (lem-get-post id))
         (post (alist-get 'post_view post-view)))
    (lem-ui-with-buffer (get-buffer-create "*lem-post*") 'lem-mode nil
      (lem-ui-render-post post sort :community)
      (lem-ui-render-post-comments id)
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-post)
      (goto-char (point-min))))) ; limit

(defvar lem-ui--link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [return] #'lem-ui--follow-link-at-point)
    (define-key map [mouse-2] #'lem-ui--follow-link-at-point)
    (define-key map [follow-link] 'mouse-face)
    map)
  "The keymap for link-like things in buffer (except for shr.el generate links).
This will make the region of text act like like a link with mouse
highlighting, mouse click action tabbing to next/previous link
etc.")

(defun lem-ui--follow-link-at-point ()
  "Follow link at point."
  (interactive)
  (let ((id (lem-ui--get-id :string 'id))
        (creator-id (lem-ui--get-id :string 'creator-id))
        (community-id (lem-ui--get-id :string 'community-id))
        (item-type (get-text-property (point) 'lem-tab-stop)))
    (cond ((eq item-type 'community)
           (lem-ui-view-community community-id))
          ((eq item-type 'user)
           (lem-ui-view-user creator-id)))))

(defun lem-ui-top-byline (name score timestamp
                               &optional community community-url featured-p)
  "Format a top byline for post with NAME, SCORE and TIMESTAMP.
COMMUNITY and COMMUNITY-URL are those of the community the item belongs to.
FEATURED-P means the item is pinned."
  (propertize
   (concat
    (propertize name
                ;; 'shr-url user-url
                'keymap lem-ui--link-map
                'button t
                'category 'shr
                'follow-link t
                'mouse-face 'highlight
                'lem-tab-stop 'user
                'face 'underline)
    (when community
      (concat
       (propertize " to "
                   'face font-lock-comment-face)
       (propertize community
                   'shr-url community-url
                   'keymap lem-ui--link-map
                   'button t
                   'category 'shr
                   'follow-link t
                   'face 'underline
                   'lem-tab-stop 'community
                   'mouse-face 'highlight)))
    (propertize
     (concat
      " | "
      (lem-ui-symbol 'upvote) " "
      (number-to-string score) " | "
      timestamp
      (if (eq featured-p t)
          (concat " | "
                  (lem-ui-symbol 'pinned))
        ""))
     'face font-lock-comment-face))
   'byline-top t))

(defun lem-ui-bt-byline (comments &optional id)
  "Format a bottom byline for a post or comment.
COMMENTS is the comments count to render.
ID is the item's id."
  (propertize
   (format "%s %s | %s" (lem-ui-symbol 'reply)
           (number-to-string comments)
           (propertize (concat "id: "
                               (number-to-string id))
                       'face font-lock-comment-face))
   'byline-bottom t))

(defun lem-ui--render-url (url)
  "Render URL, a plain non-html string."
  (when url
    (let ((parsed (url-generic-parse-url url))
          rendered)
      (with-temp-buffer
        (insert "<a href=" url ">" (url-host parsed) "</a>")
        (shr-render-buffer (current-buffer))
        (setq rendered (buffer-string))
        (kill-buffer-and-window))
      rendered)))

(defun lem-ui-render-body (body)
  "Render post BODY as markdowned html."
  (let ((buf "*lem-md*")
        str)
    (with-temp-buffer
      (insert body)
      (markdown-standalone buf)
      (with-current-buffer buf
        (shr-render-buffer (current-buffer))
        (re-search-forward "\n\n" nil :no-error)
        (setq str (buffer-substring (point) (point-max)))
        (kill-buffer-and-window) ; shr's *html*
        (kill-buffer buf))) ; our md
    str))

(defun lem-ui-render-post (post &optional sort community trim)
  ;; NB trim in instance, community, and user views
  ;; NB show community info in instance and in post views
  "Render single POST.
Optionally render post's COMMUNITY.
Optionally TRIM post length.
SORT must be a member of `lem-sort-types'."
  (let-alist post
    (let ((url (lem-ui--render-url .post.url))
          (body (when .post.body
                  (lem-ui-render-body .post.body))))
      (insert
       (propertize
        (concat
         "\n"
         (lem-ui-top-byline .creator.name
                            .counts.score
                            .post.published
                            (when community .community.name)
                            (when community .community.actor_id)
                            .post.featured_local)
         "\n"
         (propertize .post.name
                     'face '(:weight bold))
         "\n"
         (if url
             (concat url "\n\n")
           "")
         (if .post.body
             (if trim
                 (string-limit body 400)
               body)
           "")
         "\n"
         (lem-ui-bt-byline .counts.comments .post.id)
         "\n"
         lem-ui-horiz-bar
         "\n\n")
        'json post
        'id .post.id
        'community-id .post.community_id
        'creator-id .creator.id
        'type (caar post))))))

(defun lem-ui-render-posts (posts &optional sort community trim)
  "Render a list of posts POSTS in BUFFER.
Used for instance, communities, posts, and users.
COMMENTS means also show post comments.
SORT is the kind of sorting to use.
COMMUNITY means display what community it was posted to.
TRIM means trim each post for length."
  (cl-loop for x in posts
           do (lem-ui-render-post x sort community trim)))

(defun lem-ui-save-item ()
  "Save item at point.
Saved items can be viewed in your profile, like bookmarks."
  (interactive)
  (let ((id (lem-ui--get-id))
        (type (lem-ui--item-type)))
    (cond ((eq type 'post)
           (lem-save-post id)
           (message "%s %s saved!" type id))
          ((eq type 'comment)
           (lem-api-save-comment id)
           (message "%s %s saved!" type id))
          (t
           (message "You can only save posts and comments.")))))

(defun lem-ui-view-saved-posts (&optional id)
  "View saved posts of the current user, or of user with ID."
  (interactive)
  (let* ((saved-only (lem-api-get-person-saved-only
                      (number-to-string (or id lem-user-id))))
         (posts (alist-get 'posts saved-only))
         (buffer (format "*lem-saved-posts*")))
    (lem-ui-with-buffer (get-buffer-create buffer) 'lem-mode nil
      (lem-ui-render-posts posts buffer)
      (goto-char (point-min)))))

;;; CREATE A POST

(defun lem-ui-new-post-simple ()
  "."
  (interactive)
  (let* ((name (read-string "Post title: "))
         (communities (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-alist communities))
         (choice (completing-read "Community: "
                                  list))
         (community-id (string-to-number
                        (alist-get choice list nil nil #'equal)))
         (body (read-string "Post body [optional]: "))
         (url (read-string "URL [optional]: "))
         (response
          (lem-create-post name community-id body
                           (when (not (equal "" url))
                             url))))
    ;; TODO: nsfw, etc.
    (when response
      (let-alist response
        (message "Post %s created!" .post_view.post.name)))))

;;; COMMUNITIES

(defun lem-ui-view-communities (&optional type sort limit)
  "View Lemmy communities.
TYPE must be one of `lem-listing-types'.
SORT must be one of `lem-sort-types'."
  (interactive)
  ;; FIXME: we rly don't need to have to ask for these now that we have type
  ;; cycling. Make Customize defaults or just set them and go.
  (let* ((type (or type (completing-read "View communities: "
                                         lem-listing-types)))
         (sort (or sort (completing-read "Sorted by: " ; or custom default
                                         lem-sort-types)))
         (limit (or limit (read-string "Number of results [max 50]: ")))
         (json (lem-list-communities type sort limit))
         (list (alist-get 'communities json))
         (buffer (format "*lem-communities*")))
    (lem-ui-with-buffer (get-buffer-create buffer) 'lem-mode nil
      (cl-loop for c in list
               for id = (alist-get 'id (alist-get 'community c))
               for view = (lem-get-community (number-to-string id) nil)
               do (lem-ui-render-community view buffer :stats :view))
      (goto-char (point-min)))))

(defun lem-ui-subscribe-to-community-at-point ()
  "."
  (interactive)
  (lem-ui-with-id
      ;; TODO: needs feedback!
      (lem-follow-community id)
    :number))

(defun lem-ui-view-community-at-point ()
  "."
  (interactive)
  (lem-ui-with-id
      (let ((community (lem-get-community id)))
        (lem-ui-view-community id))))

(defun lem-ui--communities-alist (communities)
  "Return an alist of name/description and ID from COMMUNITIES."
  (cl-loop for item in (alist-get 'communities communities)
           collect (let-alist item
                     (cons (concat .community.name " | "
                                   (string-limit .community.description 40))
                           (number-to-string .community.id)))))

(defun lem-ui-jump-to-subscribed ()
  "Prompt for a subscribed community and view it."
  (interactive)
  (let* ((communities ;(setq lem-test-c (car (alist-get 'communities
          (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-alist communities))
         (choice (completing-read "Jump to community: "
                                  list))
         (id (alist-get choice list nil nil #'equal)))
    (lem-ui-view-community id 'posts)))

(defun lem-ui-view-community (id &optional item sort limit page)
  "View community with ID.
ITEM is a symbol, either posts or comments.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is the page number of items to display, a string."
  (let* ((community (lem-get-community id))
         (view (alist-get 'community_view community))
         (buf (get-buffer-create "*lem-community*"))
         ;; in case we set community posts, then switch to comments:
         (sort (if (eq item 'comments)
                   (unless (lem-comment-sort-type-p sort)
                     (car lem-comment-sort-types))
                 sort))
         (items (if (eq item 'comments)
                    (alist-get 'comments
                               (lem-get-comments nil nil nil sort limit id nil page))
                  (alist-get 'posts
                             (lem-get-posts nil sort limit id nil page))))) ; no sorting
    (lem-ui-with-buffer buf 'lem-mode nil
      (lem-ui-render-community community nil :stats :view)
      (if (eq item 'comments)
          (progn
            (insert (lem-ui-format-heading "comments"))
            (lem-ui-render-comments items nil sort))
        (insert (lem-ui-format-heading "posts"))
        (lem-ui-render-posts items buf sort)) ; no children
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-community item page)
      (goto-char (point-min)))))

(defun lem-ui-get-community-id (community &optional string)
  "Return ID of COMMUNITY.
If STRING, return one, else number."
  (let ((id
         (alist-get 'id
                    (alist-get 'community
                               (alist-get 'community_view community)))))
    (if string
        (number-to-string id)
      id)))

(defun lem-ui-render-communities (communities &optional type sort)
  "Render COMMUNITIES.
TYPE
SORT."
  (cl-loop for x in communities
           do (lem-ui-render-community x :stats)))

(defun lem-ui-render-community (community &optional stats view)
  "Render header details for COMMUNITY.
BUFFER is the one to render in, a string.
STATS are the community's stats to print.
VIEW means COMMUNITY is a community_view."
  (let ((community (if view
                       (alist-get 'community_view community)
                     community)))
    (let-alist community
      (let ((desc (if view
                      (when .community.description
                        (lem-ui-render-body .community.description))
                    (when .description
                      (lem-ui-render-body .description)))))
        (insert
         (propertize
          (concat
           (propertize .community.title
                       'face '(:weight bold))
           " | "
           (lem-ui-font-lock-comment .community.name)
           "\n"
           (lem-ui-font-lock-comment .community.actor_id)
           "\n"
           desc
           ;; .community.description
           "\n"
           lem-ui-horiz-bar
           "\n")
          'json community
          'byline-top t ; next/prev hack
          'id .community.id
          'type 'community))) ;(caar community)))
      ;; stats:
      (when stats
        (lem-ui-render-stats .counts.subscribers
                             .counts.posts
                             .counts.comments))
      (insert .subscribed "\n"))
    ;; mods:
    (let* ((mods-list (alist-get 'moderators community))
           (mods (mapcar (lambda (x)
                           (let-alist (alist-get 'moderator x)
                             (list (number-to-string .id)
                                   (or .display_name .name) .actor_id)))
                         mods-list)))
      (when mods
        (insert "mods: "
                (mapconcat (lambda (x)
                             (mapconcat #'identity x " "))
                           mods " | ")
                "\n"
                lem-ui-horiz-bar
                "\n")))
    (insert "\n")))

(defun lem-ui-render-stats (subscribers posts comments
                                        &optional communities)
  "Render stats for SUBSCRIBERS, POSTS, COMMENTS.
And optionally for instance COMMUNITIES."
  (let ((s (number-to-string subscribers))
        (s-sym (lem-ui-symbol 'person))
        (p (number-to-string posts))
        (p-sym (lem-ui-symbol 'direct))
        (c (number-to-string comments))
        (c-sym (lem-ui-symbol 'reply))
        (ties (if communities (number-to-string communities) ""))
        (ties-sym (if communities (lem-ui-symbol 'community) "")))
    (insert
     (format "%s %s | %s %s | %s %s | %s %s\n" s-sym s p-sym p c-sym c ties-sym ties))))

(defun lem-ui-view-item-community ()
  "View community of item at point."
  (interactive)
  (let ((id (get-text-property (point) 'community-id)))
    (if id
        (let* ((str (number-to-string id)))
          (lem-ui-view-community str))
      ("Not item at point?"))))

;;; REPLIES

(defun lem-ui-reply-simple ()
  "Reply to post or comment at point.
Simple means we just read a string."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (type (lem-ui--item-type))
         (content (read-string "Reply: "))
         (post-id (if (equal type 'post)
                      (lem-ui--get-id)
                    (when-let ((post (alist-get 'post json)))
                      (alist-get 'id post))))
         (comment-id (when (equal type 'comment)
                       (when-let ((comment (alist-get 'comment json)))
                         (alist-get 'id comment))))
         (response (lem-create-comment post-id content comment-id)))
    (when response
      (let-alist response
        (message "Comment created: %s" .comment_view.comment.content)))))

(defun lem-ui-view-replies (&optional unread)
  "View reply comments to the current user."
  (interactive)
  (let* ((replies (lem-get-replies unread))
         (list (alist-get 'replies replies))
         (buf (get-buffer-create "*lem-replies*")))
    (lem-ui-with-buffer buf 'lem-mode nil
      (lem-ui-render-replies list))))

(defun lem-ui-render-replies (replies)
  "Render REPLIES, reply comments to the current user."
  (cl-loop for reply in replies
           do (lem-ui-render-comment reply)))

;;; COMMENTS

(defun lem-ui-render-comment (comment &optional sort)
  "Render single COMMENT.
SORT must be a member of `lem-comment-sort-types'."
  (let-alist comment
    (let ((content (when .comment.content
                     (lem-ui-render-body .comment.content))))
      (insert
       (propertize
        (concat
         "\n"
         (lem-ui-top-byline .creator.name
                            .counts.score
                            .comment.published)
         "\n"
         (or content "")
         "\n"
         (lem-ui-bt-byline .counts.child_count .comment.id)
         "\n"
         lem-ui-horiz-bar
         "\n")
        'json comment
        'id .comment.id
        'post-id .comment.post_id
        'community-id .post.community_id
        'creator-id .creator.id
        'type (caar comment))))))

(defun lem-ui-render-comments (comments &optional buffer type sort)
  "Render COMMENTS, a list of comment objects.
TYPE
SORT.
For viewing a plain list of comments, not a hierarchy."
  (cl-loop for x in comments
           do (lem-ui-render-comment x sort)))

;; (setq lem-post-comments (lem-get-post-comments "1235982" "651145" "New"))
;; (setq lem-post-comments (lem-get-post-comments "1235982" nil "New"))

;;; THREADED COMMENTS:
;; Path: "The path / tree location of a comment, separated by dots, ending with the comment's id. Ex: 0.24.27"
;; https://github.com/LemmyNet/lemmy/blob/63d3759c481ff2d7594d391ae86e881e2aeca56d/crates/db_schema/src/source/comment.rs#L39

(defvar-local lem-comments-raw nil)

(defun lem-ui--build-and-render-comments-hierarchy (comments)
  "Build `lem-comments-hierarchy', a hierarchy, from COMMENTS, and render."
  (setq lem-comments-raw comments)
  (let ((list (alist-get 'comments comments)))
    (lem-ui--build-hierarchy list)) ; sets `lem-comments-hierarchy'
  (with-current-buffer (get-buffer-create "*lem-post*")
    (hierarchy-print
     lem-comments-hierarchy
     (lambda (item indent)
       (lem-ui-format-comment item indent))
     (lem-ui-symbol 'reply-bar))))

(defun lem-ui--parent-id (comment)
  "Return the parent id of COMMENT as a number.
Return nil if comment is only a child of the root post."
  (let* ((path (lem-ui-get-comment-path comment))
         (split (lem-ui-split-path path))
         (id (string-to-number
              (car (last split 2)))))
    (if (eq id 0)
        nil
      id)))

(defun lem-ui--parentfun (child)
  "Return the parent of CHILD in `lemmy-comments-hierarchy', recursively.
Parent-fun for `hierarchy-add-tree'."
  (let* ((parent-id (lem-ui--parent-id child))
         (list (alist-get 'comments lem-comments-raw)))
    (cl-find-if
     (lambda (comment)
       (let ((com (alist-get 'comment comment)))
         (equal parent-id
                (alist-get 'id com))))
     list)))

(defun lem-ui-get-comment-path (comment)
  "Get path value from COMMENT."
  (alist-get 'path
             (alist-get 'comment comment)))

(defun lem-ui-split-path (path)
  "Call split string on PATH with \".\" separator."
  (split-string path "\\."))

(defvar-local lem-comments-hierarchy nil)

(defun lem-ui--build-hierarchy (comments)
  "Build a hierarchy of COMMENTS using `hierarchy.el'."
  ;; (hierarchy-add-trees lem-comments-hierarchy
  ;; list
  ;; #'lem-ui--parentfun)))
  (setq lem-comments-hierarchy (hierarchy-new))
  (cl-loop for comment in comments
           do (hierarchy-add-tree lem-comments-hierarchy
                                  comment
                                  #'lem-ui--parentfun)))

(defun lem-ui-format-comment (comment &optional indent)
  "Format COMMENT, optionally with INDENT amount of indent bars."
  (let-alist comment
    (let ((content (when .comment.content
                     (lem-ui-render-body .comment.content)))
          (indent-str (make-string indent (string-to-char
                                           (lem-ui-symbol 'reply-bar)))))
      (propertize
       (concat
        (lem-ui-top-byline .creator.name
                           .counts.score
                           .comment.published)
        "\n"
        (or content "")
        "\n")
       'json comment
       'id .comment.id
       'post-id .comment.post_id
       'community-id .post.community_id
       'creator-id .creator.id
       'type 'comment
       'line-prefix indent-str))))

(defun lem-ui-render-post-comments (post-id &optional sort)
  "Render a hierarchy of post's comments.
POST-ID is the post's id."
  (let* ( ;(post-id (number-to-string .post.id))
         (comments (lem-api-get-post-comments post-id "All" sort "160"))
         (unique-comments (cl-remove-duplicates comments)))
    (lem-ui--build-and-render-comments-hierarchy unique-comments)))

(defun lem-ui-view-comment-post ()
  "View post of comment at point."
  (interactive)
  (if (not (eq (lem-ui--item-type) 'comment))
      (message "Not at a comment?")
    (let* ((post (get-text-property (point) 'post-id))
           (str (number-to-string post)))
      (lem-ui-view-post str))))

;;; LIKES / VOTES

(defun lem-ui-like-item (&optional dislike)
  "Like (upvote) item at point.
TYPE is either post or comment
If DISLIKE, dislike (downvote) it."
  (interactive)
  (lem-ui-with-id
      (let* ((type (get-text-property (point) 'type))
             (fun (if (eq type 'post)
                      #'lem-like-post
                    #'lem-like-comment))
             (id (string-to-number id))
             (score (if dislike -1 1)))
        (if (or (eq type 'post)
                (eq type 'comment))
            (progn (funcall fun id score)
                   (message "%s %s %sliked!" type id (if dislike "dis" "")))
          (message "No post or comment at point?")))))

(defun lem-ui-dislike-item ()
  "Dislike (downvote) item at point."
  (interactive)
  (lem-ui-like-item :dislike))

;;; USERS

(defun lem-ui-render-users (json)
  "JSON."
  (let ((users (alist-get 'users json)))
    (cl-loop for user in json
             do (lem-ui-render-user user))))

(defun lem-ui-render-user (json)
  "Render user with data JSON."
  (let-alist json
    (insert
     (propertize
      (concat
       (number-to-string .person.id) " "
       (propertize .person.name
                   'face '(:weight bold))
       " "
       .person.actor_id
       "\n"
       (lem-ui-symbol 'direct) " " ; FIXME: we need a post symbol
       (number-to-string .counts.post_count) " | "
       (lem-ui-symbol 'reply) " "
       (number-to-string .counts.comment_count)
       " | "
       "joined: " .person.published
       "\n"
       lem-ui-horiz-bar
       "\n")
      'json json
      'id .person.id
      'type (caar json)))))

(defun lem-ui-view-user (id &optional view-type sort limit)
  "View user with ID.
VIEW-TYPE must be a member of `lem-user-view-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is max items to show."
  (let ((json (lem-api-get-person-by-id id sort limit))
        (buf (get-buffer-create "*lem-user*")))
    (lem-ui-with-buffer buf 'lem-mode nil
      (let-alist json
        (lem-ui-render-user .person_view)
        (cond ((equal view-type "posts")
               (insert (lem-ui-format-heading "posts"))
               (lem-ui-render-posts .posts buf sort :community :trim))
              ((equal view-type "comments")
               (insert (lem-ui-format-heading "comments"))
               (lem-ui-render-comments .comments))
              (t ; no arg: overview
               (insert (lem-ui-format-heading "overview"))
               ;; TODO: insert mixed comments/posts
               (lem-ui-render-posts .posts buf sort :community :trim)
               (lem-ui-render-comments .comments)))
        (lem-ui-set-buffer-spec view-type sort #'lem-ui-view-user)
        (goto-char (point-min))))))

;; TODO: view own profile: full sort types
;; overview/comments/posts/saved listings
;; list of communities moderated
;; list subscribed

(defun lem-ui-view-item-user ()
  "View user of item at point."
  (interactive)
  (let ((user (get-text-property (point) 'creator-id)))
    (if user
        (let ((str (number-to-string user)))
          (lem-ui-view-user str 'overview))
      (message "No user item at point?"))))

(defun lem-ui-format-heading (name)
  "Format a heading for NAME."
  (propertize
   (concat " " lem-ui-horiz-bar "\n "
           (upcase name)
           "\n " lem-ui-horiz-bar "\n")
   'face 'success))

(defun lem-ui-view-user-at-point ()
  "View user at point."
  (interactive)
  (lem-ui-with-id
      (lem-ui-view-user id 'overview)))

(defun lem-ui-message-user-at-point ()
  "Send private message to user at point."
  (interactive)
  (lem-ui-with-id
      (let ((message (read-string "Private message: ")))
        (lem-send-private-message message id))))

(provide 'lem-ui)
;;; lem-ui.el ends here
