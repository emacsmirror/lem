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
    (person    . ("👤" . "[people]"))
    (replied   . ("⬇" . "↓"))
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

(defun lem-ui-font-lock-comment (str)
  "Font lock comment face STR."
  (propertize str
              'face font-lock-comment-face))

(defun lem-ui-thing-json ()
  "Get json of thing at point, comment, post, community or user."
  (get-text-property (point) 'json))

(defun lem-ui--get-id (&optional string type)
  "Return id as a string, from alist KEY in JSON.
SLOT is a symbol, either post, comment, user, or community.
STRING means return as string, else return number.
TYPE is the name of the ID property to get."
  (let ((id (get-text-property (point) (or type 'id))))
    (if string
        (number-to-string id)
      id)))

(defun lem-ui--item-type ()
  "Return the type property of item at point."
  (get-text-property (point) 'type))

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
       ,@body)))

(defmacro lem-ui-with-id (body)
  "Call BODY after fetching ID of THING (at point), a symbol.
Thing can be anything handled by `lem-ui-thing-json', currently:
comment, post, community or person.
Within this macro call, args JSON and ID are available."
  (declare (debug 'body)
           (indent 1))
  `(let* ((json (lem-ui-thing-json))
          ;; TODO: make generic by getting THING from JSON
          (id (lem-ui--get-id :string)))
     ,body))

;;; BUFFER DETAILS
(defvar-local lem-ui-buffer-spec nil
  "A plist containing details about the current lem buffer.")

(defun lem-ui-set-buffer-spec (listing-type sort view-fun) ; endpoint etc.
  "Set `lem-ui-buffer-spec' for the current buffer.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'."
  (setq lem-ui-buffer-spec
        `(:sort ,sort :listing-type ,listing-type :view-fun ,view-fun)))

(defun lem-ui-get-buffer-spec (key)
  "Return value of KEY in `lem-ui-buffer-spec'."
  (plist-get lem-ui-buffer-spec key))

;;; NAV

(defun lem--goto-pos (fun &optional refresh pos)
  "Search for toot with FUN.
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
(defun lem-ui-view-instance (&optional type sort limit)
  "View posts of current user's home instance.
SORT must be a member of `lem-sort-types'.
TYPE must be member of `lem-listing-types'.
LIMIT is the amount of results to return."
  (let ((posts (lem-get-posts type sort limit)) ; sort here too?
        (buf (get-buffer-create "*lem*")))
    (lem-ui-with-buffer buf 'lem-mode nil
      (lem-ui-render-posts posts buf nil sort :community :trim)
      (lem-ui-set-buffer-spec type sort #'lem-ui-view-instance) ; no children
      (goto-char (point-min)))))

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
(interactive)
(let* ((type (lem-ui-get-buffer-spec :listing-type))
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
         (if (eq type 'posts)
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

;; TODO: make for any current view:
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
  "Read a listing-type type and load it."
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

(defun lem-ui-search ()
  "Do a search for one of the types in `lem-search-types'."
  (interactive)
  (let* ((type (lem-ui-read-type "Search type: " lem-search-types))
         ;; LISTING/SORT doesn't make sense for all search types, eg users!:
         (listing-type (lem-ui-read-type "Listing type: " lem-listing-types))
         (sort (lem-ui-read-type "Sort by: " lem-sort-types))
         (query (read-string "Query: "))
         ;; TODO: handle all search args: community, page, limit
         (response (lem-search query type listing-type sort)))
    ;; TODO: render other responses:
    (cond ((equal type "Users")
           (let ((users (alist-get 'users response))
                 ;; TODO: refactor as lem-ui-render-users
                 (buf (get-buffer-create "*lem-users*")))
             (lem-ui-with-buffer buf 'lem-mode nil
               (lem-ui-render-users users)))))))

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
      (lem-ui-render-post post :comments sort :community)
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
                               &optional community community-url)
  "Format a top byline for post with NAME, SCORE and TIMESTAMP.
COMMUNITY and COMMUNITY-URL are those of the community the item belongs to."
  ;; TODO: name link to user page, etc.
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
      (lem-ui-symbol 'favourite) " "
      (number-to-string score) " | "
      timestamp)
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
        ;; (goto-char (point-min))
        (re-search-forward "\n\n" nil :no-error)
        (setq str (buffer-substring (point) (point-max)))
        (kill-buffer-and-window) ; shr's *html*
        (kill-buffer buf))) ; our md
    str))

(defun lem-ui-render-post (post &optional comments sort community trim)
  ;; NB trim both in instance and community views
  ;; NB show community info in instance and in post views
  "Render single POST.
Optionally render its COMMENTS. Optionally render post's COMMUNITY.
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
                            (when community .community.actor_id))
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
         "\n")
        'json post
        'id .post.id
        'community-id .post.community_id
        'creator-id .creator.id
        'type (caar post)))
      (when (and comments
                 (< 0 .counts.comments))
        (let* ((post-id (number-to-string .post.id))
               (comments (lem-api-get-post-comments post-id "All" sort)))
          ;; (list (alist-get 'comments comments)))
          (lem-ui-render-comments comments "All" sort)))))) ; NB: type All, make arg?

(defun lem-ui-render-posts (posts &optional buffer comments sort community trim)
  "Render a list of posts POSTS in BUFFER.
Used for instance, communities, posts, and users.
COMMENTS means also show post comments.
SORT is the kind of sorting to use.
COMMUNITY means display what community it was posted to.
TRIM means trim each post for length."
  (let ((list (alist-get 'posts posts))
        (buf (or buffer (get-buffer-create "*lem*"))))
    (with-current-buffer buf
      (cl-loop for x in list
               do (lem-ui-render-post x comments sort community trim)))))

;;; COMMUNITIES

(defun lem-ui-view-communities (&optional type sort)
  "View communities, subscribed to by the logged in user.
TYPE must be one of `lem-listing-types'.
SORT must be one of `lem-sort-types'."
  (interactive)
  (let* ((type (or type (completing-read "View communities: "
                                         lem-listing-types)))
         (sort (or sort (completing-read "Sorted by: "
                                         lem-sort-types)))
         (json (lem-list-communities type sort))
         (list (alist-get 'communities json))
         (buffer (format "*lem-" (downcase type) "-communities*")))
    (lem-ui-with-buffer (get-buffer-create buffer) 'lem-mode t
      (cl-loop for c in list
               for id = (alist-get 'id (alist-get 'community c))
               for view = (lem-get-community (number-to-string id) nil)
               for community = (alist-get 'community_view view)
               do (lem-ui-render-community-header community buffer :stats))
      (goto-char (point-min)))))

(defun lem-ui-subscribe-to-community-at-point ()
  "."
  (interactive)
  (lem-ui-with-id
      ;; TODO: needs feedback!
      (lem-follow-community id)))

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

(defun lem-ui-view-community (id &optional item sort limit)
  "View community with ID.
ITEM is a symbol, either posts or comments.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return."
  (let* ((community (lem-get-community id))
         (view (alist-get 'community_view community))
         (buf (get-buffer-create "*lem-community*"))
         ;; in case we set community posts, then switch to comments:
         (sort (if (eq item 'comments)
                   (unless (lem-comment-sort-type-p sort)
                     (car lem-comment-sort-types))
                 sort))
         (items (if (eq item 'posts)
                    (lem-get-posts nil sort limit id)
                  (lem-get-comments nil nil nil sort limit id)))) ; no sorting
    (lem-ui-with-buffer buf 'lem-mode nil
      (lem-ui-render-community-header view nil :stats)
      (if (eq item 'posts)
          (progn
            (insert (lem-ui-format-heading "posts"))
            (lem-ui-render-posts items buf nil sort)) ; no children
        (insert (lem-ui-format-heading "comments"))
        (lem-ui-render-comments items nil sort))
      (lem-ui-set-buffer-spec item sort #'lem-ui-view-community)
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

(defun lem-ui-render-community-header (community &optional buffer stats)
  "Render header details for COMMUNITY.
BUFFER is the one to render in, a string.
STATS are the community's stats to print."
  (with-current-buffer (get-buffer-create (or buffer "*lem-community*"))
    (let-alist community
      (insert
       (propertize
        (concat
         (propertize .community.title
                     'face '(:weight bold))
         " | "
         (lem-ui-font-lock-comment .community.name)
         "\n"
         .community.description
         "\n"
         (lem-ui-font-lock-comment .community.actor_id)
         "\n"
         .subscribed
         "\n"
         lem-ui-horiz-bar
         "\n")
        'json community
        'id .community.id
        'type (caar community)))
      ;; stats:
      (when stats
        (lem-ui-render-community-stats .counts.subscribers
                                       .counts.posts
                                       .counts.comments)))
    ;; mods:
    (let* ((mods-list (alist-get 'moderators community))
           (mods (mapcar (lambda (x)
                           (let-alist (alist-get 'moderator x)
                             (list (number-to-string .id)
                                   (or .display_name .name) .actor_id)))
                         mods-list)))
      (insert "mods: "
              (mapconcat (lambda (x)
                           (mapconcat #'identity x " "))
                         mods " | ")
              "\n"
              lem-ui-horiz-bar
              "\n"))))

(defun lem-ui-render-community-stats (subscribers posts comments)
  "Render stats for SUBSCRIBERS, POSTS and COMMENTS."
  ;; TODO: get symbols for these
  (let ((s (number-to-string subscribers))
        (s-sym (lem-ui-symbol 'person))
        (p (number-to-string posts))
        (p-sym (lem-ui-symbol 'direct))
        (c (number-to-string comments))
        (c-sym (lem-ui-symbol 'reply)))
    (insert
     (format "%s %s | %s %s | %s %s\n" s-sym s p-sym p c-sym c))))

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
         (post-id (lem-ui--get-id))
         (parent-id (when-let ((comment (alist-get 'comment json)))
                      (alist-get 'id comment)))
         (content (read-string "Reply: "))
         (response (lem-create-comment post-id content parent-id)))
    (when response
      (let-alist response
        (message "Comment created: %s" .comment_view.comment.content)))))

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

(defun lem-ui-get-comment-path (comment)
  "Get path value from COMMENT."
  (alist-get 'path
             (alist-get 'comment comment)))

(defun lem-ui-split-path (path)
  "Call split string on PATH with \".\" separator."
  (split-string path "\\."))

;; Path: "The path / tree location of a comment, separated by dots, ending with the comment's id. Ex: 0.24.27"
;; https://github.com/LemmyNet/lemmy/blob/63d3759c481ff2d7594d391ae86e881e2aeca56d/crates/db_schema/src/source/comment.rs#L39

(defun lem-ui-sort-comments (list)
  "LIST."
  (cl-loop for c in list
           for path = (lem-ui-get-comment-path c)
           for path-split = (lem-ui-split-path path)
           ;; collect c))
           collect path-split))

(defun lem-ui-render-comments (comments &optional type sort)
  "Render COMMENTS.
TYPE
SORT."
  (let ((list (alist-get 'comments comments)))
    ;; TODO: build comment tree
    (cl-loop for x in list ;comments ; sorted
             do (lem-ui-render-comment x sort))))

;; (setq lem-post-comments (lem-get-post-comments "1235982" "651145" "New"))
;; (setq lem-post-comments (lem-get-post-comments "1235982" nil "New"))

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
               (lem-ui-render-posts json buf nil sort :community :trim))
              ((equal view-type "comments")
               (insert (lem-ui-format-heading "comments"))
               (lem-ui-render-comments json))
              (t ; no arg: overview
               (insert (lem-ui-format-heading "overview"))
               ;; TODO: insert mixed comments/posts
               (lem-ui-render-posts json buf nil sort :community :trim)
               (lem-ui-render-comments json)))
        (lem-ui-set-buffer-spec view-type sort #'lem-ui-view-user)
        (goto-char (point-min))))))

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
   (concat "\n " lem-ui-horiz-bar "\n "
           (upcase name)
           "\n " lem-ui-horiz-bar "\n\n")
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
