;;; lem-ui.el --- Basics for a lemmy interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; URL: https://codeberg.org/martianh/lem.el
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

;; Some simple, unadorned, primitive, humble, basic, dashed-off functions for
;; an interface to Lemmy, the federated link-aggregator and forum software.
;; See <https://joinlemmy.org>.

;;; Code:

;; (require 'hierarchy)
(require 'cl-lib)
(require 'shr)
(require 'markdown-mode)
(require 'hierarchy)
(require 'vtable)

(require 'lem-api)

(defvar lem-listing-types)
(defvar lem-comment-sort-types)
(defvar lem-default-comment-sort-type)
(defvar lem-sort-types)
(defvar lem-default-sort-type)
(defvar lem-user-view-types)
(defvar lem-search-types)
(defvar lem-user-id)

(defvar-local lem-ui-post-community-mods-ids nil
  "A list of ids of the moderators of the community of the current post.")

(autoload 'lem-mode "lem.el")
(autoload 'lem-comment-sort-type-p "lem.el")

;;; HIERARCHY PATCHES

(defun lem--hierarchy-print (hierarchy &optional to-string)
  "Insert HIERARCHY in current buffer as plain text.

Use TO-STRING to convert each element to a string.  TO-STRING is
a function taking an item of HIERARCHY as input and returning a
string.

Calls `lem--hierarchy-print-line' with `hierarchy-labelfn-indent' as
second argument."
  (let ((to-string (or to-string (lambda (item) (format "%s" item)))))
    (lem--hierarchy-print-line
     hierarchy
     (hierarchy-labelfn-indent
      (lambda (item _)
        (funcall to-string item))))))

(defun lem--hierarchy-print-line (hierarchy &optional labelfn)
  "Insert HIERARCHY in current buffer as plain text.

Use LABELFN to convert each element to a string.  LABELFN is
a function taking an item of HIERARCHY as input and returning a
string.  If nil, LABELFN defaults to a call to `format' with \"%s\".

This function is not responsible for indentation, but it can be
achieved by providing a function such as
`hierarchy-labelfun-indent' for LABELFN."
  (let ((labelfn (or labelfn (lambda (item) (format "%s" item)))))
    (hierarchy-map
     (lambda (item indent)
       (insert (funcall labelfn item indent) "\n"))
     hierarchy)))

;;; VARS

(defvar lem-ui-comments-limit "50"
  "The number of comments to request for a post.
Server maximum appears to be 50.")

(defvar-local lem-ui-current-items nil
  "A list holding the ids of all items in the current view.
Used for pagination.")

(defvar lem-ui-url-regex
  ;; adapted from ffap-url-regexp
  (concat
   "\\(?2:\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)" ; uri prefix
   "[^ )\n\t]*\\)" ; any old thing, i.e. we allow invalid/unwise chars. but no )
   "\\(/\\)?" ; optional ending slash? ; TODO: some are caught, some are not
   "\\b"))

(defvar lem-ui-image-formats
  '("png" "jpg" "jpeg" "webp")
  "Image formats that we may want to render for post URLs.")

;;; UTILITIES

(defvar lem-ui-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

(defun lem-ui-format-heading (name)
  "Format a heading for NAME, a string."
  (propertize
   (concat " " lem-ui-horiz-bar "\n "
           (upcase name)
           "\n " lem-ui-horiz-bar "\n")
   'face 'success))

(defun lem-ui-insert-heading (name)
  "Insert heading for NAME, a string."
  (insert (lem-ui-format-heading name)))

(defgroup lem nil
  "Lemmy client."
  :prefix "lem-ui-"
  :group 'lem)

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
    (reply-bar . ("┃" . "|"))
    (deleted   . ("🗑" . "[deleted]")))
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
  "Font lock comment face STRS."
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
  (lem-ui--property 'lem-type))

(defun lem-ui--id-from-prop (&optional string type)
  "Return id as a string, from alist KEY in JSON.
SLOT is a symbol, either post, comment, user, or community.
STRING means return as string, else return number.
TYPE is the name of the ID property to get."
  (let ((id (lem-ui--property (or type 'id))))
    (if (and string id)
        (number-to-string id)
      id)))

(defun lem-ui--id-from-json (json type &optional string)
  "Return the ID of json object JSON, of TYPE.
If STRING, return the id as a string."
  (let ((id
         (alist-get 'id
                    (alist-get type json))))
    (if (and string id)
        (number-to-string id)
      id)))

;; TODO: add to `lem-ui-with-buffer'? we almost always call it
(defun lem-ui--init-view ()
  "Initialize a lemmy view.
Inserts images and sets relative timestamp timers."
  (let ((inhibit-read-only t))
    ;; load images
    (lem-ui-insert-images)
    ;; relative timestamps:
    (setq
     ;; Initialize with a minimal interval; we re-scan at least once
     ;; every 5 minutes to catch any timestamps we may have missed
     fedi-timestamp-next-update (time-add (current-time)
                                          (seconds-to-time 300)))
    (setq fedi-timestamp-update-timer
          (when fedi-enable-relative-timestamps
            (run-at-time (time-to-seconds
                          (time-subtract fedi-timestamp-next-update
                                         (current-time)))
                         nil ;; don't repeat
                         #'fedi--update-timestamps-callback
                         (current-buffer)
                         nil)))))

;;; MACROS

(defmacro lem-ui-with-buffer (buffer mode-fun other-window bindings &rest body)
  "Evaluate BODY in a new or existing buffer called BUFFER.
MODE-FUN is called to set the major mode.
OTHER-WINDOW means call `switch-to-buffer-other-window' rather
than `switch-to-buffer'.
BINDINGS is a list of variables for which to display bidings."
  (declare (debug t)
           (indent 4))
  `(with-current-buffer (get-buffer-create ,buffer)
     (let* ((inhibit-read-only t)
            (sort-str (when (member 'lem-sort-types ,bindings)
                        "\\[lem-ui-cycle-sort]: cycle sort"))
            (listing-str (when (member 'lem-listing-types ,bindings)
                           "\\[lem-ui-cycle-listing-type]: cycle listing\n"))
            (view-str (when (or (member 'lem-view-types ,bindings)
                                (member 'lem-user-view-types ,bindings))
                        "\\[lem-ui-toggle-posts-comments]: toggle posts/comments"))
            (msg-str (concat listing-str "\n" sort-str "\n" view-str)))
       (erase-buffer)
       (funcall ,mode-fun)
       (if ,other-window
           (switch-to-buffer-other-window ,buffer)
         (switch-to-buffer ,buffer))
       ,@body
       (goto-char (point-min))
       ;; FIXME: this needs to come after media messages:
       (when ,bindings
         (sleep-for 1)
         (message
          (substitute-command-keys msg-str))))))

(defmacro lem-ui-with-item (body &optional number)
  "Call BODY after fetching ID of thing (at point).
Thing can be anything handled by `lem-ui-thing-json', currently:
comment, post, community or person.
Within this macro call, arg ID is available.
NUMBER means return ID as a number."
  (declare (debug t)
           (indent 1))
  `(let* ((id (lem-ui--id-from-prop (if ,number nil :string))))
     (if (not id)
         (message "Looks like there's no item at point?")
       ,body)))

;;; BUFFER DETAILS

(defvar-local lem-ui-buffer-spec nil
  "A plist containing details about the current lem buffer.")

(defun lem-ui-set-buffer-spec (&optional listing-type sort
                                         view-fun item page unread)
  "Set `lem-ui-buffer-spec' for the current buffer.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
ITEM is a symbol, either posts or comments."
  ;; TODO: allow us to set a single element:
  (setq lem-ui-buffer-spec
        `(:listing-type ,listing-type :sort ,sort :view-fun ,view-fun
                        :item ,item :page ,(or page 1) :unread ,unread)))

(defun lem-ui-get-buffer-spec (key)
  "Return value of KEY in `lem-ui-buffer-spec'."
  (plist-get lem-ui-buffer-spec key))

;;; NAV

(defun lem--goto-pos (fun &optional refresh pos)
  "Search for item with FUN.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (fedi--goto-pos fun 'byline-top refresh pos))

(defun lem-next-item ()
  "Move to next item."
  (interactive)
  (lem--goto-pos #'next-single-property-change #'lem-ui-more))

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
          ((or (eq type 'comment)
               (eq type 'comment-reply))
           (lem-ui-view-comment-post))
          ((eq type 'person)
           (lem-ui-view-user-at-point)))))

(defun lem-ui-scroll-up-command ()
  "Call `scroll-up-command', loading more toots if necessary.
If we hit `point-max', call `lem-ui-more' then `scroll-up-command'."
  (interactive)
  (if (not (equal (point) (point-max)))
      (scroll-up-command)
    (lem-ui-more)
    (scroll-up-command)))

(defun lem-ui-next-tab-item ()
  "Jump to next tab item."
  (interactive)
  (fedi-next-tab-item nil 'lem-tab-stop))

(defun lem-ui-prev-tab-item ()
  "Jump to prev tab item."
  (interactive)
  (fedi-next-tab-item :prev 'lem-tab-stop))

;;; INSTANCE

;; TODO: toggle posts or comments (ITEM arg)
(defun lem-ui-view-instance (&optional type sort limit page sidebar item)
  "View posts of current user's home instance.
SORT must be a member of `lem-sort-types'.
TYPE must be member of `lem-listing-types'.
ITEM is a symbol, either posts or comments."
  (interactive)
  (let* ((instance (lem-get-instance))
         (items (if (eq item 'comments)
                    ;;FIXME: sort arg breaks the request:
                    (lem-get-comments nil nil type nil limit page) ;sort limit page)
                  (lem-get-posts type sort limit page)))
         (items (if (eq item 'comments)
                    (alist-get 'comments items)
                  (alist-get 'posts items)))
         (sort (or sort lem-default-sort-type))
         (buf "*lem-instance*")
         (bindings (lem-ui-view-options 'instance)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (lem-ui-render-instance instance :stats sidebar)
      (lem-ui-insert-heading (if (eq nil item) "posts" (symbol-name item)))
      (if (eq item 'comments)
          (lem-ui-render-comments items)
        (lem-ui-render-posts-instance items))
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec
       type sort #'lem-ui-view-instance item page))))

(defun lem-ui-view-instance-full ()
  "View full instance details."
  ;; TODO: full instance info: sidebar, full desc,
  ;; trending communities, stats, admins
  (interactive)
  (lem-ui-view-instance nil nil nil nil :sidebar))

(defun lem-ui-view-modlog (_args)
  "Docstring."
  ;; TODO
  )

(defun lem-ui-insert-people (list str)
  "Insert propertized link for each person in LIST.
Each person is a three item list of username, id, and URL, the
value returned by `lem-ui--names-list'.
STR is the preceding string to insert."
  (insert
   str
   (mapconcat
    (lambda (x)
      (lem-ui--propertize-link (cl-first x)
                               (cl-second x)
                               'user
                               (cl-third x)
                               'warning))
    list " | ")))

(defun lem-ui-render-instance (instance &optional stats sidebar)
  "INSTANCE.
STATS.
SIDEBAR."
  (let* ((admins-list (alist-get 'admins instance))
         (admins (lem-ui--names-list admins-list 'person))
         (inst (alist-get 'site_view instance)))
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
         .site.description "\n"
         (if sidebar
             (concat (lem-ui-render-body .site.sidebar)
                     "\n")
           "")
         lem-ui-horiz-bar "\n")
        'json instance
        'byline-top t ; next/prev hack
        'id .site.id
        'lem-type 'instance)))
    ;; stats:
    (when stats
      (let-alist (alist-get 'counts inst)
        (lem-ui-render-stats .users
                             .posts
                             .comments
                             .communities)))
    ;; admins:
    (when admins
      (lem-ui-insert-people admins "admins: ")
      (insert "\n" lem-ui-horiz-bar "\n"))
    (insert "\n")))

;;; VIEWS SORTING AND TYPES

(defun lem-ui-view-options (view)
  "Return the various sorting and other options for VIEW.
Returns a list of the variables containing the specific options."
  (cond ((eq view 'post)
         '(lem-comment-sort-types))
        ((eq view 'instance)
         '(lem-view-types lem-sort-types lem-listing-types))
        ((eq view 'search)
         '(lem-listing-types lem-sort-types lem-search-types))
        ((eq view 'user)
         '(lem-user-view-types lem-sort-types))
        ((eq view 'community)
         '(lem-view-types lem-sort-types))))

(defun lem-ui-toggle-posts-comments ()
  "Switch between displaying posts or comments.
Works on instance, community, and user views."
  (interactive)
  (let* ((item-type (lem-ui-get-buffer-spec :item))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         (sort (lem-ui-get-buffer-spec :sort))
         (type (lem-ui-get-buffer-spec :listing-type))
         (id (lem-ui-get-view-id))
         (user-p (eq view-fun #'lem-ui-view-user))
         (community-p (eq view-fun #'lem-ui-view-community))
         (instance-p (or (eq view-fun #'lem-ui-view-instance)
                         (eq view-fun #'lem-ui-view-instance-full))))
    (cond (community-p
           (if (not (eq item-type 'comments))
               (funcall view-fun id 'comments sort)
             (funcall view-fun id 'posts sort)))
          (user-p
           (cond ((equal type "overview")
                  (lem-ui-toggle-funcall view-fun id "posts" sort))
                 ((equal type "posts")
                  (lem-ui-toggle-funcall view-fun id "comments" sort))
                 (t ; comments or nil
                  (lem-ui-toggle-funcall view-fun id "overview" sort))))
          (instance-p
           (if (not (eq item-type 'comments))
               (funcall view-fun nil sort nil nil nil 'comments)
             (funcall view-fun nil sort nil nil nil 'posts)))
          (t
           (user-error "Posts/Comments toggle not available in this view")))))

(defun lem-ui-toggle-funcall (fun id view-type sort)
  "Call FUN with ID VIEW-TYPE and SORT as args.
Then message VIEW-TYPE."
  ;; users have `lem-user-view-types', community/instance `lem-view-types'
  (funcall fun id view-type sort)
  (message "Viewing: %s" view-type))

(defun lem-ui-get-view-id ()
  "Get id of the view item, a post or user."
  (save-excursion
    (goto-char (point-min))
    (lem-ui--id-from-prop :string)))

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
  "Cycle view between `lem-listing-types'."
  ;; For a user view, cycle between overview, posts and comments."
  ;; For a community view, cycle between posts and comments."
  ;; FIXME: remove posts/comments from this logic/binding
  ;; but if user/community/post don't cycle, what does cycle? instance + ?
  ;; in which case, lets pull it out and leave the others!
  (interactive)
  (let* ((type (lem-ui-get-buffer-spec :listing-type))
         (sort (lem-ui-get-buffer-spec :sort))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         ;; (id (lem-ui-get-view-id))
         (user-p (eq view-fun #'lem-ui-view-user))
         (post-p (eq view-fun #'lem-ui-view-post))
         (community-p (eq view-fun #'lem-ui-view-community)))
    ;; TODO: refactor
    (cond (user-p
           (message "User views don't have listing type."))
          ;; (cond ((equal type "overview")
          ;;        (lem-ui-cycle-funcall view-fun
          ;;                              "posts" sort 'listing id))
          ;;       ((equal type "posts")
          ;;        (lem-ui-cycle-funcall view-fun
          ;;                              "comments" sort 'listing id))
          ;;       (t ; comments or nil
          ;;        (lem-ui-cycle-funcall view-fun
          ;;                              "overview" sort 'listing id))))
          (community-p
           (message "Community views don't have listing type."))
          ;;  (if (not (eq item-type 'comments))
          ;;      (lem-ui-cycle-funcall view-fun 'comments sort 'listing id)
          ;;    (lem-ui-cycle-funcall view-fun 'posts sort 'listing id)))
          (post-p
           (message "Post views don't have listing type."))
          (t
           (if (or (equal type (car (last lem-listing-types)))
                   (null type))
               (lem-ui-cycle-funcall
                view-fun (car lem-listing-types) sort 'listing)
             (lem-ui-cycle-funcall
              view-fun (cadr (member type lem-listing-types))
              sort 'listing))))))

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
         (list (if (eq sort-or-type 'lem-type)
                   type-list
                 sort-list))
         (choice (completing-read (format "View by %s" sort-or-type)
                                  list nil :match)))
    (if id
        (cond ((eq sort-or-type 'lem-type)
               (funcall view-fun id choice sort))
              (post-p
               (funcall view-fun id choice))
              (t
               (funcall view-fun id type choice)))
      (if (eq sort-or-type 'lem-type)
          (funcall view-fun choice sort)
        (funcall view-fun type choice)))))

(defun lem-ui-call-sort-or-type (sort-or-type)
  "Call `lem-ui-call-or-type', with id arg if needed.
SORT-OR-TYPE is either sort or type."
  (let ((view-fun (lem-ui-get-buffer-spec :view-fun))
        (id (lem-ui-get-view-id)))
    (if (and (eq view-fun #'lem-ui-view-post)
             (eq sort-or-type 'lem-type))
        (message "Post views don't have listing type.")
      (if (or (eq view-fun #'lem-ui-view-post)
              (eq view-fun #'lem-ui-view-user)
              (eq view-fun #'lem-ui-view-community))
          (lem-ui-sort-or-type sort-or-type view-fun id)
        (lem-ui-sort-or-type sort-or-type view-fun)))))

(defun lem-ui-choose-type ()
  "Read a listing type and load it."
  (interactive)
  (lem-ui-call-sort-or-type 'lem-type))

(defun lem-ui-choose-sort ()
  "Read a sort type and load it."
  (interactive)
  (lem-ui-call-sort-or-type 'sort))

(defun lem-ui-read-type (prompt types-list)
  "Read a choice from TYPES-LIST using PROMPT."
  (completing-read prompt
                   types-list nil :match))

(defun lem-ui-search (&optional limit)
  "Do a search for objects of one of the types in `lem-search-types'.
LIMIT is the max results to return."
  (interactive)
  (let* ((types ; remove not-yet-implemented search types:
          (remove "Url"
                  (remove "All" lem-search-types)))
         (type (downcase (lem-ui-read-type "Search type: " types)))
         ;; FIXME: LISTING/SORT doesn't make sense for all search types, eg users!:
         ;; (listing-type (lem-ui-read-type "Listing type: " lem-listing-types))
         ;; (sort (lem-ui-read-type "Sort by: " lem-sort-types))
         (query (read-string "Query: "))
         (type-fun (intern (concat "lem-ui-render-" type)))
         (buf (format "*lem-search-%s*" type))
         ;; TODO: handle all search args: community, page, limit
         (response (lem-search query (capitalize type) nil nil ; listing-type sort
                               (or limit lem-ui-comments-limit)))
         (data (alist-get (intern type) response)))
    ;; TODO: render other responses:
    ;; ("All" TODO
    ;; "Comments" DONE
    ;; "Posts" DONE
    ;; "Communities" DONE
    ;; "Users" DONE
    ;; "Url") TODO
    (lem-ui-with-buffer buf 'lem-mode nil nil
      ;; and say a prayer to the function signature gods:
      (funcall type-fun data))))

(defun lem-ui-lookup-call (type data fun &optional string)
  "Call FUN on ID of item of TYPE, from DATA.
STRING means ID should be a string."
  (let* ((thing (alist-get type data))
         (id (lem-ui--id-from-json thing type string))
         (post-id (when (eq type 'comment)
                    (number-to-string
                     (alist-get 'post_id
                                (alist-get 'comment thing))))))
    (if (eq type 'comment)
        (funcall fun post-id id)
      (funcall fun id))))

(defun lem-fedilike-url-p (query)
  "Check if QUERY resembles a fediverse URL."
  ;; calqued off https://github.com/tuskyapp/Tusky/blob/c8fc2418b8f5458a817bba221d025b822225e130/app/src/main/java/com/keylesspalace/tusky/BottomSheetActivity.kt
  ;; thx to Conny Duck!
  (let* ((uri-parsed (url-generic-parse-url query))
         (query (url-filename uri-parsed)))
    (save-match-data
      (or (string-match "^/@[^/]+$" query)
          (string-match "^/@[^/]+/[[:digit:]]+$" query)
          (string-match "^/user[s]?/[[:alnum:]]+$" query)
          (string-match "^/notice/[[:alnum:]]+$" query)
          (string-match "^/objects/[-a-f0-9]+$" query)
          (string-match "^/notes/[a-z0-9]+$" query)
          (string-match "^/display/[-a-f0-9]+$" query)
          (string-match "^/profile/[[:alpha:]]+$" query)
          (string-match "^/p/[[:alpha:]]+/[[:digit:]]+$" query)
          (string-match "^/[[:alpha:]]+$" query)
          (string-match "^/u/[_[:alpha:]]+$" query)
          (string-match "^/c/[@._[:alnum:]]+$" query)
          (string-match "^/post/[[:digit:]]+$" query)
          (string-match "^/comment/[[:digit:]]+$" query)))))

(defun lem-ui-url-lookup (&optional url)
  "Perform a webfinger lookup on URL and load the result in `lem.el'.
Or url at point, or text prop shr-url, or read a URL in the minibuffer.
Lemmy supports lookups for users, posts, comments and communities."
  (interactive)
  (let ((query (or url
                   (thing-at-point-url-at-point)
                   (lem-ui--property 'shr-url)
                   (read-string "Lookup URL: "))))
    (if (not (lem-fedilike-url-p query))
        (browse-url query)
      (message "Performing lookup...")
      (let ((response (lem-resolve-object query)))
        (cond ((stringp response)
               (progn
                 (message "%s" response)
                 (browse-url query)))
              ((equal 'person (caar response))
               (lem-ui-lookup-call 'person response 'lem-ui-view-user :str))
              ((equal 'comment (caar response))
               (lem-ui-lookup-call 'comment response 'lem-ui-view-comment-post))
              ((equal 'post (caar response))
               (lem-ui-lookup-call 'post response 'lem-ui-view-post :str))
              ((equal 'community (caar response))
               (lem-ui-lookup-call 'community response 'lem-ui-view-community :str))
              (t
               (message "unknown lookup response.")
               (browse-url query)))))))

;;; POSTS

(defun lem-ui-view-post-at-point ()
  "View post at point."
  (interactive)
  (lem-ui-with-item
      (lem-ui-view-post id)))

(defun lem-ui-view-post (id &optional sort limit)
  "View post with ID.
SORT must be a member of `lem-comment-sort-types.'
LIMIT."
  (let* ((post-view (lem-get-post id))
         (post (alist-get 'post_view post-view))
         (community-id (alist-get 'community_id
                                  (alist-get 'post post)))
         (sort (or sort lem-default-comment-sort-type))
         (bindings (lem-ui-view-options 'post)))
    (lem-ui-with-buffer "*lem-post*" 'lem-mode nil bindings
      (lem-ui--set-mods community-id)
      (lem-ui-render-post post :community)
      (lem-ui-render-post-comments id sort limit)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-post 'post)))) ; limit

;;; LINKS

(defvar lem-ui--link-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map shr-map)
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
  (let ((id (lem-ui--id-from-prop :string 'id))
        (creator-id (lem-ui--id-from-prop :string 'creator-id))
        (community-id (lem-ui--id-from-prop :string 'community-id))
        (item-type (lem-ui--property 'lem-tab-stop))
        url)
    (cond ((setq url (lem-ui--property 'shr-url))
           (if (string-prefix-p "/c/" url) ; community relative link
               (lem-get-community (substring-no-properties url 3))
             (lem-ui-url-lookup url)))
          ((eq item-type 'community)
           (lem-ui-view-community community-id))
          ((and (eq item-type 'user)
                creator-id)
           (lem-ui-view-user creator-id))
          ;; admin display in instance header:
          ;; (type user, but id not creator-id)
          ((eq item-type 'user)
           (lem-ui-view-user id))
          ((and (eq (lem-ui--property 'lem-type) 'post)
                (lem-ui--property 'title))
           (lem-ui-view-post-at-point)))))

(defun lem-ui--propertize-link (item id type &optional url face help-echo)
  "Propertize a link ITEM with ID and TYPE.
Optionally provide URL for shr-url.
FACE is a face to use.
HELP-ECHO is a help-echo string."
  (propertize item
              'shr-url url
              'keymap lem-ui--link-map
              'button t
              'category 'shr
              'follow-link t
              'mouse-face 'highlight
              'id id
              'lem-tab-stop type
              'face `(:inherit ,face :underline t)
              'help-echo help-echo))

(defun lem-ui--find-property-range (property start-point
                                             &optional search-backwards)
  "Return nil if no such range is found.
If PROPERTY is set at START-POINT returns a range around
START-POINT otherwise before/after START-POINT.
SEARCH-BACKWARDS determines whether we pick point
before (non-nil) or after (nil)"
  (if (get-text-property start-point property)
      ;; We are within a range, so look backwards for the start:
      (cons (previous-single-property-change
             (if (equal start-point (point-max)) start-point (1+ start-point))
             property nil (point-min))
            (next-single-property-change start-point property nil (point-max)))
    (if search-backwards
        (let* ((end (or (previous-single-property-change
                         (if (equal start-point (point-max))
                             start-point (1+ start-point))
                         property)
                        ;; we may either be just before the range or there
                        ;; is nothing at all
                        (and (not (equal start-point (point-min)))
                             (get-text-property (1- start-point) property)
                             start-point)))
               (start (and end (previous-single-property-change
                                end property nil (point-min)))))
          (when end
            (cons start end)))
      (let* ((start (next-single-property-change start-point property))
             (end (and start (next-single-property-change
                              start property nil (point-max)))))
        (when start
          (cons start end))))))

(defun lem-ui--process-link (start end)
  "Process link URL in JSON as userhandle, community, or normal link.
START and END are the boundaries of the link in the post body."
  (let* ((help-echo (get-text-property start 'help-echo))
         (keymap lem-ui--link-map)
         (lem-tab-stop-type 'shr-url))
    (add-text-properties start end
                         (append
                          (list 'lem-tab-stop lem-tab-stop-type
                                'keymap keymap
                                'help-echo help-echo)))))

;;; BYLINES

(defun lem-ui-propertize-box (str)
  "Propertize STR with box and `font-lock-keyword-face'."
  (propertize str
              'face '(:inherit font-lock-keyword-face :box t)))

(defun lem-ui-propertize-title (title-str)
  "Propertize TITLE-STR as a post title."
  (propertize title-str
              'mouse-face 'highlight
              'cursor-face '(:inherit highlight :extend t)
              'title t
              'keymap lem-ui--link-map
              'face '(:weight bold)))

(defun lem-ui-top-byline (title url username _score timestamp
                                &optional community _community-url
                                featured-p op-p admin-p mod-p del-p handle)
  "Format a top byline for post with TITLE, URL, USERNAME, SCORE and TIMESTAMP.
COMMUNITY and COMMUNITY-URL are those of the community the item belongs to.
FEATURED-P means the item is pinned.
OP-P is a flag, meaning we add a boxed OP string to the byline.
ADMIN-P means we add same for admins, MOD-P means add same for moderators.
DEL-P means add icon for deleted item.
HANDLE is a user handle as a string."
  (let ((url (lem-ui-render-url url))
        (parsed-time (date-to-time timestamp)))
    (propertize
     (concat
      (if title
          (concat (lem-ui-propertize-title title)
                  "\n")
        "")
      (if url
          (concat url "\n")
        "")
      (lem-ui--propertize-link username nil 'user nil 'warning handle)
      (when op-p
        (concat " "
                (lem-ui-propertize-box "OP")))
      (when admin-p
        (concat " "
                (lem-ui-propertize-box "ADM")))
      (when mod-p
        (concat " "
                (lem-ui-propertize-box "MOD")))
      (when del-p
        (concat " "
                (lem-ui-symbol 'deleted)))
      (when community
        (concat
         (propertize " to "
                     'face font-lock-comment-face)
         (lem-ui--propertize-link community nil 'community nil 'success)))
      (propertize
       (concat
        " | "
        (propertize timestamp
                    'timestamp parsed-time
                    'display (if fedi-enable-relative-timestamps
                                 (fedi--relative-time-description parsed-time)
                               parsed-time))
        (if (eq featured-p t)
            (concat " | "
                    (lem-ui-symbol 'pinned))
          ""))
       'face font-lock-comment-face))
     'byline-top t)))

(defun lem-ui-prop-score (my-vote score)
  "Propertize byline SCORE according to MY-VOTE, a number."
  (cond ((eq my-vote 1)
         (propertize (number-to-string score)
                     'face '(:inherit success :box t
                                      :weight bold)
                     'help-echo "you liked"))
        ((eq my-vote -1)
         (propertize (number-to-string score)
                     'face '(:inherit error :box t
                                      :weight bold)
                     'help-echo "you disliked"))
        (t
         (number-to-string score))))

(defun lem-ui-bt-byline (score comments &optional my-vote saved)
  "Format a bottom byline for an item.
SCORE is the item's score.
COMMENTS is the comments count to render.
MY-VOTE is a number, the current vote by the current user.
SAVED means to add saved icon."
  (let* ((my-score (lem-ui-prop-score my-vote score))
         (str (concat (lem-ui-symbol 'upvote) " "
                      my-score " | "
                      (lem-ui-symbol 'reply) " "
                      (number-to-string comments)
                      (when (eq saved t)
                        (concat " | "
                                (lem-ui-symbol 'bookmark))))))
    (propertize str
                'byline-bottom t)))

(defun lem-ui-update-bt-byline-from-json (&optional vote saved)
  "Update the text of the bottom byline based on item JSON.
Used to adjust counts after (un)liking.
VOTE is a number, handed to `lem-ui-bt-byline'.
SAVED means to add saved icon."
  (let-alist (lem-ui--property 'json)
    (let ((inhibit-read-only t)
          (byline
           (fedi--find-property-range 'byline-bottom (point)
                                      (when (lem-ui--property 'byline-bottom)
                                        :backwards))))
      ;; `emojify-mode' doesn't work with display prop, so we replace byline
      ;; string:
      (lem-ui--replace-region-contents
       (car byline) (cdr byline)
       (lambda ()
         (lem-ui-bt-byline .counts.score
                           (or .counts.child_count
                               .counts.comments)
                           vote
                           saved))))))

(defun lem-ui--replace-region-contents (beg end replace-fun)
  "Replace buffer contents from BEG to END with REPLACE-FUN.
We roll our own `replace-region-contents' because it is as
non-destructive as possible, whereas we need to always replace
the whole likes count in order to propertize it fully."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (delete-region (point-min) (point-max))
      (insert
       (funcall replace-fun)))))

(defun lem-ui-render-url (url &optional no-shorten)
  "Render URL, a plain non-html string.
NO-SHORTEN means display full URL, else only the domain is shown."
  (when url
    (let ((parsed (url-generic-parse-url url))
          rendered)
      (with-temp-buffer
        (insert "<a href=" url ">"
                (if no-shorten
                    url
                  (url-host parsed))
                "</a>")
        (shr-render-buffer (current-buffer))
        (setq rendered (buffer-string))
        (kill-buffer-and-window))
      rendered)))

(defun lem-ui-mdize-plain-urls ()
  "Markdown-ize any plain string URLs found in current buffer."
  ;; FIXME: this doesn't rly work with ```verbatim``` in md
  (while (re-search-forward lem-ui-url-regex nil :no-error)
    (unless (save-excursion
              (goto-char (1- (point)))
              (markdown-inside-link-p))
      (replace-match
       (concat "<" (match-string 0) ">")))))

(defun lem-ui-render-shr-url ()
  "Call `lem-ui--process-link' on any shr-url found in buffer."
  ;; JSON is the item's data to process the link with."
  (save-excursion
    (let (region)
      (while (setq region (lem-ui--find-property-range
                           'shr-url (or (cdr region) (point-min))))
        ;; TODO: handle "/c/group@instance.org" shr-urls
        (lem-ui--process-link (car region) (cdr region))))))
;; (get-text-property (car region) 'shr-url))))))

(defun lem-ui-render-body (body &optional json indent)
  "Render item BODY as markdowned html.
JSON is the item's data to process the link with.
INDENT is a number, the level of indent for the item."
  (let ((buf "*lem-md*")
        str)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (lem-ui-mdize-plain-urls)
      ;; FIXME: this breaks a normal URL containing a handle (e.g a link to a
      ;; mastodon user page):
      (let ((replaced (string-replace "@" "\\@" (buffer-string))))
        (erase-buffer)
        (insert replaced)
        (markdown-standalone buf))
      (with-current-buffer buf
        (let ((shr-width (when indent
                           (- (window-width) (+ 1 indent)))))
          ;; shr render:
          (shr-render-buffer (current-buffer))))
      (with-current-buffer "*html*" ; created by shr
        ;; our render:
        (when json
          (lem-ui-render-shr-url))
        (re-search-forward "\n\n" nil :no-error)
        (setq str (buffer-substring (point) (point-max)))
        (kill-buffer-and-window)        ; shr's *html*
        (kill-buffer buf)))             ; our md
    str))

(defun lem-ui--set-mods (community-id)
  "Set `lem-ui-post-community-mods-ids'.
The variable contains the list of community moderator ids for the
community of the current post, with COMMUNITY-ID."
  (let* ((community-json (lem-get-community community-id))
         (mods (alist-get 'moderators community-json))
         (mods-ids (cl-loop for mod in mods
                            collect (alist-get 'id
                                               (alist-get 'moderator mod)))))
    (setq lem-ui-post-community-mods-ids mods-ids)))

(defun lem-ui-render-post (post &optional community trim)
  ;; NB trim in instance, community, and user views
  ;; NB show community info in instance and in post views
  "Render single POST.
Optionally render post's COMMUNITY.
Optionally TRIM post length.
SORT must be a member of `lem-sort-types'."
  (let-alist post
    (let* (;(url (lem-ui-render-url .post.url))
           (body (when .post.body
                   (lem-ui-render-body .post.body (alist-get 'post post))))
           (handle (lem-ui--handle-from-user-url .creator.actor_id))
           (admin-p (eq t .creator.admin))
           (mod-p (cl-member .creator.id lem-ui-post-community-mods-ids))
           (del-p (eq t .post.deleted)))
      (insert
       (propertize
        (concat
         (lem-ui-top-byline .post.name
                            .post.url
                            (or .creator.display_name .creator.name)
                            .counts.score
                            .post.published
                            (when community .community.name)
                            (when community .community.actor_id)
                            .post.featured_community ; pinned for community
                            ;; the other option is .post.featured_local
                            nil admin-p mod-p del-p handle)
         "\n"
         (if .post.body
             (if trim
                 (string-limit body 400)
               body)
           "")
         (lem-ui-insert-post-image-maybe post)
         "\n"
         (lem-ui-bt-byline .counts.score .counts.comments .my_vote .saved)
         "\n"
         lem-ui-horiz-bar
         "\n\n")
        'json post
        'id .post.id
        'community-id .post.community_id
        'creator-id .creator.id
        'lem-type (caar post))))))

(defun lem-ui-insert-post-image-maybe (post) ; &optional alt)
  "Render URL of POST as an image if it resembles one."
  (let-alist post
    ;; (setq image-url .post.url)
    (when .post.url
      (let* ((parsed (url-generic-parse-url .post.url))
             (filename (url-filename parsed))
             (ext (car (last (split-string filename "\\.")))))
        (if (member ext lem-ui-image-formats)
            (let ((html (concat "<img src=\"" .post.url "\" alt=\"*\" />"))
                  rendered)
              (with-temp-buffer
                (insert html)
                (shr-render-buffer (current-buffer))
                (setq rendered (buffer-string))
                (kill-buffer-and-window))
              (concat "\n" rendered))
          "")))))

(defun lem-ui-render-posts-instance (posts)
  "Render a list of posts POSTS in BUFFER, trimmed and showing community."
  ;; SORT should be a member of `lem-sort-types'."
  (lem-ui-render-posts posts :community :trim))

(defun lem-ui-render-posts (posts &optional community trim)
  "Render a list of posts POSTS in BUFFER.
Used for instance, communities, posts, and users.
COMMUNITY means display what community it was posted to.
TRIM means trim each post for length."
  (cl-loop for x in posts
           do (lem-ui-render-post x community trim)))

(defun lem-ui-save-item (&optional unsave)
  "Save item at point.
Saved items can be viewed in your profile, like bookmarks.
If UNSAVE, unsave the item instead."
  (interactive)
  (lem-ui-with-item
      (let* ( ;(id (lem-ui--id-from-prop))
             (type (lem-ui--item-type))
             (s-str (if unsave "unsaved" "saved"))
             (s-bool (if unsave :json-false t))
             (json (lem-ui--property 'json))
             (saved-p (alist-get 'saved json)))
        (cond ((and unsave (eq saved-p :json-false))
               (message "You can only unsave saved items."))
              ((eq type 'post)
               (let ((json (lem-save-post id s-bool))
                     (my-vote (alist-get 'my_vote json)))
                 (lem-ui--update-item-json (alist-get 'post_view json))
                 (lem-ui-update-bt-byline-from-json my-vote s-bool)
                 (message "%s %s %s!" type id s-str)))
              ((eq type 'comment)
               (let ((json (lem-save-comment id s-bool))
                     (my-vote (alist-get 'my_vote json)))
                 (lem-ui--update-item-json (alist-get 'comment_view json))
                 (lem-ui-update-bt-byline-from-json my-vote s-bool)
                 (message "%s %s %s!" type id s-str)))
              (t
               (message "You can only save posts and comments."))))
    :number))

(defun lem-ui-unsave-item ()
  "Unsave item at point."
  (interactive)
  (lem-ui-save-item :unsave))

(defun lem-ui-save-item-toggle ()
  "Toggle saved status of item at point."
  (interactive)
  (let* ((json (lem-ui--property 'json))
         (saved-p (alist-get 'saved json)))
    (if (eq saved-p :json-false)
        (lem-ui-save-item)
      (lem-ui-unsave-item))))

(defun lem-ui-view-saved-items (&optional id sort limit page)
  "View saved items of the current user, or of user with ID.
SORT. LIMIT. PAGE."
  (interactive)
  (let* ((saved-only (lem-api-get-person-saved-only
                      (or id lem-user-id)
                      sort (or limit lem-ui-comments-limit) page))
         (posts (alist-get 'posts saved-only))
         (comments (alist-get 'comments saved-only))
         (buf "*lem-saved-items*"))
    (lem-ui-with-buffer buf 'lem-mode nil nil
      (lem-ui-insert-heading "SAVED POSTS")
      (lem-ui-render-posts posts)
      (lem-ui-insert-heading "SAVED COMMENTS")
      (lem-ui-render-comments comments)
      (lem-ui--init-view))))

;;; COMMUNITIES

(defun lem-ui-view-communities (&optional type sort limit)
  "View Lemmy communities.
TYPE must be one of `lem-listing-types'.
SORT must be one of `lem-sort-types'.
LIMIT is the max results to return."
  (interactive)
  (let* ((json (lem-list-communities type sort limit))
         (list (alist-get 'communities json))
         (buf "*lem-communities*"))
    (lem-ui-with-buffer buf 'lem-mode nil nil
      (cl-loop for c in list
               for id = (alist-get 'id (alist-get 'community c))
               for view = (lem-get-community id nil)
               do (lem-ui-render-community view :stats :view))
      (lem-ui-set-buffer-spec
       type sort #'lem-ui-view-communities 'communities))))

;;; PATCH VTABLE (fixed in 30.0.50, needed in 29.1):

(defvar-keymap lem-vtable-map
  "S" #'lem-vtable-sort-by-current-column
  "{" #'vtable-narrow-current-column
  "}" #'vtable-widen-current-column
  "g" #'lem-vtable-revert-command
  "M-<left>" #'vtable-previous-column
  "M-<right>" #'vtable-next-column)

(defun lem-vtable-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (vtable-current-column)
    (user-error "No current column"))
  (let* ((table (vtable-current-table))
         (last (car (last (vtable-sort-by table))))
         (index (vtable-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (vtable-sort-by table)
          (delq (assq index (vtable-sort-by table))
                (vtable-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (vtable-sort-by table)
          (append (vtable-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (lem-vtable-revert))

(defun lem-vtable-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (vtable-current-table)))
    (when (vtable-objects-function table)
      (setf (vtable-objects table) (funcall (vtable-objects-function table))))
    (vtable--clear-cache table))
  (lem-vtable-revert))

(defun lem-vtable-beginning-of-table ()
  "Go to the start of the current table."
  ;; pred arg stops `vtable-revert' from deleting non-table info in our
  ;; buffer.
  (if (text-property-search-backward 'vtable (vtable-current-table) t)
      (point)
    (goto-char (point-min))))

(defun lem-vtable-end-of-table ()
  "Go to the end of the current table."
  (if (text-property-search-forward 'vtable (vtable-current-table) t)
      (point)
    (goto-char (point-max))))

(defun lem-vtable-revert ()
  "Regenerate the table under point."
  (let ((table (vtable-current-table))
        (object (vtable-current-object))
        (column (vtable-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (lem-vtable-beginning-of-table) (lem-vtable-end-of-table))
    (vtable-insert table)
    (when object
      (vtable-goto-object object))
    (when column
      (vtable-goto-column column))))

(define-button-type 'lem-tl-button
  'follow-link t
  'help-echo "View community"
  'action #'lem-ui-view-community-at-point-tl)

(defun lem-ui-return-community-obj (community)
  "Return a vtable object for COMMUNITY."
  (let-alist community
    (cl-loop for i in
             (list
              (propertize .community.title
                          'id .community.id
                          'type 'lem-tl-button)
              .counts.subscribers
              .counts.users_active_month .counts.posts
              (if (equal "Subscribed" .subscribed)
                  "*"
                "")
              .community.actor_id
              ;; .community.description
              )
             collect (propertize
                      (if (numberp i)
                          (number-to-string i)
                        i)
                      'id .community.id
                      'lem-type 'community))))

(defun lem-ui-view-communities-tl (&optional type sort limit)
  "View Lemmy communities in a tabulated list.
TYPE must be one of `lem-listing-types'.
SORT must be one of `lem-sort-types'.
LIMIT is the max results to return."
  (interactive)
  (let* ((json (lem-list-communities (or type "All")
                                     (or sort "TopAll")
                                     (or limit "50")))
         ;; (list (alist-get 'communities json))
         (buf "*lem-communities*"))
    (lem-ui-with-buffer buf 'lem-mode nil nil
      (lem-ui-render-instance (lem-get-instance) :stats nil)
      (make-vtable
       :use-header-line nil
       :columns '("Name" "Members" "Monthly users" "Posts"
                  (:name "Sub" :min-width 4) "URL") ; "Description")
       :objects-function
       (lambda ()
         (cl-loop for c in (alist-get 'communities json)
                  collect (lem-ui-return-community-obj c)))
       :row-colors  '(highlight vtable)
       :divider-width 1
       :keymap lem-vtable-map
       :actions '("RET" lem-ui-view-community-at-point-tl
                  "s" lem-ui-subscribe-to-community-at-point-tl))
      (lem-ui-set-buffer-spec
       type sort #'lem-ui-view-communities-tl 'communities))))

;; actions are called on the column's object, but we use text props instead,
;; so we have to reimplement these for tl:
(defun lem-ui-view-community-at-point-tl (_)
  "View community at point, from tabulated list."
  (lem-ui-view-community-at-point))

(defun lem-ui-subscribe-to-community-at-point-tl (_)
  "Subscribe to community at point, from tabulated list."
  (lem-ui-subscribe-to-community-at-point))

(defun lem-ui-subscribe-to-community (&optional id)
  "Subscribe to a community, using ID or prompt for a handle."
  (interactive)
  (let* ((handle (unless id
                   (read-string "Handle of community to subscribe to: ")))
         (community (unless id
                      (lem-get-community nil handle))))
    (if-let ((id (or id (lem-ui-get-community-id community)))
             (fol (lem-follow-community id t))
             (comm (alist-get 'community
                              (alist-get 'community_view fol)))
             (name (or (alist-get 'title comm)
                       (alist-get 'name comm))))
        (message "Subscribed to community %s!" name)
      (message "something went wrong."))))

(defun lem-ui-subscribe-to-community-at-point ()
  "Subscribe to community at point."
  (interactive)
  (lem-ui-with-item
      (if (not (equal 'community (lem-ui--item-type)))
          (message "no community at point?")
        (lem-ui-subscribe-to-community id))
    :number))

(defun lem-ui-unsubscribe-from-community ()
  "Prompt for a subscribed community and unsubscribe from it."
  (interactive)
  (lem-ui-do-subscribed-completing
   "Unsubscribe from community: "
   (lambda (id choice)
     (when (and (y-or-n-p (format "Unsubscribe from %s?" choice))
                (lem-follow-community id :json-false))
       (message "Community %s unsubscribed!" choice)))))

(defun lem-ui-view-community-at-point ()
  "View community at point."
  (interactive)
  (lem-ui-with-item
      (lem-ui-view-community id)))

(defun lem-ui--communities-alist (communities)
  "Return an alist of name/description and ID from COMMUNITIES."
  (cl-loop for item in communities
           collect (let-alist item
                     (list (format "%s@%s"
                                   .community.name
                                   (url-domain
                                    (url-generic-parse-url .community.actor_id)))
                           .community.description
                           .community.id))))

(defun lem-ui-do-subscribed-completing (prompt-str action-fun)
  "Read a subscribed community with PROMPT-STR and call ACTION-FUN on it."
  (let* ((communities (lem-api-get-subscribed-communities))
         (subs (lem-ui--communities-alist communities))
         (completion-extra-properties
          (list :annotation-function
                (lambda (c)
                  (let ((annot (nth 1 (assoc c subs #'equal))))
                    (concat
                     (propertize " " 'display
                                 '(space :align-to (- right-margin 51)))
                     (string-limit (car (string-lines annot)) 50))))))
         (choice (completing-read prompt-str subs))
         (id (nth 2 (assoc choice subs #'equal))))
    (funcall action-fun id choice)))

(defun lem-ui-jump-to-subscribed ()
  "Prompt for a subscribed community and view it."
  (interactive)
  (lem-ui-do-subscribed-completing
   "Jump to community: "
   (lambda (id _choice)
     (lem-ui-view-community id 'posts))))

(defun lem-ui-view-community (id &optional item sort limit page)
  "View community with ID.
ITEM is a symbol, either posts or comments.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is the page number of items to display, a string."
  (let* ((community (lem-get-community id))
         ;; (view (alist-get 'community_view community))
         (buf "*lem-community*")
         ;; in case we set community posts, then switch to comments:
         (sort (if (eq item 'comments)
                   (unless (lem-comment-sort-type-p sort)
                     lem-default-comment-sort-type)
                 (or sort lem-default-sort-type)))
         (items (if (eq item 'comments)
                    (alist-get 'comments
                               (lem-api-get-community-comments
                                id nil sort limit page))
                  (alist-get 'posts
                             (lem-api-get-community-posts-by-id
                              id nil sort limit page)))) ; no sorting
         (bindings (lem-ui-view-options 'community)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (lem-ui-render-community community :stats :view)
      (if (eq item 'comments)
          (progn
            (lem-ui-insert-heading "comments")
            (lem-ui-render-comments items)) ; no type
        (lem-ui-insert-heading "posts")
        (lem-ui-render-posts items nil :trim)) ; no children
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-community
                              (or item 'posts) page))))

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

(defun lem-ui-render-communities (communities)
  "Render COMMUNITIES.
TYPE
SORT."
  (cl-loop for x in communities
           do (lem-ui-render-community x :stats)))

(defun lem-ui--names-list (names-list type)
  "Return list of name, id, and url for each moderator in NAMES-LIST.
TYPE is a symbol, either person or moderator."
  (cl-loop for x in names-list
           collect (let-alist (alist-get type x)
                     (list (or .display_name .name)
                           .id
                           .actor_id))))

(defun lem-ui-render-community (community &optional stats view brief)
  "Render header details for COMMUNITY.
BUFFER is the one to render in, a string.
STATS are the community's stats to print.
VIEW means COMMUNITY is a community_view.
BRIEF means show fewer details, it is used on the current user's
profile page."
  (let* ((mods-list (unless brief (alist-get 'moderators community)))
         (mods (unless brief (lem-ui--names-list mods-list 'moderator)))
         (community (if view
                        (alist-get 'community_view community)
                      community)))
    (let-alist community
      (let ((desc (if brief
                      ""
                    (if view
                        (when .community.description
                          (lem-ui-render-body .community.description
                                              community))
                      ;; more communities list means we have 'community
                      ;; objects, requiring .community.description:
                      (when-let ((desc (or .community.description
                                           .description)))
                        (lem-ui-render-body desc community))))))
        (insert
         (propertize
          (concat
           (propertize .community.title
                       'face '(:weight bold))
           " | "
           (lem-ui-font-lock-comment
            (concat "!" .community.name))
           "\n"
           (lem-ui-font-lock-comment .community.actor_id)
           (unless brief (concat "\n" desc "\n"
                                 lem-ui-horiz-bar "\n")))
          'json community
          'byline-top t ; next/prev hack
          'id .community.id
          'lem-type 'community)))
      ;; stats:
      (when stats
        (lem-ui-render-stats .counts.subscribers
                             .counts.posts
                             .counts.comments))
      (unless brief
        (insert .subscribed "\n")))
    ;; mods:
    (when mods
      (lem-ui-insert-people mods "mods: ")
      (insert
       "\n" lem-ui-horiz-bar "\n"))
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
  (lem-ui-with-item
      (let ((type (lem-ui--property 'lem-type))
            (id (or (lem-ui--property 'community-id)
                    (lem-ui--property 'id)))) ; community header
        (if (eq type 'instance)
            (user-error "Item has no community")
          (lem-ui-view-community id)))))

;;; REPLIES

(defun lem-ui-view-replies-unread ()
  "View unread replies."
  (interactive)
  (lem-ui-view-replies :unread))

(defun lem-ui-view-replies (&optional unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (let* ((replies (lem-get-replies (if unread "true" nil)))
         (list (alist-get 'replies replies))
         (buf "*lem-replies*"))
    (lem-ui-with-buffer buf 'lem-mode nil :no-bindings
      (lem-ui-render-replies list)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil nil #'lem-ui-view-replies
                              'comment-reply nil unread))))

(defun lem-ui-render-replies (replies)
  "Render REPLIES, reply comments to the current user."
  (cl-loop for reply in replies
           do (lem-ui-render-comment reply :reply)))

(defun lem-ui-mark-reply-comment-read ()
  "Mark the comment-reply at point as read."
  (interactive)
  (let ((id (lem-ui--property 'id)))
    (lem-mark-comment-reply-read id)))

(defun lem-ui-mark-all-read ()
  "Mark all replies as read."
  (interactive)
  (lem-mark-all-read))

(defun lem-ui-view-mentions (&optional unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (let* ((mentions (lem-get-mentions (if unread "true" nil)))
         (list (alist-get 'mentions mentions))
         (buf "*lem-mentions*"))
    (lem-ui-with-buffer buf 'lem-mode nil :no-bindings
      (lem-ui-render-mentions list)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil nil #'lem-ui-view-mentions
                              'mention nil unread))))

(defun lem-ui-render-mentions (mentions)
  "Render mentions MENTIONS."
  (cl-loop for men in mentions
           for comment = (alist-get 'comment men)
           do (insert
               (lem-ui-format-comment comment)
               "\n")))

(defun lem-ui-view-private-messages (&optional unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (let* ((private-messages (lem-get-private-messages (if unread "true" nil)))
         (list (alist-get 'private_messages private-messages))
         (buf "*lem-private-messages*"))
    (lem-ui-with-buffer buf 'lem-mode nil :no-bindings
      ;; (lem-ui-render-private-messages list))))
      (lem-ui-render-private-messages list)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil nil #'lem-ui-view-private-messages
                              'private-message nil unread))))

(defun lem-ui-render-private-messages (private-messages)
  "Render private messages PRIVATE-MESSAGES."
  (cl-loop for pm in private-messages
           do (insert
               (lem-ui-format-private-message pm)
               "\n")))

(defun lem-ui-mark-private-message-read ()
  "Mark the private message at point as read."
  (interactive)
  (let ((id (lem-ui--property 'id)))
    (lem-mark-private-message-read id)))

;;; EDIT/DELETE POSTS/COMMENTS

(defmacro lem-ui-with-own-item (item-type &rest body)
  "Call BODY if ITEM-TYPE is at point and owned by the current user."
  (declare (debug t)
           (indent 1))
  `(cond ((not (eq ,item-type (lem-ui--property 'lem-type)))
          (message "No %s at point?" ,item-type))
         ((not (equal lem-user-id (lem-ui--property 'creator-id)))
          (message "You can only modify your own items"))
         (t
          ,@body)))

(defun lem-ui-edit-comment ()
  "Edit comment at point if possible."
  (interactive)
  (lem-ui-with-own-item 'comment
    (let* ((id (lem-ui--property 'id))
           (json (lem-ui--property 'json))
           (old-str (alist-get 'content (alist-get 'comment json)))
           (new-str (read-string "Edit comment: " old-str)))
      (lem-edit-comment id new-str))))

(defun lem-ui-delete-item (item fun &optional restore)
  "Delete item of type ITEM at point, calling FUN.
If RESTORE, restore the item instead."
  (lem-ui-with-own-item item
    (let* ((id (lem-ui--property 'id)))
      (when (y-or-n-p (format "%s %s?"
                              (if restore "Restore" "Delete")
                              item))
        (progn
          (funcall fun id (if restore :json-false t))
          (message "%s %s %s!" item id
                   (if restore "restored" "deleted")))))))

(defun lem-ui-delete-comment ()
  "Delete comment at point."
  (interactive)
  (lem-ui-delete-item 'comment #'lem-delete-comment))

(defun lem-ui-delete-post ()
  "Delete post at point."
  (interactive)
  (lem-ui-delete-item 'post #'lem-delete-post))

(defun lem-ui-restore-post ()
  "Restore deleted post at point."
  (interactive)
  (lem-ui-delete-item 'post #'lem-delete-post :restore))

(defun lem-ui-delete-post-or-comment ()
  "Delete post or comment at point."
  (interactive)
  ;; TODO: check for deleted status first
  (let ((type (lem-ui--property 'lem-type)))
    (cond ((eq type 'post)
           (lem-ui-delete-post))
          ((eq type 'comment)
           (lem-ui-delete-comment)))))

;;; COMMENTS

(defun lem-ui-render-comment (comment &optional reply)
  "Render single COMMENT.
REPLY means it is a comment-reply object."
  ;; SORT must be a member of `lem-comment-sort-types'."
  (insert
   (lem-ui-format-comment comment nil reply)
   "\n"))

(defun lem-ui-render-comments (comments)
  "Render COMMENTS, a list of comment objects.
;; TYPE
;; SORT.
For viewing a plain list of comments, not a hierarchy."
  (cl-loop for x in comments
           do (lem-ui-render-comment x)))

;;; THREADED COMMENTS:
;; Path: "The path / tree location of a comment, separated by dots, ending
;; with the comment's id. Ex: 0.24.27"
;; https://github.com/LemmyNet/lemmy/blob/63d3759c481ff2d7594d391ae86e881e2aeca56d/crates/db_schema/src/source/comment.rs#L39
(defvar-local lem-comments-hierarchy nil)
(defvar-local lem-comments-raw nil)

(defun lem-ui--build-and-render-comments-hierarchy (comments)
  "Build `lem-comments-hierarchy', a hierarchy, from COMMENTS, and render."
  (setq lem-comments-raw comments)
  (let ((list (alist-get 'comments comments)))
    (lem-ui--build-hierarchy list)) ; sets `lem-comments-hierarchy'
  (with-current-buffer (get-buffer-create "*lem-post*")
    (let ((inhibit-read-only t))
      (lem--hierarchy-print-line lem-comments-hierarchy
                                 (hierarchy-labelfn-indent
                                  (lambda (item indent)
                                    (lem-ui-format-comment item indent))
                                  (lem-ui-symbol 'reply-bar))))))

(defun lem-ui-get-comment-path (comment)
  "Get path value from COMMENT."
  (alist-get 'path
             (alist-get 'comment comment)))

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

(defun lem-ui-split-path (path)
  "Call split string on PATH with \".\" separator."
  (split-string path "\\."))

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

(defun lem-ui--handle-from-user-url (url)
  "Return a formatted user handle from user URL."
  (let* ((parsed (url-generic-parse-url url))
         (host (url-host parsed))
         (file (url-filename parsed)))
    (save-match-data
      ;; TODO: add further legit urls:
      (when (string-match "^/u/[_[:alnum:]]+$" file)
        (let ((split (split-string file "/" t)))
          (propertize
           (concat (cadr split) "@" host)
           ;; props
           ))))))

(defun lem-ui-format-comment (comment &optional indent reply)
  "Format COMMENT, optionally with INDENT amount of indent bars.
REPLY means it is a comment-reply object."
  ;; NB: no stray requests in here.
  (let-alist comment
    (let ((content (when .comment.content
                     (lem-ui-render-body .comment.content
                                         (alist-get 'comment comment)
                                         indent)))
          (indent-str (when indent
                        (make-string indent (string-to-char
                                             (lem-ui-symbol 'reply-bar)))))
          (handle (lem-ui--handle-from-user-url .creator.actor_id))
          (admin-p (eq t .creator.admin))
          (mod-p (cl-member .creator.id lem-ui-post-community-mods-ids))
          (op-p (eq .comment.creator_id .post.creator_id)))
      (push .comment.id lem-ui-current-items) ; pagination
      (propertize
       (concat
        (lem-ui-top-byline nil nil
                           (or .creator.display_name .creator.name)
                           .counts.score
                           .comment.published
                           nil nil nil
                           op-p admin-p mod-p nil handle)
        "\n"
        (or content "")
        "\n"
        (lem-ui-bt-byline .counts.score .counts.child_count .my_vote .saved)
        "\n"
        lem-ui-horiz-bar
        "\n")
       'json comment
       'id (if reply .comment_reply.id .comment.id)
       'post-id .comment.post_id
       'community-id .post.community_id
       'creator-id .creator.id
       'lem-type (if reply 'comment-reply 'comment)
       'line-prefix indent-str))))

;; TODO: refactor format funs? will let-alist dot notation work?
(defun lem-ui-format-private-message (private-message &optional indent)
  "Format PRIVATE-MESSAGE, optionally with INDENT amount of indent bars."
  (let-alist private-message
    (let ((content (when .private_message.content
                     (lem-ui-render-body
                      .private_message.content
                      (alist-get 'private_message private-message))))
          (indent-str (when indent
                        (make-string indent
                                     (string-to-char
                                      (lem-ui-symbol 'reply-bar))))))
      (push .private_message.id lem-ui-current-items) ; pagination
      (propertize
       (concat
        (lem-ui-top-byline nil nil
                           (or .creator.display_name .creator.name)
                           nil ;.counts.score
                           .private_message.published)
        "\n"
        (or content "")
        "\n"
        "\n"
        lem-ui-horiz-bar
        "\n")
       'json private-message
       'id .private_message.id
       'creator-id .creator.id
       'lem-type 'private-message
       'line-prefix indent-str))))

(defun lem-ui-render-post-comments (post-id &optional sort limit)
  "Render a hierarchy of post's comments.
POST-ID is the post's id.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of items to return."
  ;; TODO: TYPE_ default:
  (let* ((comments (lem-api-get-post-comments
                    post-id "All" sort (or limit lem-ui-comments-limit))))
    (if (eq 'string (type-of comments))
        (message comments) ; server error
      (let ((unique-comments (cl-remove-duplicates comments)))
        (lem-ui--build-and-render-comments-hierarchy unique-comments)))))

(defun lem-ui-plural-symbol (symbol)
  "Return a plural of SYMBOL."
  (if (eq symbol 'community)
      'communities
    (intern
     (concat (symbol-name symbol) "s"))))

(defun lem-ui-remove-displayed-items (items type)
  "Remove item from ITEMS if it is in `lem-ui-current-items'.
TYPE is the item type.
ITEMS should be an alist of the form '\=(plural-name ((items-list)))'."
  (cl-remove-if
   (lambda (x)
     (let ((id (alist-get 'id
                          (alist-get type x))))
       (cl-member id lem-ui-current-items)))
   (alist-get (lem-ui-plural-symbol type)
              items)))

(defun lem-ui-more ()
  "Append more items to the current view."
  (interactive)
  (cond ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-post)
         (lem-ui-more-items 'comment 'lem-api-get-post-comments
                            'lem-ui--build-and-render-comments-hierarchy))
        ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-community)
         (if (eq (lem-ui-get-buffer-spec :item) 'posts)
             (lem-ui-more-items 'post 'lem-api-get-community-posts-by-id
                                'lem-ui-render-posts)
           (lem-ui-more-items 'comment 'lem-api-get-community-comments-by-id
                              'lem-ui-render-comments)))
        ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-instance)
         (lem-ui-more-items 'post 'lem-api-get-instance-posts
                            'lem-ui-render-posts-instance))
        ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-user)
         ;; TODO: user overview view type:
         (if (equal (lem-ui-get-buffer-spec :item) "posts")
             (lem-ui-more-items 'post 'lem-api-get-person-posts
                                'lem-ui-render-posts)
           (lem-ui-more-items 'comment 'lem-api-get-person-comments
                              'lem-ui-render-comments)))
        ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-communities)
         (lem-ui-more-items 'community 'lem-list-communities
                            'lem-ui-render-communities))
        (t (message "More type not implemented yet"))))

(defun lem-ui-more-items (type get-fun render-fun)
  "Add one more page of items of TYPE to the current view.
GET-FUN is the name of a function to fetch more items.
RENDER-FUN is the name of a function to render them."
  (message "Loading more items...")
  (let* ((page (1+ (lem-ui-get-buffer-spec :page)))
         (id (save-excursion
               (goto-char (point-min))
               (lem-ui--property 'id)))
         (sort (lem-ui-get-buffer-spec :sort))
         (all-items
          ;; get-instance-posts have no need of id arg:
          (cond ((or (eq get-fun 'lem-api-get-instance-posts)
                     (eq get-fun 'lem-list-communities))
                 (funcall get-fun
                          (or (lem-ui-get-buffer-spec :listing-type) "All")
                          sort
                          lem-ui-comments-limit
                          page))
                ;; user funs have no list-type arg:
                ((eq (lem-ui-get-buffer-spec :view-fun) 'lem-ui-view-user)
                 (funcall get-fun id sort
                          lem-ui-comments-limit page))
                (t
                 (funcall get-fun
                          id
                          (or (lem-ui-get-buffer-spec :listing-type) "All")
                          sort
                          lem-ui-comments-limit
                          page))))
         (no-duplicates (lem-ui-remove-displayed-items all-items type)))
    (setf (alist-get (lem-ui-plural-symbol type) all-items)
          no-duplicates)
    (lem-ui-set-buffer-spec (lem-ui-get-buffer-spec :listing-type)
                            (lem-ui-get-buffer-spec :sort)
                            (lem-ui-get-buffer-spec :view-fun)
                            (lem-ui-get-buffer-spec :item)
                            page)
    (goto-char (point-max))
    (let ((old-max (point))
          (inhibit-read-only t))
      ;; NB: `lem-ui-current-items' is updated during rendering:
      (if (eq render-fun 'lem-ui--build-and-render-comments-hierarchy)
          (funcall render-fun all-items)
        (funcall render-fun (alist-get (lem-ui-plural-symbol type)
                                       all-items)))
      (goto-char old-max)
      (lem-ui--init-view)
      (message "Loading more items... [done]"))))

(defun lem-ui-post-goto-comment (comment-id)
  "Move point to comment with COMMENT-ID, a number, if possible."
  ;; TODO: implement forward-search/pagination
  (with-current-buffer "*lem-post*"
    (when-let ((match (text-property-search-forward 'id comment-id t)))
      (goto-char (prop-match-beginning match)))))

(defun lem-ui-view-comment-post (&optional post-id comment-id)
  "View post of comment at point, or of POST-ID.
If COMMENT-ID is provided, move point to that comment."
  (interactive)
  (if (not (or post-id
               (eq (lem-ui--item-type) 'comment)
               (eq (lem-ui--item-type) 'comment-reply)))
      (message "Not at a comment?")
    (let* ((post (or post-id (lem-ui--property 'post-id))))
      (lem-ui-view-post post)
      (lem-ui-post-goto-comment comment-id))))

;;; LIKES / VOTES

;; TODO: check for liked status before changing it
(defun lem-ui-like-item (&optional type)
  "Like (upvote) item at point.
TYPE should be either :unlike, :dislike, or nil to like."
  (interactive)
  (lem-ui-with-item
      (let* ((item (lem-ui--property 'lem-type))
             (fun (if (eq item 'post)
                      #'lem-like-post
                    #'lem-like-comment))
             (score (cond ((eq type :unlike)
                           0)
                          ((eq type :dislike)
                           -1)
                          (t 1)))
             (like-str (cond ((eq type :unlike)
                              "unliked")
                             ((eq type :dislike)
                              "disliked")
                             (t "liked"))))
        (if (or (eq item 'post)
                (eq item 'comment)
                (eq item 'comment-reply))
            (progn
              (let* ((vote (funcall fun id score))
                     (obj (intern (concat (symbol-name item) "_view")))
                     (i (alist-get obj vote))
                     (saved (alist-get 'saved i))
                     (my-vote (alist-get 'my_vote i)))
                (when i ; no my_vote if we unliked in 0.19-rc5?
                  (lem-ui--update-item-json i)
                  (lem-ui-update-bt-byline-from-json my-vote saved)
                  (message "%s %s %s!" item id like-str))))))
    :number))

(defun lem-ui-dislike-item ()
  "Dislike (downvote) item at point."
  (interactive)
  (lem-ui-like-item :dislike))

(defun lem-ui-unlike-item ()
  "Unlike item at point."
  (interactive)
  (lem-ui-like-item :unlike))

(defun lem-ui-like-item-toggle ()
  "Toggle like status of item at point."
  (interactive)
  (lem-ui-with-item
      (let* ((json (lem-ui--property 'json))
             (my-vote (alist-get 'my_vote json)))
        (if json
            (cond ((eq my-vote -1)
                   (lem-ui-unlike-item))
                  ((eq my-vote 1)
                   (lem-ui-dislike-item))
                  ((or (eq my-vote nil)
                       (eq my-vote 0))
                   (lem-ui-like-item)))))))

(defun lem-ui--update-item-json (new-json)
  "Replace the json property of item at point with NEW-JSON."
  (let ((inhibit-read-only t)
        (region (fedi--find-property-range 'json (point) :backwards)))
    (add-text-properties (car region) (cdr region)
                         `(json ,new-json))))

;;; USERS

(defun lem-ui-render-users (json)
  "JSON."
  ;; (let ((users (alist-get 'users json)))
  (cl-loop for user in json
           do (lem-ui-render-user user)))

(defun lem-ui-render-user (json)
  "Render user with data JSON."
  (let-alist json
    (insert
     (propertize
      (concat
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
      'lem-type (caar json)))))

(defun lem-ui-render-user-subscriptions (json)
  "Render subscribed communities from JSON data."
  (cl-loop for community in json
           do (lem-ui-render-community community nil nil :subscription)))

(defun lem-ui-view-user (id &optional view-type sort limit current-user)
  "View user with ID.
VIEW-TYPE must be a member of `lem-user-view-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is max items to show.
CURRENT-USER means we are displaying the current user's profile."
  (let ((user-json (lem-api-get-person-by-id id sort limit))
        (sort (or sort lem-default-sort-type))
        (buf "*lem-user*")
        (bindings (lem-ui-view-options 'user)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (when current-user
        (let-alist current-user
          (lem-ui-render-user .local_user_view)
          (insert "Subscribed communities:\n")
          (lem-ui-render-user-subscriptions .follows)))
      (let-alist user-json
        (unless current-user
          (lem-ui-render-user .person_view))
        (cond ((equal view-type "posts")
               (lem-ui-insert-heading "posts")
               (lem-ui-render-posts .posts :community :trim))
              ((equal view-type "comments")
               (lem-ui-insert-heading "comments")
               (lem-ui-render-comments .comments))
              (t ; no arg: overview
               (lem-ui-insert-heading "overview")
               ;; web app just does comments then posts for "overview"?:
               (lem-ui-render-comments .comments)
               (lem-ui-render-posts .posts :community :trim)))
        (lem-ui--init-view)
        ;; FIXME: don't confuse view-type and listing-type (& fix cycling):
        (lem-ui-set-buffer-spec
         view-type sort #'lem-ui-view-user view-type)))))

(defun lem-ui-view-own-profile ()
  "View profile of the current user."
  (interactive)
  (let* ((current-user (lem-api-get-current-user)))
    (lem-ui-view-user lem-user-id nil nil nil current-user)))

;; TODO: view own profile: full sort types
;; overview/comments/posts/saved listings
;; list of communities moderated
;; list subscribed

(defun lem-ui-view-item-user ()
  "View user of item at point."
  (interactive)
  (lem-ui-with-item
      (let ((user (lem-ui--property 'creator-id)))
        (lem-ui-view-user user 'overview))))

(defun lem-ui-view-user-at-point ()
  "View user at point."
  (interactive)
  (lem-ui-with-item
      (lem-ui-view-user id 'overview)))

(defun lem-ui-message-user-at-point ()
  "Send private message to user at point."
  (interactive)
  (lem-ui-with-item
      (let ((message (read-string "Private message: ")))
        (lem-send-private-message message id))))

;;; IMAGES
(defun lem-ui-insert-images ()
  "Insert any image-url images in the buffer with `shr-insert-image'.
It's a cheap hack, alas."
  (save-excursion
    (goto-char (point-min))
    (let (match
          (url-user-agent lem-user-agent))
      (while (setq match (text-property-search-forward 'image-url))
        (goto-char (prop-match-beginning match))
        ;; (re-search-forward "\*" nil :no-error) ; * is just for no alt-text
        ;; (backward-char 1)
        (progn (shr-insert-image)
               (delete-char 1))
        (goto-char (prop-match-end match))))))

(defun lem-ui-copy-item-url ()
  "Copy the URL (ap_id) of the post or comment at point."
  (interactive)
  (lem-ui-with-item
      (let* ((json (lem-ui--property 'json))
             (item (or (alist-get 'comment json)
                       (alist-get 'post json)))
             (url (alist-get 'ap_id item)))
        (if item
            (progn (kill-new url)
                   (message "url %s copied!" url))))))

(provide 'lem-ui)
;;; lem-ui.el ends here
