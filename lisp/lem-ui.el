;;; lem-ui.el --- An interface for lemmy instances -*- lexical-binding: t; -*-

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

;; An interface to Lemmy, the federated link-aggregator and forum software.
;; See <https://joinlemmy.org>.

;;; Code:

;; (require 'hierarchy)
(require 'cl-lib)
(require 'shr)
(require 'hierarchy)
(require 'vtable)

(require 'widget)
(require 'wid-edit)
;; (eval-when-compile
;; (require 'wid-edit))

(require 'markdown-mode)

(require 'fedi-post) ; handle regex

(require 'lem-api)

(defvar lem-listing-types)
(defvar lem-comment-sort-types)
(defvar lem-default-comment-sort-type)
(defvar lem-sort-types)
(defvar lem-default-sort-type)
(defvar lem-default-listing-type)
(defvar lem-user-items-types)
(defvar lem-items-types)
(defvar lem-search-types)
(defvar lem-inbox-types)
(defvar lem-user-id)
(defvar lem-user-view-sort-types)
(defvar lem-inbox-sort-types)

(defvar lem-enable-relative-timestamps)

(defvar-local lem-ui-post-community-mods-ids nil
  "A list of ids of the moderators of the community of the current post.")

(autoload 'lem-mode "lem.el")
(autoload 'lem-comment-sort-type-p "lem.el")
(autoload 'lem-sort-type-p "lem.el")
(autoload 'lem-user-view-sort-type-p "lem.el")

(defface lem-ui-user-face '((t :inherit warning :underline t))
  "Face user displaying usernames.")

(defface lem-ui-community-face '((t :inherit success :underline t))
  "Face for displaying communities.")

(defface lem-ui-widget-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face used for widgets.")

;;; HIERARCHY PATCHES

(defun lem--hierarchy-labelfn-indent (labelfn) ; &optional indent-string)
  ;; prop attrib cycle-fun)
  "Return a function rendering LABELFN indented with INDENT-STRING.

INDENT-STRING defaults to a 2-space string.  Indentation is
multiplied by the depth of the displayed item.
PROP is a property name, a symbol.
ATTRIB is the prop's attribute, a kw symbol.

CYCLE-FUN is called with one argument, the current indent level
inside the loop, and is used to return the color for that indentation.
Currently it is always `lem-ui-cycle-colors'."
  ;; (let ((indent-string (or indent-string "  ")))
  (lambda (item indent)
    ;; don't manually insert indent string here, as it isn't a line-prefix
    ;; and so point can end up on it, which we don't want.
    ;; comments "seem" to work fine without any of this?
    ;; and our format-comment fun handles line-prefixing:
    ;; (dotimes (index indent)
    ;;   (insert
    ;;    (funcall 'propertize indent-string
    ;;             ;; theres a better way to do this, but we need to funcall
    ;;             ;; cycle-fun on index to work
    ;;             prop (list attrib
    ;;                        (funcall cycle-fun index)))))
    (funcall labelfn item indent)))

;; (defun lem--hierarchy-print (hierarchy &optional to-string)
;;   "Insert HIERARCHY in current buffer as plain text.

;; Use TO-STRING to convert each element to a string.  TO-STRING is
;; a function taking an item of HIERARCHY as input and returning a
;; string.

;; Calls `lem--hierarchy-print-line' with `hierarchy-labelfn-indent' as
;; second argument."
;;   (let ((to-string (or to-string (lambda (item) (format "%s" item)))))
;;     (lem--hierarchy-print-line
;;      hierarchy
;;      (lem--hierarchy-labelfn-indent
;;       (lambda (item _)
;;         (funcall to-string item))))))

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

;;; WIDGETS

(defvar lem-widget-keymap
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "\t" 'widget-forward)
    ;; (define-key map "\e\t" 'widget-backward)
    ;; (define-key map [(shift tab)] 'widget-backward)
    ;; (put 'widget-backward :advertised-binding [(shift tab)])
    ;; (define-key map [backtab] 'widget-backward)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [touchscreen-begin] 'widget-button-click)
    ;; The following definition needs to avoid using escape sequences that
    ;; might get converted to ^M when building loaddefs.el
    (define-key map [(control ?m)] 'widget-button-press)
    map)
  "Keymap containing useful binding for buffers containing widgets.
Recommended as a parent keymap for modes using widgets.
Note that such modes will need to require wid-edit.")

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
   "\\b")
  "Regex matching a URL.")

(defvar lem-ui-handle-regex fedi-post-handle-regex)

(defvar lem-ui-community-regex
  (rx (| (any ?\( "\n" "\t "" ") bol) ; preceding things
      (group-n 1 ; = commuinty with !
        ?!
        (group-n 2 ; = community only
          (* (any ?- ?_ ?. "A-Z" "a-z" "0-9" )))
        (? ?@ ; = optional for lem-post.el
           (group-n 3 ; = domain only
             (* (not (any "\n" "\t" " "))))))
      (| "'" word-boundary))
  "Regex matching a lemmy community, ie \"!community@instance.com\".")

(defvar lem-ui-image-formats
  '("png" "jpg" "jpeg" "webp")
  "Image formats that we may want to render for post URLs.")

;;; CUSTOMS

(defgroup lem nil
  "Lemmy client."
  :prefix "lem-ui-"
  :group 'lem)

(defvar lem-ui-symbols fedi-symbols)

;;; UTILITIES

(defun lem-ui-make-fun (prefix suffix)
  "Make a function from PREFIX, a string, and SUFFIX, a symbol."
  (intern
   (concat prefix
           (symbol-name suffix))))

(defun lem-ui-hyphen-to-underscore (symbol)
  "Replace any - with _ in SYMBOL."
  (intern
   (string-replace "-" "_"
                   (symbol-name symbol))))

(defvar lem-ui-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

(defalias 'lem-ui-format-heading 'fedi-format-heading)

(defalias 'lem-ui-insert-heading 'fedi-insert-heading)

(defalias 'lem-ui-symbol 'fedi-symbol)

(defalias 'lem-ui-font-lock-comment 'fedi-font-lock-comment)

(defalias 'lem-ui-thing-json 'fedi-thing-json)

(defalias 'lem-ui--property 'fedi--property)

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

(defun lem-ui-handle-from-url (url &optional prefix)
  "Format a handle, user or community, from a URL.
PREFIX is a string, ! for community, @ for user."
  (let* ((parsed (url-generic-parse-url url))
         (domain (url-domain parsed))
         (filename (url-filename parsed))
         (item (car (last (split-string filename "/")))))
    (concat (or prefix "")
            item "@" domain)))

(defalias 'lem-ui-response-msg 'fedi-response-msg)

;; TODO: add to `lem-ui-with-buffer'? we almost always call it
;; TODO: factor out into fedi.el (just has 1 lem fn call)
(defun lem-ui--init-view ()
  "Initialize a lemmy view.
Inserts images and sets relative timestamp timers."
  (let ((inhibit-read-only t))
    ;; don't wrap long verbatim text:
    (setq truncate-lines t)
    ;; load images:
    (lem-ui-insert-images)
    ;; relative timestamps:
    (setq
     ;; Initialize with a minimal interval; we re-scan at least once
     ;; every 5 minutes to catch any timestamps we may have missed
     fedi-timestamp-next-update (time-add (current-time)
                                          (seconds-to-time 300)))
    (setq fedi-timestamp-update-timer
          (when lem-enable-relative-timestamps
            (run-at-time (time-to-seconds
                          (time-subtract fedi-timestamp-next-update
                                         (current-time)))
                         nil ;; don't repeat
                         #'fedi--update-timestamps-callback
                         (current-buffer)
                         nil)))))

(defun lem-ui-item-to-alist-key (item)
  "Given ITEM, a symbol, return a valid JSON key, item_view.
Item may be post, comment, community, etc."
  (intern
   (concat
    (symbol-name item) "_view")))

(defun lem-ui--current-indent ()
  "Return current indent level as an integer."
  (length (lem-ui--property 'line-prefix)))

;;; MACROS

(defmacro lem-ui-with-buffer (buffer mode-fun other-window bindings &rest body)
  "Evaluate BODY in a new or existing buffer called BUFFER.
MODE-FUN is called to set the major mode.
OTHER-WINDOW means call `switch-to-buffer-other-window' rather
than `pop-to-buffer'.
BINDINGS is a list of variables for which to display bidings.
Return the buffer."
  (declare (debug t)
           (indent 4))
  `(with-current-buffer (get-buffer-create ,buffer)
     (let* ((inhibit-read-only t)
            (sort-str (when (member 'lem-sort-types ,bindings)
                        "\\[lem-ui-cycle-sort]: cycle sort "))
            (listing-str (when (member 'lem-listing-types ,bindings)
                           "\\[lem-ui-cycle-listing-type]: cycle listing "))
            (view-str (when (or (member 'lem-items-types ,bindings)
                                (member 'lem-user-items-types ,bindings))
                        "\\[lem-ui-toggle-posts-comments]: toggle posts/comments "))
            (inbox-str (when (member 'lem-inbox-types ,bindings)
                         "\\[lem-ui-cycle-listing-type]: cycle message type "))
            (msg-str (concat listing-str
                             sort-str
                             view-str
                             inbox-str)))
       (erase-buffer)
       (funcall ,mode-fun)
       (if ,other-window
           (switch-to-buffer-other-window ,buffer)
         ;; (switch-to-buffer ,buffer))
         (pop-to-buffer ,buffer '(display-buffer-same-window)))
       ,@body
       (goto-char (point-min))
       (when ,bindings
         ;; this needs to come after media messages:
         ;; but this also kills any view-type messages
         ;; (sleep-for 1)
         (message
          (substitute-command-keys msg-str)))
       ,buffer)))

(defmacro lem-ui-with-item (type body &optional number)
  "Call BODY after fetching ID of thing (at point).
Thing can be anything handled by `lem-ui-thing-json', currently:
comment, post, community, or person.
If TYPE is all, don't check for item type.
Within this macro call, arg ID is available.
NUMBER means return ID as a number."
  (declare (debug t)
           (indent 1))
  `(if (and (not (eq ,type 'all))
            (not (eq ,type (lem-ui--property 'lem-type))))
       (user-error "No %s at point?" ,type)
     (let* ((id (lem-ui--id-from-prop (if ,number nil :string))))
       (if (not id)
           (message "Unable to find item id.")
         ,body))))

(defmacro lem-ui-with-own-item (item-type &rest body)
  "Call BODY if ITEM-TYPE is at point and owned by the current user."
  (declare (debug t)
           (indent 1))
  `(cond ((not (eq ,item-type (lem-ui--property 'lem-type)))
          (user-error "No %s at point?" ,item-type))
         ((not (equal lem-user-id (lem-ui--property 'creator-id)))
          (user-error "You can only modify your own items"))
         (t
          ,@body)))

;;; BUFFER DETAILS

(defvar-local lem-ui-buffer-spec nil
  "A plist containing details about the current lem buffer.")

(defun lem-ui-set-buffer-spec (&optional listing-type sort
                                         view-fun item page unread query)
  "Set `lem-ui-buffer-spec' for the current buffer.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
ITEM is a symbol, either posts or comments."
  ;; TODO: allow us to set a single element:
  (setq lem-ui-buffer-spec
        `(:listing-type ,listing-type :sort ,sort :view-fun ,view-fun
                        :item ,item :page ,(or page 1) :unread ,unread
                        :query ,query)))

(defun lem-ui-get-buffer-spec (key)
  "Return value of KEY in `lem-ui-buffer-spec'."
  (plist-get lem-ui-buffer-spec key))

;;; NAV

(defun lem--goto-pos (fun &optional refresh pos)
  "Search for item with FUN.
If search returns nil, execute REFRESH function.
Optionally start from POS."
  (fedi--goto-pos fun 'byline-top refresh pos))

(defun lem-next-item (&optional no-refresh)
  "Move to next item.
NO-REFRESH means don't try to load more items at EOB."
  (interactive)
  (lem--goto-pos #'next-single-property-change
                 (unless no-refresh #'lem-ui-more)))

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
           (lem-ui-view-item-community))
          ((or (eq type 'comment)
               (eq type 'comment-reply))
           (lem-ui-view-comment-post))
          ((eq type 'user)
           (lem-ui-view-item-user)))))

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

(defun lem-ui-view-instance (&optional type sort limit page item sidebar)
  "View posts of current user's home instance.
SORT must be a member of `lem-comment-sort-types' if item is
\"comments\", otherwise it must be a member of `lem-sort-types'.
TYPE must be member of `lem-listing-types'.
ITEM must be a member of `lem-items-types'."
  (interactive)
  (let* ((instance (lem-get-instance))
         (sort (or sort (lem-ui-view-default-sort 'instance)))
         (type (or type lem-default-listing-type))
         (items (if (equal item "comments")
                    (progn
                      (unless (lem-comment-sort-type-p sort)
                        (setq sort (car lem-comment-sort-types)))
                      (lem-get-comments nil nil type sort limit page))
                  (lem-get-posts type sort limit page)))
         (items (if (equal item "comments")
                    (alist-get 'comments items)
                  (alist-get 'posts items)))
         (buf "*lem-instance*")
         (bindings (lem-ui-view-options 'instance)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (lem-ui-render-instance instance :stats sidebar)
      (lem-ui-set-buffer-spec
       type sort #'lem-ui-view-instance (or item "posts") page)
      (lem-ui-widgets-create `("Listing" ,type "Sort" ,sort))
      (lem-ui-insert-heading (if (eq nil item) "posts" item))
      (if (equal item "comments")
          (lem-ui-render-comments items :details)
        (lem-ui-render-posts-instance items))
      (lem-ui--init-view))))

(defun lem-ui-view-instance-full ()
  "View full instance details."
  ;; TODO: full instance info: sidebar, full desc,
  ;; trending communities, stats, admins
  (interactive)
  (lem-ui-view-instance nil nil nil nil nil :sidebar))

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
                               nil ; no URL so follow-link doesn't do lookup
                               'lem-ui-user-face
                               (cl-third x)))
    list " | ")))

(defun lem-ui-render-instance (instance &optional stats sidebar)
  "INSTANCE.
STATS.
SIDEBAR."
  (let* ((admins-list (alist-get 'admins instance))
         (admins (lem-ui--names-list admins-list 'person))
         (inst (alist-get 'site_view instance)))
    (let-alist inst
      (let ((created (fedi--relative-time-description
                      (date-to-time .site.published))))
        (insert
         (propertize
          (concat
           (propertize .site.name
                       'face '(:weight bold))
           " | "
           (lem-ui-font-lock-comment .site.actor_id)
           (lem-ui-font-lock-comment " created: " created);.site.published)
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
          'lem-type 'instance))))
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

(defun lem-ui-block-item-instance ()
  "Block instance of item at point.
Blocking an instance means you wont see content from that
instance, but will still see content from its users if they are
active on other instances."
  (interactive)
  (lem-ui-with-item 'all
    (let-alist (lem-ui--property 'json)
      (let ((instance (url-host (url-generic-parse-url .post.ap_id))))
        (when (y-or-n-p (format "Block instance %s?" instance))
          (lem-ui-response-msg
           (lem-block-instance .community.instance_id t)
           'blocked t
           (format "Instance %s blocked!" instance)))))
    :number))

(defun lem-ui-unblock-instance ()
  "Prompt for a blocked instance and unblock it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-blocked-instances
   #'lem-ui--instances-list
   "Unblock instance:"
   (lambda (id choice)
     (lem-ui-response-msg
      (lem-block-instance id :json-false)
      'blocked :json-false
      (format "Instance %s unblocked!" choice)))))

;;; CYCLE SORT, LISTING, and ITEMS TYPE

(defun lem-ui-view-default-sort (&optional view)
  "Return the default sort type for the current view.
Returns the car of `lem-user-view-sort-types',
`lem-comment-sort-types' or `lem-sort-types'.
Optionally return default sort type for VIEW."
  (let ((view (or view (lem-ui-view-type))))
    (cond ((or (eq view 'user)
               (eq view 'current-user))
           (car lem-user-view-sort-types)) ;"New"
          ((eq view 'post)
           (car lem-comment-sort-types)) ; "Hot"
          ((eq view 'communities) ; browse communities
           "TopMonth")
          ((eq view 'inbox)
           (car lem-inbox-sort-types)) ; "New"
          (t
           ;; Roll our own comments preference here, the webUI is
           ;; contradictory:
           (let ((item (lem-ui-get-buffer-spec :item)))
             (if (equal item "posts")
                 (car lem-sort-types) ; "Active"
               (car lem-comment-sort-types))))))) ; "Hot"

(defun lem-ui-view-type ()
  "Return the current view, based on `lem-ui-buffer-spec'."
  ;; TODO: minor ones
  (let ((view-fun (lem-ui-get-buffer-spec :view-fun)))
    (cond ((eq view-fun 'lem-ui-view-post)
           'post)
          ((eq view-fun 'lem-ui-view-community)
           'community)
          ((or (eq view-fun 'lem-ui-view-communities)
               (eq view-fun 'lem-ui-browse-communities))
           'communities)
          ((eq view-fun 'lem-ui-view-user)
           'user)
          ((eq view-fun 'lem-ui-view-own-profile)
           'current-user)
          ((eq view-fun 'lem-ui-view-instance)
           'instance)
          ((eq view-fun 'lem-ui-search)
           'search)
          ((eq view-fun 'lem-ui-view-saved-items)
           'saved-items)
          ((eq view-fun 'lem-ui-view-inbox)
           'inbox))))

(defun lem-ui-view-options (view)
  "Return the various sorting and other options for VIEW.
Returns a list of the variables containing the specific options."
  (cond ((eq view 'post)
         '(lem-comment-sort-types))
        ((eq view 'instance)
         '(lem-items-types lem-sort-types lem-listing-types))
        ((eq view 'search)
         '(lem-listing-types lem-sort-types lem-search-types))
        ((or (eq view 'user)
             (eq view 'current-user))
         '(lem-user-items-types lem-sort-types))
        ((eq view 'community)
         '(lem-items-types lem-sort-types))
        ((eq view 'communities)
         '(lem-listing-types lem-sort-types))
        ((eq view 'inbox)
         '(lem-comment-sort-types lem-inbox-types))))

(defun lem-ui-toggle-posts-comments ()
  "Switch between displaying posts or comments.
Works on instance, community, and user views, which also have an overview."
  (interactive)
  (let* ((item (lem-ui-get-buffer-spec :item))
         (view (lem-ui-view-type))
         (sort-last (lem-ui-get-buffer-spec :sort))
         (sort-types (if (equal item "posts")
                         lem-comment-sort-types
                       lem-sort-types))
         ;; sort value must be valid for the item we toggle to:
         (sort (if (equal item "comments")
                   nil ; overview
                 (if (member sort-last sort-types)
                     sort-last
                   (car sort-types))))
         (type (lem-ui-get-buffer-spec :listing-type))
         (id (lem-ui-get-view-id))
         (item-types (if (or (eq view 'user)
                             (eq view 'current-user))
                         lem-user-items-types
                       lem-items-types))
         (item-next (lem-ui-next-type item item-types)))
    (cond ((eq view 'community)
           (lem-ui-view-community id item-next sort)
           (message "Viewing: %s" item-next))
          ((or (eq view 'user)
               (eq view 'current-user))
           (lem-ui-view-user id item-next sort)
           (message "Viewing: %s" item-next))
          ((eq view 'instance)
           (lem-ui-view-instance type sort nil nil item-next)
           (message "Viewing: %s" item-next))
          (t
           (user-error "Posts/Comments toggle not available in this view")))))

(defun lem-ui-get-view-id ()
  "Get id of the view item, a post or user."
  (save-excursion
    (goto-char (point-min))
    (lem-ui--id-from-prop :string)))

(defun lem-ui-next-listing-type (type)
  "Return next listing type after TYPE in `lem-listing-types'."
  (lem-ui-next-type type lem-listing-types))

(defun lem-ui-next-type (type list)
  "Return next listing type after TYPE in LIST."
  (if (or (equal type (car (last list)))
          (null type))
      (car list)
    (cadr (member type list))))

(defun lem-ui-cycle-listing-type (&optional type)
  "Cycle view between `lem-listing-types'.
Works in instance and search views.
If TYPE is given, load that listing-type.
It must be a member of the same list."
  (interactive)
  (let* ((type-last (lem-ui-get-buffer-spec :listing-type))
         (sort (lem-ui-get-buffer-spec :sort))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         (view (lem-ui-view-type))
         (item (lem-ui-get-buffer-spec :item))
         (query (lem-ui-get-buffer-spec :query))
         (listing-type (or type (lem-ui-next-listing-type type-last))))
    (cond ((or (eq view 'user)
               (eq view 'current-user)
               (eq view 'community)
               (eq view 'post))
           (message "%s views don't have listing type."
                    view))
          ((eq view 'instance)
           (funcall view-fun listing-type sort nil nil item))
          ;; (message "listing: %s" listing-type))
          ((eq view 'communities)
           (lem-ui-browse-communities listing-type sort))
          ;; (message "listing: %s" listing-type))
          ((eq view 'inbox)
           (lem-ui-cycle-inbox))
          ((eq view 'search)
           (lem-ui-search query item listing-type sort))
          (t ;; TODO: search / communities
           (message "Not implemented yet")))))

(defun lem-ui-choose-listing-type ()
  "Prompt for a listing type, and use it to reload current view."
  (interactive)
  (let ((choice (completing-read "Listing type:"
                                 lem-listing-types nil :match)))
    (lem-ui-cycle-listing-type choice)))

(defun lem-ui-get-sort-types (view item)
  "Return sort type list according to VIEW.
Return either `lem-comment-sort-types',
`lem-user-view-sort-types' or `lem-sort-types'.
If VIEW is `eq' to post, or ITEM to \"comments\", return the
former. IF VIEW is `eq' user, return the second."
  (cond ((or (eq view 'post)
             (equal item "comments"))
         lem-comment-sort-types)
        ((or (eq view 'user)
             (eq view 'current-user))
         lem-user-view-sort-types)
        ((eq view 'inbox)
         lem-inbox-sort-types)
        (t
         lem-sort-types)))

(defun lem-ui-cycle-sort (&optional sort)
  "Cycle view between some `lem-sort-types'.
For post view or other comments view, use
`lem-comment-sort-types'.
Optionally, use SORT."
  (interactive)
  (let* ((type (lem-ui-get-buffer-spec :listing-type))
         (sort-last (lem-ui-get-buffer-spec :sort))
         (view (lem-ui-view-type))
         (item (lem-ui-get-buffer-spec :item))
         (id (lem-ui-get-view-id))
         (query (lem-ui-get-buffer-spec :query))
         (sort-types (unless sort
                       (lem-ui-get-sort-types view item)))
         (sort-next (or sort
                        (lem-ui-next-type sort-last sort-types))))
    (cond ((or (eq view 'user)
               (eq view 'current-user))
           (if (equal item "overview")
               (user-error "Not implemented for overview")
             (lem-ui-view-user id item sort-next)))
          ((eq view 'community)
           (lem-ui-view-community id item sort-next))
          ((eq view 'post)
           (lem-ui-view-post id sort-next))
          ((eq view 'instance)
           (lem-ui-view-instance type sort-next nil nil item))
          ((eq view 'saved-items)
           (lem-ui-view-saved-items nil sort-next))
          ((eq view 'communities)
           (lem-ui-browse-communities type sort-next))
          ((eq view 'search)
           (lem-ui-search query item type sort-next))
          ((eq view 'inbox)
           (if (or (eq item 'all)
                   (eq item 'private-messages))
               (user-error "Sort not available for this inbox item")
             (lem-ui-view-inbox item sort-next)))
          (t
           ;; TODO: search
           (user-error "Not implemented yet")))))

(defun lem-ui-choose-sort ()
  "Prompt for a sort type, and use it to reload the current view."
  (interactive)
  (let* ((view (lem-ui-view-type))
         (item (lem-ui-get-buffer-spec :item))
         (sort-list (lem-ui-get-sort-types view item))
         (choice (completing-read "Sort by:" sort-list nil :match)))
    (lem-ui-cycle-sort choice)))

;;; SEARCH

(defun lem-ui-read-type (prompt types-list)
  "Read a choice from TYPES-LIST using PROMPT."
  (completing-read prompt
                   types-list nil :match))

(defun lem-ui-choose-search-type ()
  "Choose a search type from `lem-search-types' and repeat current query."
  (interactive)
  (if (not (eq (lem-ui-view-type) 'search))
      (user-error "You can only choose search type in a search")
    (let* ((types (remove "Url"
                          (remove "All" lem-search-types)))
           (choice (lem-ui-read-type "Search type:" types))
           (sort (lem-ui-get-buffer-spec :sort))
           (query (lem-ui-get-buffer-spec :query))
           (listing-type (lem-ui-get-buffer-spec :listing-type)))
      (lem-ui-search query choice listing-type sort))))

(defun lem-ui-search-type-fun (search-type)
  "Given SEARCH-TYPE, return a render function."
  (intern (concat "lem-ui-render-" search-type)))

(defun lem-ui-search (&optional query search-type
                                listing-type sort limit page
                                community-id creator-id)
  "Search for QUERY, of SEARCH-TYPE, one of the types in `lem-search-types'.
LISTING-TYPE is one of `lem-listing-types'.
SORT is one of `lem-sort-types'.
LIMIT is the max results to return.
PAGE is the page number.
COMMUNITY-ID is the ID of a community to limit search to.
CREATOR-ID is same to limit search to a user."
  (interactive)
  (let* ((types ; remove not-yet-implemented search types:
          (remove "Url"
                  (remove "All" lem-search-types)))
         (type (downcase
                (or search-type
                    (lem-ui-read-type "Search type: " types))))
         ;; LISTING/SORT doesn't make sense for all search types, eg users,
         ;; lets just cycle in results:
         ;; (listing-type (lem-ui-read-type "Listing type: " lem-listing-types))
         ;; (sort (lem-ui-read-type "Sort by: " lem-sort-types))
         (query (or query (read-string "Query: ")))
         (type-fun (lem-ui-search-type-fun type))
         (buf (format "*lem-search-%s*" type))
         ;; TODO: handle community
         (response (lem-search query (capitalize type) listing-type sort
                               (or limit lem-ui-comments-limit)
                               page nil
                               community-id creator-id))
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
      (cond ((or (equal type "posts")
                 (equal type "comments"))
             (funcall type-fun data t))
            ((equal type "users")
             (funcall type-fun data :search))
            (t
             (funcall type-fun data)))
      (lem-ui-set-buffer-spec listing-type sort
                              #'lem-ui-search
                              type page nil query))))

(defun lem-ui-search-in-community ()
  "Search in the current community."
  (interactive)
  (if (not (eq (lem-ui-view-type) 'community))
      (user-error "Not in a community view")
    (let ((id (save-excursion
                (goto-char (point-min))
                (lem-ui--property 'id)))
          (type (lem-ui-read-type "Search type: " '("posts" "comments"))))
      (lem-ui-search nil type nil nil nil nil id))))

(defun lem-ui-search-in-user ()
  "Search in the user currently viewed."
  (interactive)
  (if (not (or (eq (lem-ui-view-type) 'user)
               (eq (lem-ui-view-type) 'current-user)))
      (user-error "Not in a user view")
    (let ((id (save-excursion
                (goto-char (point-min))
                (lem-ui--property 'id)))
          (type (lem-ui-read-type "Search type: " '("posts" "comments"))))
      (lem-ui-search nil type nil nil nil nil nil id))))

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
          ;; (string-match "^/[[:alpha:]]+$" query) ; unsinn! this matches https://example.com/example
          (string-match "^/u/[_[:alpha:]]+$" query)
          (string-match "^/c/[@._[:alnum:]]+$" query)
          (string-match "^/post/[[:digit:]]+$" query)
          (string-match "^/comment/[[:digit:]]+$" query)))))

(defun lem-ui-url-lookup (&optional url)
  "Perform a webfinger lookup on URL and load the result in `lem.el'.
Or url at point, or text prop shr-url, or read a URL in the minibuffer.
Lemmy supports lookups for users, posts, comments and communities."
  (interactive)
  (let ((query (or ; is this right? search fails if url wrongly contains
                ;; uppercase term:

                ;; we now try to only call this on rendered urls, all else
                ;; should use an api get function, so no downcasing.
                url ; works with "https://lemmy.ml/u/JoeBidet" in mods list
                ;; (when url (downcase url)) ; fails with "https://lemmy.ml/u/JoeBidet" in mods list
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

;;; FEATURE (PIN) POSTS

(defun lem-ui-do-feature (id arg type str)
  "Call `lem-feature-post' and handle the response.
ID, ARG TYPE are for that function.
STR is for message."
  (let ((response (lem-feature-post id arg type))
        (view (lem-ui-get-buffer-spec :view-fun)))
    (lem-ui-response-msg
     response
     'post_view :non-nil
     (format "Post %s!" str))
    (lem-ui--update-item-json (alist-get 'post_view response))
    (lem-ui-update-item-from-json
     'byline-top
     (lambda (json)
       (lem-ui-top-byline-replace
        json
        (unless (eq view 'lem-ui-view-community)
          :community))))))

(defun lem-ui-feature-post (&optional unfeature)
  "Feature (pin) a post, either to its instance or community.
UNFEATURE means we are unfeaturing a post."
  (interactive)
  (lem-ui-with-item 'post
    (let* ((json (lem-ui--property 'json))
           (post (alist-get 'post json))
           (id (lem-ui--property 'id))
           (mod-p (alist-get 'creator_is_moderator json))
           (admin-p (alist-get 'creator_is_admin json))
           (feat-comm (alist-get 'featured_community post))
           (feat-loc (alist-get 'featured_local post))
           ;; TODO: annotate Local with "instance":
           (feat-type
            (if unfeature
                (cond ((eq t feat-comm) "Community")
                      ((eq t feat-loc) "Local")
                      (t
                       (user-error "Post not featured?")))
              (completing-read "Feature type: "
                               '("Local" "Community"))))
           (feat-arg (if unfeature :json-false t))
           (feat-str (if unfeature "unfeatured" "featured")))
      (if (equal feat-type "Community")
          ;; TODO: refactor conds:
          (cond (unfeature
                 (lem-ui-do-feature id feat-arg feat-type feat-str))
                ((not (eq t mod-p))
                 (user-error "You need to be a mod to feature to community"))
                ((eq t feat-comm)
                 (user-error "Post already featured?"))
                (t
                 (lem-ui-do-feature id feat-arg feat-type feat-str)))
        (cond (unfeature
               (lem-ui-do-feature id feat-arg feat-type feat-str))
              ((not (eq t admin-p))
               (user-error "You need to be an admin to feature to instance"))
              ((eq t feat-loc)
               (user-error "Post already featured?"))
              (t
               (lem-ui-do-feature id feat-arg feat-type feat-str)))))
    :number))

(defun lem-ui-unfeature-post ()
  "Unfeature (unpin) post at point."
  (interactive)
  (lem-ui-feature-post :unfeature))

;;; LINKS

(defvar lem-ui-link-map
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
  ;; If a link is a part of a larger element, "id" prop will likely be for the
  ;; parent element, as the propertizing of the parent will override any
  ;; propertizing of the link. so the link needs an element-id prop, e.g.
  ;; community-id

  ;; if a link is shr-rendered (in a body somewhere) it requires url-lookup,
  ;; and the id won't work for other functions

  ;; perhaps we just have to check for shr-url, and be careful about when we
  ;; set it ourselves, ie only do so if we can't load the view by other means
  (interactive)
  (let ((id (lem-ui--id-from-prop :string 'id))
        (creator-id (lem-ui--id-from-prop :string 'creator-id))
        (community-id (lem-ui--id-from-prop :string 'community-id))
        (item-type (lem-ui--property 'lem-tab-stop))
        (shr-url (lem-ui--property 'shr-url)))
    (cond (shr-url ; shr-url: url-lookup (for rendered links)
           (if (string-prefix-p "/c/" shr-url) ; community relative link
               (lem-get-community (substring-no-properties shr-url 3))
             (lem-ui-url-lookup shr-url)))
          ((eq item-type 'community)
           (lem-ui-view-community community-id)) ; in bylines, etc.
          ((and (eq item-type 'user)
                creator-id)
           (lem-ui-view-user creator-id "overview"))
          ;; admin display in instance header:
          ;; (type user, but id not creator-id)
          ((eq item-type 'user)
           (lem-ui-view-user id "overview"))
          ((and (or (eq (lem-ui--property 'lem-type) 'comment)
                    (eq (lem-ui--property 'lem-type) 'comment-reply))
                (lem-ui--property 'title)) ; detailed comment
           (lem-ui-view-comment-post (lem-ui--property 'post-id)))
          ((and (eq (lem-ui--property 'lem-type) 'post)
                (lem-ui--property 'title))
           (lem-ui-view-post-at-point)))))

(defun lem-ui--propertize-link (item id type &optional url face help-echo community-id)
  "Propertize a link ITEM with ID and TYPE.
Optionally provide URL for shr-url.
FACE is a face to use.
HELP-ECHO is a help-echo string.
COMMUNITY-ID is a community id."
  ;; FIXME: we shouldn't ghost shr rendering just to have buttons, we need to
  ;; distinguish shr categories/shr-urls from our own links
  ;; that way we have no follow-link-at-point problems:
  ;; rendered shr-urls use url-lookup, others use our get functions.
  (propertize item
              'shr-url url
              'keymap lem-ui-link-map
              'button t
              'category 'shr
              'follow-link t
              'mouse-face 'highlight
              'id id
              'community-id community-id ; for mods links
              'lem-tab-stop type
              'face face
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
         (keymap lem-ui-link-map)
         (lem-tab-stop-type 'shr-url))
    (add-text-properties start end
                         (append
                          (list 'lem-tab-stop lem-tab-stop-type
                                'keymap keymap
                                'help-echo help-echo)))))

;;; BYLINES

(defun lem-ui-propertize-box (str color obj)
  "Propertize STR with COLOR, box, `font-lock-keyword-face' and OBJ help-echo."
  (propertize str
              'face `(:foreground ,color :box t)
              'help-echo obj))

(defun lem-ui-propertize-admin-box ()
  "Return a propertized admin box."
  (lem-ui-propertize-box "A" "red3" "instance admin"))

(defun lem-ui-propertize-title (title-str)
  "Propertize TITLE-STR as a post title."
  (propertize title-str
              'mouse-face 'highlight
              'cursor-face '(:inherit highlight :extend t)
              'title t
              'keymap lem-ui-link-map
              'face '(:weight bold)))

(defun lem-ui--format-community-as-link (community id url)
  "Format COMMUNITY, a string, as a link using URL.
ID is a community-id."
  (lem-ui--propertize-link community nil 'community
                           nil ; no shr-url if not rendered!
                           'lem-ui-community-face
                           url id))

(defun lem-ui-top-byline (title url username _score timestamp
                                &optional community community-url
                                featured-p op-p admin-p mod-p del-p handle
                                post-title edited)
  "Format a top byline with TITLE, URL, USERNAME, SCORE and TIMESTAMP.
COMMUNITY and COMMUNITY-URL are those of the community the item
belongs to.
FEATURED-P means the item is pinned.
OP-P is a flag, meaning we add a boxed OP string to the byline.
ADMIN-P means we add same for admins, MOD-P means add same for moderators.
DEL-P means add icon for deleted item.
HANDLE is a user handle as a string.
POST-TITLE is the name of the parent post, used for detailed
comment display.
EDITED is a timestamp."
  (let ((url (ignore-errors (lem-ui-render-url url)))
        (parsed-time (date-to-time timestamp))
        (edited-parsed (when edited (date-to-time edited))))
    (propertize
     (concat
      (if title
          ;; TODO: preserve shr-props and add bold?
          (lem-ui-propertize-title
           (lem-ui-render-body title))
        "")
      (if url
          (concat url "\n")
        "")
      ;; username:
      (lem-ui--propertize-link username nil 'user
                               nil 'lem-ui-user-face handle)
      ;; boxes:
      (when op-p
        (concat " "
                (lem-ui-propertize-box "OP" "green3" "original poster")))
      (when admin-p
        (concat " "
                (lem-ui-propertize-admin-box)))
      (when mod-p
        (concat " "
                (lem-ui-propertize-box "M" "blue3" "community moderator")))
      (when del-p
        (concat " "
                (lem-ui-symbol 'deleted)))
      ;; community
      (when community
        (concat
         (propertize " to "
                     'face font-lock-comment-face)
         (lem-ui--format-community-as-link community nil ; comm-id added to post
                                           community-url)))
      ;; timestamp:
      (concat
       " | "
       (propertize
        timestamp
        'timestamp parsed-time
        'display (if lem-enable-relative-timestamps
                     (fedi--relative-time-description parsed-time)
                   parsed-time))
       (propertize
        (concat
         (if edited
             (concat " " (lem-ui-symbol 'edited)
                     " "
                     (propertize
                      edited
                      'timestamp edited-parsed
                      'display
                      (if lem-enable-relative-timestamps
                          (fedi--relative-time-description edited-parsed)
                        edited-parsed)))
           "")
         (if (eq featured-p t)
             (concat " | "
                     (lem-ui-symbol 'pinned))
           ""))
        'face font-lock-comment-face))
      ;; post title:
      (when post-title
        (concat "\n"
                (lem-ui-propertize-title post-title))))
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

(defun lem-ui-bt-byline (score comments &optional my-vote saved prefix)
  "Format a bottom byline for an item.
SCORE is the item's score.
COMMENTS is the comments count to render.
MY-VOTE is a number, the current vote by the current user.
SAVED means to add saved icon.
PREFIX is a \"line-prefix\" property to add."
  (let* ((my-score (lem-ui-prop-score my-vote score))
         (str (concat (lem-ui-symbol 'upvote) " "
                      my-score " | "
                      (lem-ui-symbol 'reply) " "
                      (number-to-string comments)
                      (when (eq saved t)
                        (concat " | "
                                (lem-ui-symbol 'bookmark))))))
    (propertize str
                'byline-bottom t
                'line-prefix prefix)))

;;; UPDATING ITEMS

;; currently if we need to update something after an action we update that
;; item's json, then update the item or part thereof from the updated json.
;; because we always update the json prop for the whole item, but then
;; sometimes only update a part of the item, e.g. bylines.

(defalias 'lem-ui-update-item-from-json 'fedi-update-item-from-json)

(defalias 'lem-ui--replace-region-contents 'fedi--replace-region-contents)

(defalias 'lem-ui--update-item-json 'fedi--update-item-json)

(defun lem-ui-bt-byline-replace (json &optional vote saved prefix)
  "Call `lem-ui-bt-byline' to update the bottom byline.
JSON is the item's json.
VOTE, SAVED, and PREFIX are arguments for `lem-ui-bt-byline'."
  ;; FIXME: this assumes post object
  (let-alist json
    (let ((vote (or vote .my_vote))
          (saved (or saved .saved))
          (prefix (or prefix (lem-ui--property 'line-prefix))))
      (propertize
       (lem-ui-bt-byline .counts.score
                         (or .counts.child_count
                             .counts.comments)
                         vote saved prefix)
       ;; properties from render-post/comment (need to be checked):
       'json json
       'id (or .post.id .comment.id)
       'community-id .post.community_id
       'creator-id .creator.id
       'lem-type (caar json)))))

(defun lem-ui-top-byline-replace (json &optional community)
  "Call `lem-ui-top-byline' and add post properties to it.
JSON is the data to use.
COMMUNITY means display the community posted to."
  (let-alist json
    (propertize
     (lem-ui-top-byline .post.name
                        (or .post.url "")
                        (or .creator.display_name .creator.name)
                        .counts.score
                        .post.published
                        (when community .community.name)
                        (when community .community.actor_id)
                        (or (eq t .post.featured_community) ; pinned community
                            (eq t .post.featured_local)) ; pinned instance
                        nil
                        (eq t .creator_is_admin)
                        (or (eq t .creator_is_moderator)
                            (cl-member .creator.id lem-ui-post-community-mods-ids))
                        (eq t .post.deleted)
                        (lem-ui--handle-from-user-url .creator.actor_id))
     ;; add render-post props:
     'json json
     'id .post.id
     'community-id .post.community_id
     'creator-id .creator.id
     'lem-type (caar json))))

(defun lem-ui-update-parent-item-maybe ()
  "Go to buffer's first element, and reload its json data and bottom byline."
  ;; FIXME: only running in post views till we improve things.
  (when (eq (lem-ui-view-type) 'post)
    (save-restriction
      (save-excursion
        (widen)
        (goto-char (point-min))
        (forward-char)
        ;; FIXME: we have user item type, but "person_view",
        ;; so this isn't working for users
        (let* ((item-type (lem-ui--property 'lem-type))
               (id (lem-ui--property 'id))
               (item-fun (lem-ui-make-fun "lem-get-" item-type))
               (item-data (funcall item-fun id))
               (item (alist-get (intern
                                 (concat (symbol-name item-type)
                                         "_view"))
                                item-data)))
          ;; for now, just update parent posts:
          ;; as lem-ui-bt-byline-replace wrongly assumes posts
          (when (eq (lem-ui-view-type) 'post)
            (lem-ui--update-item-json item)
            (lem-ui-update-item-from-json
             'byline-bottom
             (lambda (json)
               (lem-ui-bt-byline-replace json)))))))))

(defun lem-ui-reload-view ()
  "Reload the current view."
  (interactive)
  (let ((type (lem-ui-view-type))
        (item (lem-ui-get-buffer-spec :item))
        (sort (lem-ui-get-buffer-spec :sort))
        (listing (lem-ui-get-buffer-spec :listing-type))
        (page (lem-ui-get-buffer-spec :page))
        (limit (lem-ui-get-buffer-spec :limit))
        (id (save-excursion
              (goto-char (point-min))
              (lem-ui--property 'id))))
    (cond ((eq type 'post)
           (lem-ui-view-post id sort limit))
          ((eq type 'instance)
           (lem-ui-view-instance listing sort limit page item))
          ((eq type 'community)
           (lem-ui-view-community id item sort limit page))
          ((or (eq type 'user)
               (eq type 'current-user))
           (lem-ui-view-user id item sort limit))
          ((eq type 'inbox)
           (lem-ui-view-inbox item sort))
          (t
           (user-error "Unable to reload view type %s" type)))))

;; MARKDOWN BODY RENDERING

(defun lem-ui-render-url (url &optional no-shorten)
  "Render URL, a plain non-html string.
Used for post URLs.
NO-SHORTEN means display full URL, else only the domain is shown.
Adds lem-tab-stop and `lem-ui-link-map' to rendered urls."
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
        (setq rendered
              (concat
               (propertize (buffer-string)
                           'lem-tab-stop 'url
                           'keymap lem-ui-link-map)
               " "))
        (kill-buffer-and-window))
      rendered)))

(defun lem-ui-mdize-plain-urls ()
  "Markdown-ize any plain string URLs found in current buffer."
  ;; FIXME: this doesn't rly work with ```verbatim``` in md
  ;; NB: this must not break any md, otherwise `markdown-standalone' may
  ;; hang!
  (while (re-search-forward lem-ui-url-regex nil :no-error)
    (unless
        (save-excursion
          (goto-char (1- (point)))
          (or (markdown-inside-link-p)
              ;; bbcode (seen in spam, breaks markdown if url replaced):
              (let ((regex (concat "\\[url=" markdown-regex-uri "\\/\\]"
                                   ".*" ; description
                                   "\\[\\/url\\]")))
                (thing-at-point-looking-at regex))))
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

(defun lem-ui--escape-@s (buffer)
  "Escape @ symbols in BUFFER."
  (with-current-buffer buffer
    (switch-to-buffer (current-buffer))
    (while (re-search-forward "@" nil :noerror)
      ;; FIXME: still wrong
      ;; e.g. with something we don't need to fix:
      ;; https://lemmy.ml/post/10890295
      ;; e.g. of something we DO need to fix?
      (when (and (not (markdown-link-p))
                 (not (thing-at-point 'url)))
        (ignore)
        ;; (replace-match "\\\\@")
        ))))

(defun lem-ui-render-body (body &optional json indent)
  "Render item BODY as markdowned html.
JSON is the item's data to process the link with.
INDENT is a number, the level of indent for the item."
  (let ((buf "*lem-md*")
        str)
    ;; 1: temp buffer, prepare for md
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (lem-ui-mdize-plain-urls)
      (goto-char (point-min))
      (lem-ui--escape-@s (current-buffer))
      ;; 2: md-ize or fallback
      (let ((old-buf (buffer-string)))
        (condition-case nil
            (markdown-standalone buf)
          (t ; if rendering fails, return unrendered body:
           (with-current-buffer buf
             (erase-buffer)
             (insert old-buf)))))
      ;; 3: shr-render the md
      (with-current-buffer buf
        (let ((shr-width (when indent
                           (- (window-width) (+ 1 indent))))
              (shr-discard-aria-hidden t)) ; for pandoc md image output
          ;; shr render:
          (shr-render-buffer (current-buffer))))
      ;; 4 our render shr urls + collect result
      (with-current-buffer "*html*"
        ;; our render:
        (when json
          (lem-ui-render-shr-url))
        (re-search-forward "\n\n" nil :no-error)
        (setq str (buffer-substring (point) (point-max)))
        (kill-buffer-and-window)        ; shr's *html*
        (kill-buffer buf)))             ; our md
    (setq str (lem-ui-propertize-items str json 'handle))
    (setq str (lem-ui-propertize-items str json 'community))
    (setq str (lem-ui-propertize-items str json 'url))
    str))

(defun lem-ui-tabstop-link-by-regex (regex)
  "Add lem-tab-stop property to link matching REGEX."
  (while (re-search-forward regex nil :no-error)
    ;; (let ((item (buffer-substring-no-properties (match-beginning 2)
    ;; (match-end 2))))
    (add-text-properties (match-beginning 2)
                         (match-end 2)
                         `( lem-tab-stop url
                            keymap ,lem-ui-link-map))))

(defun lem-ui-propertize-items (str json type)
  "Propertize any items of TYPE in STR as links using JSON.
Type is a symbol, either handle or community.
Communities are of the form \"!community@instance.com.\""
  (let ((regex (cond ((eq type 'community)
                      lem-ui-community-regex)
                     ((eq type 'handle)
                      lem-ui-handle-regex)
                     ((eq type 'url)
                      lem-ui-url-regex))))
    (with-temp-buffer
      ;; (switch-to-buffer (current-buffer))
      (insert str)
      (goto-char (point-min))
      (save-match-data
        ;; ideally we'd work errors out, but we don't want to ruin
        ;; our caller, which might make a page load fail:
        (ignore-errors
          (if (eq type 'url)
              (lem-ui-tabstop-link-by-regex regex)
            (while (re-search-forward regex nil :no-error)
              (let* ((item (buffer-substring-no-properties (match-beginning 2)
                                                           (match-end 2)))
                     (beg (match-beginning 1))
                     (end (match-end 1))
                     (domain (if (match-beginning 3)
                                 (buffer-substring-no-properties (match-beginning 3)
                                                                 (match-end 3))))
                     (ap-link (url-generic-parse-url (alist-get 'ap_id json)))
                     (instance (or domain (url-domain ap-link)))
                     (link (concat "https://" instance
                                   (if (eq type 'community) "/c/" "/u/")
                                   item)))
                (add-text-properties beg
                                     end
                                     `(face '(shr-text shr-link)
                                            lem-tab-stop ,type
                                            mouse-face highlight
                                            shr-tabstop t
                                            shr-url ,link
                                            button t
                                            category shr
                                            follow-link t
                                            help-echo ,link
                                            keymap ,lem-ui-link-map))))))
        (buffer-string)))))

(defun lem-ui-mods-ids (mods)
  "Return a list of the ids of MODS."
  (cl-loop for mod in mods
           collect (alist-get 'id
                              (alist-get 'moderator mod))))

(defun lem-ui--set-mods (community-id)
  "Set `lem-ui-post-community-mods-ids'.
The variable contains the list of community moderator ids for the
community of the current post, with COMMUNITY-ID."
  (let* ((community-json (lem-get-community community-id))
         (mods (alist-get 'moderators community-json))
         (mods-ids (lem-ui-mods-ids mods)))
    (setq lem-ui-post-community-mods-ids mods-ids)))

;;; POSTS

(defun lem-ui-view-post-at-point ()
  "View post at point."
  (interactive)
  (lem-ui-with-item 'post
    (lem-ui-view-post id)))

(defun lem-ui-view-post (id &optional sort limit)
  "View post with ID.
SORT must be a member of `lem-comment-sort-types.'
LIMIT."
  (let ((post-view (lem-get-post id)))
    (if (stringp post-view)
        (user-error "%s" post-view)
      (let* ((post (alist-get 'post_view post-view))
             (community-id (alist-get 'community_id
                                      (alist-get 'post post)))
             (sort (or sort (lem-ui-view-default-sort 'post)))
             (bindings (lem-ui-view-options 'post))
             (buf (format "*lem-post-%s*" id)))
        (lem-ui-with-buffer buf 'lem-mode nil bindings
          (lem-ui--set-mods community-id)
          (lem-ui-render-post post :community)
          (lem-ui-set-buffer-spec nil sort #'lem-ui-view-post 'post) ;limit
          (lem-ui-widgets-create `("Sort" ,sort))
          (lem-ui-render-post-comments id sort limit)
          (lem-ui--init-view))))))

(defun lem-ui-featured-p (post)
  "Return t if POST, which is data, is featured in the current view.
Posts can be featured either for instance or community."
  (let-alist post
    ;; (let ((view (lem-ui-view-type))) ; buffer-spec not set yet
    (if (string-suffix-p "instance*" (buffer-name))
        (eq t .post.featured_local) ; pinned instance
      (eq t .post.featured_community)))) ; pinned community

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
           (admin-p (eq t .creator_is_admin))
           (mod-p (or (eq t .creator_is_moderator)
                      (cl-member .creator.id lem-ui-post-community-mods-ids)))
           (del-p (eq t .post.deleted)))
      (insert
       (propertize
        (concat
         (lem-ui-top-byline .post.name
                            .post.url
                            (or .creator.display_name .creator.name)
                            .counts.score
                            .post.published
                            (when community (or .community.title
                                                .community.name))
                            (when community .community.actor_id)
                            (lem-ui-featured-p post)
                            nil admin-p mod-p del-p handle nil .post.updated)
         "\n"
         (if .post.body
             (if trim
                 (let ((width (* 5 (window-width))))
                   (truncate-string-to-width body width nil nil "..."))
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
    (when .post.url
      (let* ((parsed (url-generic-parse-url .post.url))
             (filename (url-filename parsed))
             (ext (car (last (split-string filename "\\.")))))
        (if (member ext lem-ui-image-formats)
            (let ((html (concat "<img src=\"" .post.url "\" alt=\"*\" />"))
                  (shr-discard-aria-hidden t) ; for pandoc md image output
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

;;; SAVING

(defun lem-ui-save-item (&optional unsave)
  "Save item at point.
Saved items can be viewed in your profile, like bookmarks.
If UNSAVE, unsave the item instead."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((type (lem-ui--item-type))
           (s-str (if unsave "unsaved" "saved"))
           (s-bool (if unsave :json-false t))
           (json (lem-ui--property 'json))
           (saved-p (alist-get 'saved json)))
      (cond ((and unsave (eq saved-p :json-false))
             (message "You can only unsave saved items."))
            ((eq type 'post)
             (let ((json (lem-save-post id s-bool))
                   (my-vote (alist-get 'my_vote json)))
               (lem-ui-response-msg json
                                    'post_view :non-nil
                                    (format "%s %s %s!" type id s-str))
               (lem-ui--update-item-json (alist-get 'post_view json))
               (lem-ui-update-item-from-json
                'byline-bottom
                (lambda (json)
                  (lem-ui-bt-byline-replace json my-vote s-bool)))))
            ((eq type 'comment)
             (let ((json (lem-save-comment id s-bool))
                   (my-vote (alist-get 'my_vote json)))
               (lem-ui-response-msg json
                                    'comment_view :non-nil
                                    (format "%s %s %s!" type id s-str))
               (lem-ui--update-item-json (alist-get 'comment_view json))
               (lem-ui-update-item-from-json
                'byline-bottom
                (lambda (json)
                  (lem-ui-bt-byline-replace json my-vote s-bool)))))
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
      (lem-ui-render-comments comments :details)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil (or sort "Active")
                              #'lem-ui-view-saved-items))))

;;; COMPLETION FOR ACTIONS

(defalias 'lem-ui-do-item-completing 'fedi-do-item-completing)

(defun lem-ui--communities-list (communities)
  "Return a list of name/description and ID from COMMUNITIES."
  (cl-loop for item in communities
           collect (let-alist item
                     (list
                      (lem-ui-handle-from-url .community.actor_id "!")
                      .community.id
                      .community.actor_id))))

(defun lem-ui--users-list (users)
  "For user in USERS, return name, URL, and id."
  (cl-loop for item in users
           collect (let-alist item
                     (list (lem-ui-handle-from-url .actor_id "@")
                           ;; .name
                           ;; .actor_id
                           .id))))

(defun lem-ui--blocks-list (blocks)
  "For user in BLOCKS, return handle, and id."
  (cl-loop for item in blocks
           collect (let-alist (alist-get 'target item)
                     (list (lem-ui-handle-from-url .actor_id "@")
                           .id))))

(defun lem-ui--instances-list (instances)
  "For each item in (blocked) INSTANCES, return domain and id."
  (cl-loop for i in instances
           collect (let-alist (alist-get 'instance i)
                     (list .domain .id))))

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
  "M-<right>" #'vtable-next-column
  "<mouse-2>" 'lem-ui-view-thing-at-point)

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

;; unfuck vtable's case-sensitive sorting:
(defun lem-ui-string> (s1 s2)
  "Case insensitive `string>', which compares S1 and S2."
  (string> (downcase s1) (downcase s2)))

(defun lem-ui-string< (s1 s2)
  "Case insensitive `string<', which compares S1 and S2."
  (string> (downcase s2) (downcase s1)))

(defvar vtable-string-greater #'lem-ui-string>)
(defvar vtable-string-lesser #'lem-ui-string<)

;; TODO: rename?
(defun vtable--sort (table)
  "TABLE."
  (pcase-dolist (`(,index . ,direction) (vtable-sort-by table))
    (let ((cache (vtable--cache table))
          (numerical (vtable-column--numerical
                      (elt (vtable-columns table) index)))
          (numcomp (if (eq direction 'descend)
                       #'> #'<))
          (stringcomp
           (if (eq direction 'descend)
               vtable-string-greater
             vtable-string-lesser)))
      (setcar cache
              (sort (car cache)
                    (lambda (e1 e2)
                      (let ((c1 (elt e1 (1+ index)))
                            (c2 (elt e2 (1+ index))))
                        (if numerical
                            (funcall numcomp (car c1) (car c2))
                          (funcall
                           stringcomp
                           (if (stringp (car c1))
                               (car c1)
                             (format "%s" (car c1)))
                           (if (stringp (car c2))
                               (car c2)
                             (format "%s" (car c2))))))))))))

(define-button-type 'lem-tl-button
  ;; few props work here, so we propertize again below:
  ;; 'follow-link t
  ;; 'category 'shr
  ;; 'face '(:inherit warning :unterline t)
  ;; 'help-echo "View community"
  ;; 'mouse-face 'highlight
  ;; 'action #'lem-ui-view-community-at-point-tl)
  )

(defun lem-ui-return-community-obj (community)
  "Return a vtable object for COMMUNITY."
  (let-alist community
    (cl-loop for i in
             (list
              (propertize .community.title
                          'id .community.id
                          'follow-link t
                          'type 'lem-tl-button
                          'category 'shr
                          'shr-url .community.actor_id
                          ;; interrupted by :row-colors below:
                          'face 'lem-ui-community-face
                          'lem-tab-stop t
                          'mouse-face 'highlight
                          'help-echo "View community")
              .counts.subscribers
              .counts.users_active_month .counts.posts
              (if (equal "Subscribed" .subscribed)
                  (if (char-displayable-p (string-to-char "✓"))
                      "✓"
                    "*")
                "")
              (propertize (url-host
                           (url-generic-parse-url .community.actor_id))
                          'help-echo .community.actor_id
                          'id .community.id
                          'follow-link t
                          'type 'lem-tl-button
                          'category 'shr
                          'mouse-face 'highlight
                          'shr-url .community.actor_id))
             ;; don't try to propertize numbers:
             collect (if (stringp i)
                         (propertize i
                                     'id .community.id
                                     'lem-type 'community)
                       i))))

;;; WIDGETS

(defun lem-ui-return-item-widgets (list)
  "Return a list of item widgets for each item, a string, in LIST."
  (cl-loop for x in list
           collect `(choice-item :value ,x :format "%[%v%] ")))

(defun lem-ui-widget-format (str &optional binding)
  "Return a widget format string for STR, its name.
BINDING is a string of a keybinding to cycle the widget's value."
  (concat "%[" (propertize str
                           'face 'lem-ui-widget-face
                           'lem-tab-stop t)
          "%]: %v"
          binding))

(defun lem-ui-widget-reset-value (widget value msg)
  "Reset WIDGET to its previous VALUE.
USED to not update widget display if the sort chosen is
unavailable in the current view.
MSG is the error message string to display."
  (widget-value-set widget value)
  (message "%s" (error-message-string msg)))

(defun lem-ui-widget-notify-fun (old-value)
  "Return a widget notify function.
OLD-VALUE is the widget's value before being changed."
  `(lambda (widget &rest ignore)
     (let ((value (widget-value widget))
           (tag (widget-get widget :tag)))
       (cond ((equal tag "Listing")
              (lem-ui-cycle-listing-type value))
             ((equal tag "Sort")
              (condition-case x
                  (lem-ui-cycle-sort value)
                (user-error ; don't update widget if cycle-sort fails:
                 (lem-ui-widget-reset-value widget ,old-value x))))
             (t (message "Widget kind not implemented yet"))))))

(defun lem-ui-widget-create (kind value)
  "Return a widget of KIND, with default VALUE.
KIND is a string, either Listing, Sort, Items, or Inbox, and will
be used for the widget's tag.
VALUE is a string, a member of the list associated with KIND."
  (let ((type-list (cond ((equal kind "Listing")
                          lem-listing-types)
                         ((equal kind "Sort")
                          (cond ((or (eq (lem-ui-view-type) 'post)
                                     (equal (lem-ui-get-buffer-spec :item) "comments"))
                                 lem-comment-sort-types)
                                ((or (eq (lem-ui-view-type) 'user)
                                     (eq (lem-ui-view-type) 'current-user))
                                 lem-user-view-sort-types)
                                (t
                                 lem-sort-types)))
                         ((equal kind "Inbox")
                          lem-inbox-types)
                         ;; maybe items is useless as we have headings:
                         ((equal kind "Items")
                          lem-items-types))))
    (if (not (member value type-list))
        (error "%s is not a member of %s" value type-list)
      (widget-create 'menu-choice
                     :tag kind
                     :value value
                     :args (lem-ui-return-item-widgets type-list)
                     :help-echo (format "Select a %s kind" kind)
                     :format (lem-ui-widget-format kind) ; "C-c C-c")
                     :notify (lem-ui-widget-notify-fun value)
                     :keymap lem-widget-keymap))))

(defun lem-ui-widgets-create (plist)
  "PLIST is a plist of kind and value arguments for `lem-ui-widget-create'."
  (while plist
    (funcall #'lem-ui-widget-create (pop plist) (pop plist)))
  (insert "\n"))

(defun lem-ui-browse-communities (&optional type sort limit)
  "View Lemmy communities in a sortable tabulated list.
TYPE must be one of `lem-listing-types'.
SORT must be one of `lem-sort-types'.
LIMIT is the max results to return."
  (interactive)
  (let* ((type (or type "All"))
         (sort (or sort "TopMonth"))
         (limit (or limit "50")) ; max
         (json (lem-list-communities type sort limit))
         (buf "*lem-communities*"))
    (lem-ui-with-buffer buf 'lem-mode nil nil
      (lem-ui-render-instance (lem-get-instance) :stats nil)
      (lem-ui-set-buffer-spec
       type sort #'lem-ui-browse-communities 'communities)
      (lem-ui-widgets-create `("Listing" ,type "Sort" ,sort))
      (make-vtable
       :use-header-line nil
       :columns '((:name "Name" :max-width 30 :width "35%")
                  (:name "Members" :width "7%")
                  (:name "Monthly users" :width "7%")
                  (:name "Posts" :width "7%")
                  (:name "Sub" ;; :min-width 4
                         :width "5%")
                  (:name "URL" :max-width 30 :width "30%"))
       :objects-function
       (lambda ()
         (cl-loop for c in (alist-get 'communities json)
                  collect (lem-ui-return-community-obj c)))
       :row-colors  '(nil highlight)    ; don't set vtable a second time
       :divider-width 1
       :keymap lem-vtable-map))))
;; whey "actions" when we have map + our own props?:
;; :actions '("RET" lem-ui-view-community-at-point-tl
;; "s" lem-ui-subscribe-to-community-at-point-tl))

;; actions are called on the column's object, but we use text props instead,
;; so we have to reimplement these for tl:
;; (defun lem-ui-view-community-at-point-tl (_)
;;   "View community at point, from tabulated list."
;;   (interactive)
;;   (lem-ui-view-item-community))

;; (defun lem-ui-subscribe-to-community-at-point-tl (_)
;;   "Subscribe to community at point, from tabulated list."
;;   (interactive)
;;   (lem-ui-subscribe-to-community-at-point))

(defun lem-ui-subscribe-to-community (&optional id)
  "Subscribe to a community, using ID or prompt for a handle."
  (interactive)
  (let* ((handle (unless id
                   (read-string "Subscribe to community (by handle): ")))
         (community (unless id
                      (lem-get-community nil handle))))
    (if-let ((id (or id (lem-ui-get-community-id community)))
             (fol (lem-follow-community id t))
             (comm (alist-get 'community
                              (alist-get 'community_view fol)))
             (name (or (alist-get 'title comm)
                       (alist-get 'name comm))))
        (lem-ui-response-msg fol
                             'community_view :non-nil
                             (format "Subscribed to community %s!" name)))))

(defun lem-ui-subscribe-to-community-at-point ()
  "Subscribe to community at point."
  (interactive)
  (lem-ui-with-item 'community
    (if (not (equal 'community (lem-ui--item-type)))
        (message "no community at point?")
      (lem-ui-subscribe-to-community id))
    :number))

(defun lem-ui-unsubscribe-from-community ()
  "Prompt for a subscribed community and unsubscribe from it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-subscribed-communities
   #'lem-ui--communities-list
   "Unsubscribe from community: "
   (lambda (id choice)
     (when (y-or-n-p (format "Unsubscribe from %s?" choice))
       (lem-ui-response-msg
        (lem-follow-community id :json-false)
        'community_view :non-nil
        (format "Community %s unsubscribed!" choice))))))

(defun lem-ui-block-community-at-point ()
  "Block community at point."
  (interactive)
  (lem-ui-with-item 'community
    (if (not (equal 'community (lem-ui--item-type)))
        (message "no community at point?")
      (let-alist (lem-ui--property 'json)
        (when (y-or-n-p (format "Block community %s?" .community.name))
          (lem-ui-response-msg
           (lem-block-community .community.id t)
           'blocked t
           (format "Community %s blocked!" .community.name))))
      :number)))

;; TODO: block item-community

(defun lem-ui-unblock-community ()
  "Prompt for a blocked community, and unblock it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-blocked-communities
   #'lem-ui--communities-list
   "Unblock community: "
   (lambda (id choice)
     (lem-ui-response-msg
      (lem-block-community id :json-false)
      'blocked :json-false
      (format "Community %s unblocked!" choice)))))

(defun lem-ui-jump-to-subscribed ()
  "Prompt for a subscribed community and view it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-subscribed-communities
   #'lem-ui--communities-list
   "Jump to community: "
   (lambda (id _choice)
     (lem-ui-view-community id "posts"))))

(defun lem-ui-jump-to-moderated ()
  "Prompt for a community moderated by the current user and view it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-moderated-communities
   #'lem-ui--communities-list
   "Jump to moderated community: "
   (lambda (id _choice)
     (lem-ui-view-community id "posts"))))

(defun lem-ui-view-community (id &optional item sort limit page)
  "View community with ID.
ITEM must be a member of `lem-items-types'.
SORT must be a member of `lem-sort-types'.
LIMIT is the amount of results to return.
PAGE is the page number of items to display, a string."
  (let* ((community (lem-get-community id))
         (buf (format "*lem-community-%s*" id))
         ;; in case we set community posts, then switch to comments:
         (sort (if (equal item "comments")
                   (if (lem-comment-sort-type-p sort)
                       sort
                     (lem-ui-view-default-sort 'community))
                 (or sort
                     (lem-ui-view-default-sort 'community))))
         (items (if (equal item "comments")
                    (alist-get 'comments
                               (lem-api-get-community-comments
                                id nil sort limit page))
                  (alist-get 'posts
                             (lem-api-get-community-posts-by-id
                              id nil sort limit page)))) ; no sorting
         (bindings (lem-ui-view-options 'community)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (lem-ui-render-community community :stats :view)
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-community
                              (or item "posts") page)
      (lem-ui-widgets-create `("Sort" ,sort))
      (if (equal item "comments")
          (progn
            (lem-ui-insert-heading "comments")
            (lem-ui-render-comments items)) ; no type
        (lem-ui-insert-heading (or item "posts"))
        (lem-ui-render-posts items nil :trim)) ; no children
      (lem-ui--init-view))))

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
           (when (eq t .community.posting_restricted_to_mods)
             (concat " " (lem-ui-symbol 'locked)))
           "\n"
           (lem-ui-font-lock-comment .community.actor_id)
           (unless brief (concat "\n" desc "\n"
                                 lem-ui-horiz-bar "\n")))
          'json community
          'mods mods-list
          'byline-top t ; next/prev hack
          'id .community.id
          'lem-type 'community)))
      ;; stats:
      (when stats
        (lem-ui-render-stats .counts.subscribers
                             .counts.posts
                             .counts.comments))
      (unless brief
        (insert (concat ;" "
                 (when (eq .community.nsfw 't)
                   (concat (propertize "NSFW"
                                       'face 'success)
                           " | "))
                 .subscribed "\n"))))
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
     (format "%s %s | %s %s | %s %s | %s %s\n"
             s-sym s p-sym p c-sym c ties-sym ties))))

(defun lem-ui-view-item-community ()
  "View community of item at point."
  (interactive)
  (lem-ui-with-item 'all
    (let ((type (lem-ui--property 'lem-type))
          (id (or (lem-ui--property 'community-id)
                  (lem-ui--property 'id)))) ; community header
      (if (or (eq type 'instance)
              (eq type 'user))
          (user-error "Item has no community")
        (lem-ui-view-community id)))))

(defun lem-ui-subscribe-to-item-community ()
  "Subscribe to community of item at point."
  (interactive)
  (lem-ui-with-item 'all
    (let ((type (lem-ui--property 'lem-type))
          (id (or (lem-ui--property 'community-id)
                  (lem-ui--property 'id))) ; community header
          (url (let-alist (lem-ui--property 'json)
                 .community.actor_id)))
      (if (or (eq type 'instance)
              (eq type 'user))
          (user-error "Item has no community")
        (when (y-or-n-p (format "Subscribe to %s?" url))
          (lem-ui-subscribe-to-community id))))))

(defun lem-ui-delete-community ()
  "Prompt for a community moderated by the current user and delete it."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-moderated-communities
   #'lem-ui--communities-list
   "Delete community: "
   (lambda (id choice)
     (lem-ui-delete-community-do id choice))))

(defun lem-ui-delete-community-do (id name)
  "Delete community with ID and NAME, after confirmation."
  (when (y-or-n-p (format "Delete community %s?" name))
    (lem-ui-response-msg
     (lem-delete-community id t)
     'community_view :non-nil
     (format "Community %s deleted!" name))))

(defun lem-ui-delete-community-at-point ()
  "Delete community at point."
  (interactive)
  (let* ((id (lem-ui--property 'id))
         (community (alist-get 'community
                               (lem-ui--property 'json)))
         (name (alist-get 'name community))
         (mods (lem-ui--property 'mods))
         (ids (lem-ui-mods-ids mods))
         (own-p (member lem-user-id ids)))
    (cond ((not (eq 'community (lem-ui--property 'lem-type)))
           (user-error "No community at point"))
          ((not own-p)
           (user-error "Must be a mod to delete community"))
          (t
           (lem-ui-delete-community-do id name)))))

;;; INBOX / REPLIES / MENTIONS / PMS

(defun lem-ui-view-replies-unread ()
  "View unread replies."
  (interactive)
  (lem-ui-view-replies :unread))

(defun lem-ui-view-replies (&optional _unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (lem-ui-view-inbox 'replies))

(defun lem-ui-render-replies (replies)
  "Render REPLIES, reply comments to the current user."
  (cl-loop for reply in replies
           do (lem-ui-render-comment reply :reply :details)))

(defun lem-ui-mark-reply-comment-read ()
  "Mark the comment-reply at point as read."
  (interactive)
  (let ((id (lem-ui--property 'id)))
    (lem-mark-comment-reply-read id)))

(defun lem-ui-mark-all-read ()
  "Mark all replies as read."
  (interactive)
  (lem-mark-all-read))

(defun lem-ui-view-mentions (&optional _unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (lem-ui-view-inbox 'mentions))

(defun lem-ui-render-mention (mention)
  "Render MENTION."
  (let ((comment (alist-get 'comment mention)))
    (insert
     (lem-ui-format-comment comment)
     "\n")))

(defun lem-ui-render-mentions (mentions)
  "Render mentions MENTIONS."
  (cl-loop for men in mentions
           do (lem-ui-render-mention men)))

(defun lem-ui-view-private-messages (&optional _unread)
  "View reply comments to the current user.
Optionally only view UNREAD items."
  (interactive)
  (lem-ui-view-inbox 'private-messages))

(defun lem-ui-render-private-message (pm)
  "Render PM, a private message."
  (insert
   (lem-ui-format-private-message pm)
   "\n"))

(defun lem-ui-render-private-messages (private-messages)
  "Render private messages PRIVATE-MESSAGES."
  (cl-loop for pm in private-messages
           do (lem-ui-render-private-message pm)))

(defun lem-ui-mark-private-message-read ()
  "Mark the private message at point as read."
  (interactive)
  (let ((id (lem-ui--property 'id)))
    (lem-mark-private-message-read id)))

(defun lem-ui-view-inbox (&optional items sort unread)
  "View user inbox, for replies, mentions, and PMs to the current user.
Optionally only view UNREAD items.
Optionally set ITEMS, a symbol, to view.
SORT is a member of `lem-inbox-sort-types'.
Sorting is not available for private messages, nor for all."
  (interactive)
  ;; NB: Sort only works for replies and mentions
  ;; Web UI sorts "all" by score for user page, maybe also for inbox?
  ;; Web UI offers all of `lem-inbox-sort-types' for sorting, but API
  ;; doesn't offer sorting for get private messages.
  (let* ((sort (or sort (lem-ui-view-default-sort 'inbox)))
         (unread-str (if unread "true" nil))
         (items (or items 'all))
         (item-fun (if (eq items 'all)
                       'lem-ui-get-inbox-all
                     (lem-ui-make-fun "lem-get-" items)))
         (render-fun (if (eq items 'all)
                         'lem-ui-render-inbox-all
                       (lem-ui-make-fun "lem-ui-render-" items)))
         (items-data (cond ((eq items 'all)
                            ;; all, no sort:
                            (funcall item-fun unread-str))
                           ((eq item-fun 'lem-get-private-messages)
                            ;; pms: unread-only page limit creator-id:
                            ;; no sort:
                            (funcall item-fun unread-str))
                           ;; mentions/replies: sort page limit unread-only
                           (t
                            (funcall item-fun
                                     sort
                                     nil ; page
                                     nil ;limit
                                     unread-str))))
         (list (if (eq items 'all)
                   items-data
                 (alist-get (lem-ui-hyphen-to-underscore items) items-data)))
         (buf "*lem-inbox*")
         (bindings (lem-ui-view-options 'inbox)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      (lem-ui-insert-heading (format "inbox: %s" items))
      (funcall render-fun list)
      (lem-ui--init-view)
      (lem-ui-set-buffer-spec nil sort #'lem-ui-view-inbox
                              items nil unread))))

(defun lem-ui-get-inbox-all (&optional unread)
  "Return a merged list of replies, mentions, and private messages.
Optionally only return UNREAD items."
  (let ((replies (alist-get 'replies
                            (lem-get-replies nil nil nil unread)))
        (mentions (alist-get 'mentions
                             (lem-get-mentions nil nil nil unread)))
        (pms (alist-get 'private_messages
                        (lem-get-private-messages unread))))
    (append replies mentions pms)))

(defun lem-ui-render-inbox-all (data)
  "Render DATA, a mix of replies, mentions, and private messages."
  (let ((sorted (sort data #'lem-ui-published-sort-predicate)))
    (cl-loop for item in sorted
             do (let ((type (caar item)))
                  (cond ((eq type 'comment_reply)
                         (lem-ui-render-comment item :reply :details))
                        ((eq type 'mention)
                         (lem-ui-render-mention item))
                        ((eq type 'private_message)
                         (lem-ui-render-private-message item)))))))

(defun lem-ui-cycle-inbox ()
  "Cycle inbox to next item view in `lem-inbox-types'."
  (interactive)
  (let* ((last (lem-ui-get-buffer-spec :item))
         (next (lem-ui-next-type last lem-inbox-types))
         (sort (lem-ui-get-buffer-spec :sort)))
    ;; TODO: implement unread arg
    (lem-ui-view-inbox next sort)))

(defun lem-ui-choose-inbox-view ()
  "Prompt for an inbox view and load it."
  (interactive)
  (let ((choice (intern
                 (completing-read "Inbox view: " lem-inbox-types)))
        (sort (lem-ui-get-buffer-spec :sort)))
    (lem-ui-view-inbox choice sort)))

;;; EDIT/DELETE POSTS/COMMENTS

(defun lem-ui-edit-comment-brief ()
  "Edit comment at point if possible, in the minibuffer."
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
    (let-alist (lem-ui--property 'json)
      (let* ((id (lem-ui--property 'id))
             (del-p (or (eq t .post.deleted)
                        (eq t .comment.deleted))))
        (cond
         ((and del-p (not restore))
          (user-error "Item already deleted?"))
         ((and restore (not del-p))
          (user-error "Item not deleted?"))
         (t
          (when (y-or-n-p (format "%s %s?"
                                  (if restore "Restore" "Delete")
                                  item))
            (let ((response (funcall fun id (if restore :json-false t)))
                  (view (lem-ui-get-buffer-spec :view-fun))
                  (indent (lem-ui--current-indent)))
              (lem-ui-response-msg
               response
               (lem-ui-item-to-alist-key item) :non-nil
               (format "%s %s %s!" item id
                       (if restore "restored" "deleted")))
              (lem-ui--update-item-json response)
              (if (eq item 'post)
                  (lem-ui-update-item-from-json
                   'byline-top
                   (lambda (response)
                     (lem-ui-top-byline-replace
                      (alist-get 'post_view response)
                      (unless (eq view 'lem-ui-view-community)
                        :community))))
                (lem-ui-update-item-from-json
                 'lem-type
                 (lambda (response)
                   (lem-ui-format-comment (alist-get 'comment_view response)
                                          indent nil :details)))
                (lem-ui-update-parent-item-maybe))))))))))

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

(defun lem-ui-restore-comment ()
  "Restore deleted comment at point."
  (interactive)
  (lem-ui-delete-item 'comment #'lem-delete-comment :restore))

(defun lem-ui-delete-post-or-comment ()
  "Delete post or comment at point."
  (interactive)
  ;; TODO: check for deleted status first
  (let ((type (lem-ui--property 'lem-type)))
    (cond ((eq type 'post)
           (lem-ui-delete-post))
          ((eq type 'comment)
           (lem-ui-delete-comment)))))

;;; REMOVING

(defun lem-ui-remove-item (item fun &optional restore)
  "Remove item of type ITEM at point, calling FUN.
If RESTORE, restore the item instead."
  (lem-ui-with-item item
    (let-alist (lem-ui--property 'json)
      (let* ((id (lem-ui--property 'id)))
        ;; TODO: check if removed necessary?
        
        ;; (del-p (or (eq t .post.deleted)
        ;; (eq t .comment.deleted))))
        ;; (cond
        ;; ((and del-p (not restore))
        ;; (user-error "Item already deleted?"))
        ;; ((and restore (not del-p))
        ;; (user-error "Item not deleted?"))
        ;; (t
        (when (y-or-n-p (format "%s %s?"
                                (if restore "Restore" "Remove")
                                item))
          (let ((response (funcall fun id (if restore :json-false t)))
                (view (lem-ui-get-buffer-spec :view-fun))
                (indent (lem-ui--current-indent)))
            (lem-ui-response-msg
             response
             (lem-ui-item-to-alist-key item) :non-nil
             (format "%s %s %s!" item id
                     (if restore "restored" "removed")))
            (lem-ui--update-item-json response)
            (if (eq item 'post)
                (lem-ui-update-item-from-json
                 'byline-top
                 (lambda (response)
                   (lem-ui-top-byline-replace
                    (alist-get 'post_view response)
                    (unless (eq view 'lem-ui-view-community)
                      :community))))
              (lem-ui-update-item-from-json
               'lem-type
               (lambda (response)
                 (lem-ui-format-comment (alist-get 'comment_view response)
                                        indent nil :details)))
              (lem-ui-update-parent-item-maybe))))))))

(defun lem-ui-remove-post ()
  "Remove the post at point.
To remove an item, you must be a moderator in its community."
  (interactive)
  (lem-ui-with-item 'post
    (let-alist (lem-ui--property 'json)
      (let ((mod-p (or (eq t .creator_is_moderator)
                       (cl-member .creator.id lem-ui-post-community-mods-ids))))
        (if (not mod-p)
            (user-error "You need to be a mod to remove items")
          ;; TODO: refactor delete-item: "delete" str, and reload on remove.
          (lem-ui-delete-item 'post #'lem-remove-post))))))


(defun lem-ui-remove-comment ()
  "Remove the comment at point.
To remove an item, you must be a moderator in its community."
  (interactive)
  (lem-ui-with-item 'comment
    (let-alist (lem-ui--property 'json)
      (let ((mod-p (or (eq t .creator_is_moderator)
                       (cl-member .creator.id lem-ui-post-community-mods-ids))))
        (if (not mod-p)
            (user-error "You need to be a mod to remove items")
          ;; TODO: refactor delete-item: "delete" str, and reload on remove.
          (lem-ui-remove-item 'comment #'lem-remove-comment))))))

;;; COMMENTS

(defun lem-ui-render-comment (comment &optional reply details)
  "Render single COMMENT.
REPLY means it is a comment-reply object.
DETAILS means display what community and post the comment is linked to."
  (insert
   (lem-ui-format-comment comment nil reply details)
   "\n"))

(defun lem-ui-render-comments (comments &optional details)
  "Render COMMENTS, a list of comment objects.
;; TYPE
;; SORT.
For viewing a plain list of comments, not a hierarchy.
DETAILS means display what community and post the comment is linked to."
  (cl-loop for x in comments
           do (lem-ui-render-comment x nil details)))

;;; THREADED COMMENTS
;; Path: "The path / tree location of a comment, separated by dots, ending
;; with the comment's id. Ex: 0.24.27"
;; https://github.com/LemmyNet/lemmy/blob/63d3759c481ff2d7594d391ae86e881e2aeca56d/crates/db_schema/src/source/comment.rs#L39
(defvar-local lem-comments-hierarchy nil)
(defvar-local lem-comments-raw nil)

(defun lem-ui--build-and-render-comments-hierarchy (comments id)
  "Build `lem-comments-hierarchy', a hierarchy, from COMMENTS, and render.
ID is the post's id, used for unique buffer names."
  (setq lem-comments-raw comments)
  (let ((list (alist-get 'comments comments))
        (buf (format "*lem-post-%s*" id)))
    (lem-ui--build-hierarchy list) ; sets `lem-comments-hierarchy'
    (with-current-buffer (get-buffer-create buf)
      (let ((inhibit-read-only t))
        (lem--hierarchy-print-line
         lem-comments-hierarchy
         (lem--hierarchy-labelfn-indent
          (lambda (item indent)
            (lem-ui-format-comment item indent))))))))
;; `lem--hierarchy-labelfn-indent' no longer handles line-prefixing:
;; (lem-ui-symbol 'reply-bar)
;; 'face ':foreground 'lem-ui-cycle-colors))))))

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
         (file (url-filename parsed))
         (case-fold-search t))
    (save-match-data
      ;; TODO: add further legit urls:
      (when (string-match "^/u\\(sers\\)?/[_[:alnum:]]+$" file)
        (let ((split (split-string file "/" t)))
          (propertize
           (concat "@" (cadr split) "@" host)
           ;; props
           ))))))

(defvar lem-ui-indent-colors
  '("red3" "orange3" "green3" "yellow3" "blue3")
  ;; Tried with rainbow-delimiters colors but they don't match the actual
  ;; display of my lovely parens?!
  ;; '("#707183"
  ;;   "#7388d6" "#909183" "#709870" "#907373"
  ;;   "#6276ba" "#858580" "#80a880" "#887070")
  ;; '("grey55" "#93a8c6" "#b0b1a3" "#97b098" "#aebed8"
  ;;   "#b0b0b3" "#90a890" "#a2b6da" "#9cb6ad")
  "List of colors for indent bars, subsequent items repeat.")

(defun lem-ui-cycle-colors (index)
  "Given INDEX, a number, cycle through `lem-ui-indent-colors'."
  (nth
   (mod index
        (length
         lem-ui-indent-colors))
   lem-ui-indent-colors))

(defun lem-ui--make-colored-indent-str (indent)
  "INDENT is the number of indent bars to return."
  (let ((str (make-string indent
                          (string-to-char
                           (lem-ui-symbol 'reply-bar)))))
    (dotimes (index indent)
      (add-text-properties
       index (1+ index)
       `(face (:foreground ,(lem-ui-cycle-colors index)))
       str))
    str))

(defun lem-ui-format-comment (comment &optional indent reply details)
  "Format COMMENT, optionally with INDENT amount of indent bars.
REPLY means it is a comment-reply object.
DETAILS means display what community and post the comment is linked to."
  ;; NB: no stray requests in here.
  (let-alist comment
    (let ((content (when .comment.content
                     (lem-ui-render-body .comment.content
                                         (alist-get 'comment comment)
                                         indent)))
          (indent-str
           ;; NB this is also done in `lem--hierarchy-labelfn-indent'
           ;; to propertize the first line's indent bars:
           (when indent
             (lem-ui--make-colored-indent-str indent)))
          (handle (lem-ui--handle-from-user-url .creator.actor_id))
          (post-title (when details .post.name))
          (community-name (when details (or .community.title
                                            .community.name)))
          (community-url (when details .community.actor_id))
          (admin-p (eq t .creator_is_admin))
          (mod-p (or (cl-member .creator.id lem-ui-post-community-mods-ids)
                     (eq t .creator_is_moderator)))
          (op-p (eq .comment.creator_id .post.creator_id))
          (deleted .comment.deleted)
          (removed .comment.removed))
      (push .comment.id lem-ui-current-items) ; pagination
      (propertize
       (concat
        (lem-ui-top-byline nil nil
                           (or .creator.display_name .creator.name)
                           .counts.score
                           .comment.published
                           community-name community-url
                           nil op-p admin-p mod-p nil handle
                           post-title .comment.updated)
        "\n"
        (if (or (eq t deleted) (eq t removed))
            (lem-ui-format-display-prop deleted removed)
          (propertize (or content "")
                      'body t))
        (propertize
         (concat
          "\n"
          (lem-ui-bt-byline .counts.score .counts.child_count .my_vote .saved)
          "\n" lem-ui-horiz-bar
          "\n")
         'byline-bt-fold t))
       'json comment
       ;; in replies view we need the actual id for like-toggling:
       'id .comment.id ;(if reply .comment_reply.id .comment.id)
       'post-id .comment.post_id
       'community-id .post.community_id
       'creator-id .creator.id
       'lem-type (if reply 'comment-reply 'comment)
       'line-prefix indent-str))))

(defun lem-ui-format-display-prop (del rem)
  "Format a string for display property.
DEL and REM are the values of the deleted and removed attributes
in an item's data."
  (cond ((eq del t)
         (propertize "[deleted by user]\n"
                     'face '(:slant italic)
                     'body t))
        ((eq rem t)
         (propertize "[removed by mod]\n"
                     'face '(:slant italic)
                     'body t))
        (t nil)))

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
        ;; "\n"
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
  ;; NB: max_depth, required for comment trees, breaks limit
  ;; and that's why huge threads are slow to load :/
  (let* ((comments (lem-api-get-post-comments
                    post-id "All" sort (or limit lem-ui-comments-limit)
                    nil nil lem-api-comments-max-depth)))
    (if (eq 'string (type-of comments))
        (message comments) ; server error
      (let ((unique-comments (cl-remove-duplicates comments)))
        (lem-ui--build-and-render-comments-hierarchy unique-comments
                                                     post-id)))))

(defun lem-ui-plural-symbol (symbol)
  "Return a plural of SYMBOL."
  (cond ((eq symbol 'community)
         'communities)
        ((or (eq symbol 'replies)
             (eq symbol 'replies)
             (eq symbol 'mentions)
             (eq symbol 'private-messages))
         symbol)
        (t
         (intern
          (concat (symbol-name symbol) "s")))))

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

(defun lem-ui-search-type-symbol (type)
  "Make TYPE, a string, singular and a symbol."
  (cond ((equal type "communities")
         'community)
        ((equal type "posts")
         'post)
        ((equal type "users")
         'user)
        ((equal type "comments")
         'comment)))

(defun lem-ui-more ()
  "Append more items to the current view."
  (interactive)
  (let ((item (lem-ui-get-buffer-spec :item))
        ;; TODO: use `lem-ui-view-type' instead
        (view-fun (lem-ui-get-buffer-spec :view-fun)))
    (cond ((eq view-fun 'lem-ui-view-post)
           ;; nb max-depth doesn't work with pagination yet:
           ;; https://github.com/LemmyNet/lemmy/issues/3585
           (lem-ui-more-items 'comment 'lem-api-get-post-comments
                              'lem-ui--build-and-render-comments-hierarchy))
          ((eq view-fun 'lem-ui-view-community)
           (if (equal item "posts")
               (lem-ui-more-items 'post 'lem-api-get-community-posts-by-id
                                  'lem-ui-render-posts)
             (lem-ui-more-items 'comment 'lem-api-get-community-comments-by-id
                                'lem-ui-render-comments)))
          ((eq view-fun 'lem-ui-view-instance)
           (lem-ui-more-items 'post 'lem-api-get-instance-posts
                              'lem-ui-render-posts-instance))
          ((eq view-fun 'lem-ui-view-user)
           ;; TODO: user overview view type:
           (if (equal item "posts")
               (lem-ui-more-items 'post 'lem-api-get-person-posts
                                  'lem-ui-render-posts)
             (lem-ui-more-items 'comment 'lem-api-get-person-comments
                                'lem-ui-render-comments)))
          ((eq item 'lem-ui-view-communities)
           (lem-ui-more-items 'community 'lem-list-communities
                              'lem-ui-render-communities))
          ((eq view-fun 'lem-ui-search)
           (let* ((search-type (lem-ui-get-buffer-spec :item))
                  (render-fun (lem-ui-search-type-fun search-type))
                  (search-type-symbol (lem-ui-search-type-symbol search-type)))
             (lem-ui-more-items search-type-symbol 'lem-search render-fun)))
          ((eq view-fun 'lem-ui-view-inbox)
           (let* ((items (lem-ui-get-buffer-spec :item))
                  (get-fun (lem-ui-make-fun "lem-get-" items))
                  (render-fun (lem-ui-make-fun "lem-ui-render-" items)))
             (lem-ui-more-items items get-fun render-fun)))
          (t (message "More type not implemented yet")))))

(defun lem-ui-more-items (type get-fun render-fun)
  "Add one more page of items of TYPE to the current view.
GET-FUN is the name of a function to fetch more items.
RENDER-FUN is the name of a function to render them."
  (message "Loading more items...")
  (let* ((listing (lem-ui-get-buffer-spec :listing-type))
         (view-fun (lem-ui-get-buffer-spec :view-fun))
         (page (1+ (lem-ui-get-buffer-spec :page)))
         (item (lem-ui-get-buffer-spec :item))
         (sort (lem-ui-get-buffer-spec :sort))
         (query (lem-ui-get-buffer-spec :query))
         (id (save-excursion
               (goto-char (point-min))
               (lem-ui--property 'id)))
         (all-items
          ;; get-instance-posts have no need of id arg:
          (cond ((or (eq get-fun 'lem-api-get-instance-posts)
                     (eq get-fun 'lem-list-communities))
                 (funcall get-fun
                          (or listing "All")
                          sort
                          lem-ui-comments-limit
                          page))
                ;; user funs have no list-type arg:
                ((eq view-fun 'lem-ui-view-user)
                 (funcall get-fun id sort
                          lem-ui-comments-limit page))
                ((eq view-fun 'lem-ui-search)
                 (funcall get-fun query (capitalize item) listing sort
                          lem-ui-comments-limit
                          page))
                ((eq view-fun 'lem-ui-view-inbox)
                 ;; mentions/replies: sort page limit unread-only
                 ;; pms: unread-only page limit creator-id
                 (funcall get-fun nil page))
                (t
                 (funcall get-fun
                          id
                          (or listing "All")
                          sort
                          lem-ui-comments-limit
                          page))))
         (no-duplicates (lem-ui-remove-displayed-items all-items type)))
    (setf (alist-get (lem-ui-plural-symbol type) all-items)
          no-duplicates)
    (lem-ui-set-buffer-spec listing
                            sort
                            view-fun
                            item
                            page
                            nil
                            query)
    (goto-char (point-max))
    (let ((old-max (point))
          (inhibit-read-only t))
      ;; NB: `lem-ui-current-items' is updated during rendering:
      (if (eq render-fun 'lem-ui--build-and-render-comments-hierarchy)
          (funcall render-fun all-items id)
        (funcall render-fun (alist-get (lem-ui-plural-symbol type)
                                       all-items)))
      (goto-char old-max)
      (lem-ui--init-view)
      (message "Loading more items... [done]"))))

(defun lem-ui-post-goto-comment (comment-id post-id &optional no-recenter)
  "Move point to comment with COMMENT-ID, a number, if possible.
POST-ID is the post's id, used to fetch the right buffer.
NO-RECENTER means don't call `recenter-top-bottom'."
  ;; TODO: implement forward-search/pagination
  (with-current-buffer (format "*lem-post-%s*" post-id)
    (goto-char (point-min))
    (when-let ((match (text-property-search-forward 'id comment-id t)))
      (goto-char (prop-match-beginning match))
      (unless no-recenter
        (recenter-top-bottom '(4))))))

(defun lem-ui-view-comment-post (&optional post-id comment-id)
  "View post of comment at point, or of POST-ID.
If COMMENT-ID is provided, move point to that comment."
  (interactive)
  (let ((comment-p (or (eq (lem-ui--item-type) 'comment)
                       (eq (lem-ui--item-type) 'comment-reply))))
    (if (not (or post-id
                 comment-p))
        (message "Not at a comment?")
      (let* ((post-id (or post-id (lem-ui--property 'post-id)))
             (comment-id (or comment-id
                             (when comment-p
                               (lem-ui--property 'id)))))
        (lem-ui-view-post post-id)
        (lem-ui-post-goto-comment comment-id post-id)))))

(defun lem-ui-prev-top-level ()
  "Move to previous top level comment.
If not currently at a top level comment, move to top of current branch."
  (interactive)
  (if (not (eq (lem-ui-view-type) 'post))
      (message "Not in a post view.")
    (let ((current-indent (lem-ui--current-indent)))
      (if (not (eq 0 current-indent))
          (lem-ui-branch-top-level)
        (lem-prev-item)
        (while (not (eq 0 (lem-ui--current-indent)))
          (lem-prev-item))))))

(defun lem-ui-next-top-level ()
  "Move to next top level comment."
  (interactive)
  (if (not (eq (lem-ui-view-type) 'post))
      (message "Not in a post view.")
    (let ((current-indent (lem-ui--current-indent)))
      (if (not (eq 0 current-indent))
          (while (not (eq 0 (lem-ui--current-indent)))
            (lem-next-item))
        (lem-next-item)
        (while (not (eq 0 (lem-ui--current-indent)))
          (lem-next-item))))))

(defun lem-ui--goto-parent-comment ()
  "Move point to parent comment.
Stop moving up at a top level comment."
  (let ((parent-id (lem-ui--parent-id (lem-ui--property 'json)))
        (post-id (lem-ui--property 'post-id)))
    (if (not parent-id)
        (message "At top level")
      (lem-ui-post-goto-comment parent-id post-id :no-recenter))))

(defun lem-ui-branch-top-level ()
  "Move point to the top of the branch of comment at point."
  (interactive)
  (lem-ui-with-item 'comment
    (while (lem-ui--parent-id (lem-ui--property 'json))
      (lem-ui--goto-parent-comment))))

;;; FOLDING COMMENTS

(defun lem-ui--set-invis-prop (invis pos)
  "Return value of INVIS as a boolean.
If INVIS is nil, return the opposite of the invisibility property at
POS."
  (cond ((eq invis :invisible)
         t)
        ((eq invis :not-invisible)
         nil)
        (t
         (not
          (get-text-property pos
                             'invisible)))))

(defun lem-ui-comment-fold-toggle (&optional invis)
  "Toggle invisibility of the comment at point.
Optionally set it to INVIS, a keyword.
Return the value of the invisibility property after toggling as
a keyword."
  (interactive)
  (lem-ui-with-item 'comment
    (let* ((inhibit-read-only t)
           (comment-range (lem-ui--find-property-range 'body
                                                       (point)))
           (byline-top (lem-ui--find-property-range 'byline-top
                                                    (point)))
           (byline-bottom (lem-ui--find-property-range 'byline-bt-fold
                                                       (point)))
           (invis-before (when comment-range
                           (get-text-property (car comment-range)
                                              'invisible))))
      (when byline-top
        (add-text-properties
         (car byline-top)
         (cdr byline-top)
         '(folded t))
        ;; set body:
        (add-text-properties
         (car comment-range)
         (cdr comment-range)
         `(invisible
           ,(lem-ui--set-invis-prop invis (car comment-range))))
        ;; set bottom byline:
        (add-text-properties
         (car byline-bottom)
         (cdr byline-bottom)
         `(invisible
           ,(lem-ui--set-invis-prop invis (car byline-bottom))))
        ;; return result of toggle as kw:
        (or invis ; kw
            (if invis-before
                :not-invisible
              :invisible))))))

(defun lem-ui-comment-tree-fold (&optional invis indent)
  "Toggle invisibility of current comment and all its children.
Optionally set INVIS, a keyword (used for recursion).
The invisibility of children should not necessarily be toggled,
but should adopt the invisibility of the top-most item. So if
some children comments have been toggled, toggling their parent
should return all items in the branch to the same invisibility.
INDENT is the level of the top level comment to be folded."
  (interactive)
  (let* ((top-indent (or indent
                         (lem-ui--current-indent)))
         ;; fold current item:
         (invis-after (lem-ui-comment-fold-toggle invis)))
    (save-excursion
      ;; maybe recur into subsequent items:
      (unless (equal "Nothing further" ; stop at last item
                     (lem-next-item :no-refresh))
        (let ((indent (lem-ui--current-indent)))
          (when (> indent top-indent)
            (lem-ui-comment-tree-fold invis-after top-indent)))))))

(defun lem-ui--fold-all-comments (buf)
  "Fold all comments in current buffer.
BUF is the buffer to fold in."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (while (not (equal "Nothing further" ; stop at last item
                         (lem-next-item :no-refresh)))
        (unless (eq t (get-text-property (point) 'folded))
          (lem-ui-comment-tree-fold))))))

(defun lem-ui-fold-current-branch (&optional buf)
  "Toggle folding the branch of comment at point.
Optionally ensure buffer BUF is current."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (save-excursion
      (lem-ui-branch-top-level)
      (lem-ui-comment-tree-fold))))

;;; LIKES / VOTES

;; TODO: check for liked status before changing it
(defun lem-ui-like-item (&optional type)
  "Like (upvote) item at point.
TYPE should be either :unlike, :dislike, or nil to like."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((item (lem-ui--property 'lem-type))
           (fun (if (eq item 'post)
                    #'lem-like-post
                  #'lem-like-comment))
           (score (cond ((eq type :unlike) 0)
                        ((eq type :dislike) -1)
                        (t 1)))
           (like-str (cond ((eq type :unlike) "unliked")
                           ((eq type :dislike) "disliked")
                           (t "liked"))))
      (when (or (eq item 'post)
                (eq item 'comment)
                (eq item 'comment-reply))
        (let* ((vote (funcall fun id score)) ; let-alist this junk:
               (item (if (eq item 'comment-reply)
                         'comment
                       item))
               (obj (lem-ui-item-to-alist-key item))
               (i (alist-get obj vote))
               (saved (alist-get 'saved i))
               (my-vote (alist-get 'my_vote i)))
          (lem-ui-response-msg i ; no my_vote if we unliked in 0.19?
                               item :non-nil
                               (format "%s %s %s!" item id like-str))
          (lem-ui--update-item-json i)
          (lem-ui-update-item-from-json
           'byline-bottom
           (lambda (json)
             (lem-ui-bt-byline-replace json my-vote saved))))))
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
  (lem-ui-with-item 'all
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

;;; USERS

(defun lem-ui-render-users (json &optional search)
  "Render JSON, a list of users.
SEARCH means we are rendering a search result."
  (cl-loop for user in json
           do (progn (lem-ui-render-user user search)
                     (insert "\n"))))

(defun lem-ui--format-moderates (community)
  "Format COMMUNITY as a link."
  (let-alist community
    (concat
     (lem-ui--format-community-as-link .community.title
                                       .community.id
                                       .community.actor_id)
     " ")))

(defun lem-ui-render-user (json &optional search)
  "Render user with data JSON.
SEARCH means we are rendering a search result."
  (let-alist (if search json (alist-get 'person_view json))
    (insert
     (propertize
      (concat
       (propertize (concat
                    ;; top byline:
                    ;; name:
                    (propertize (or .person.display_name
                                    .person.name)
                                'face '(:weight bold))
                    " "
                    ;; admin box:
                    (when (eq t .is_admin)
                      (concat
                       (lem-ui-propertize-admin-box)
                       " "))
                    ;; handle
                    (propertize
                     (lem-ui--handle-from-user-url .person.actor_id)
                     'face 'font-lock-comment-face))
                   'byline-top t) ; for prev/next cmds
       ;; bio:
       (if .person.bio
           (concat "\n"
                   (lem-ui-render-body .person.bio))
         "\n")
       ;; mods:
       (when-let ((mods (alist-get 'moderates json)))
         ;; needs wrapping or filling, maybe we `visual-line-mode' after all:
         (concat "mods: "
                 (cl-loop for c in mods
                          concat (lem-ui--format-moderates c))
                 "\n"))
       ;; stats:
       (lem-ui-symbol 'direct) " " ; FIXME: we need a post symbol
       (number-to-string .counts.post_count) " | "
       (lem-ui-symbol 'reply) " "
       (number-to-string .counts.comment_count)
       " | "
       ;; join date
       "joined: "
       (fedi--relative-time-description
        (date-to-time .person.published))
       "\n"
       lem-ui-horiz-bar
       "\n")
      'json json
      'id .person.id
      'lem-type 'user))))

(defun lem-ui-render-user-subscriptions (json)
  "Render subscribed communities from JSON data."
  (cl-loop for community in json
           do (lem-ui-render-community community nil nil :subscription)))

(defun lem-ui-ts-to-secs (ts)
  "Return TS, a timestamp, as seconds since the epoch, an integer."
  (let ((lisp-ts (date-to-time ts)))
    (string-to-number
     (format-time-string "%s" lisp-ts))))

(defun lem-ui--get-item-published (item)
  "Return published timestamp of ITEM, either comment or post."
  (let-alist item
    ;; comments also have post data so comment first
    (or .private_message.published
        .comment_reply.published
        .comment.published
        .post.published)))

(defun lem-ui-published-sort-predicate (x y)
  "Predicate function for `sort'.
Decide whether X comes before Y, based on timestamp."
  (let ((pub1 (lem-ui-ts-to-secs
               (lem-ui--get-item-published x)))
        (pub2 (lem-ui-ts-to-secs
               (lem-ui--get-item-published y))))
    (> pub1 pub2)))

(defun lem-ui-render-overview (user-json)
  "Return an overview of mixed posts and comments from USER-JSON."
  (let-alist user-json
    ;; FIXME: we need to respect sort type when doing our combining here!
    (let* ((merged (append .comments .posts))
           (sorted (sort merged #'lem-ui-published-sort-predicate)))
      (cl-loop for item in sorted
               do (let ((type (caar item))
                        (reply-p (eq item 'comment-reply)))
                    (if (eq type 'post)
                        (lem-ui-render-post item :community :trim)
                      (lem-ui-render-comment item reply-p :details)))))))

(defun lem-ui-view-user (id &optional item sort limit)
  "View user with ID.
ITEM must be a member of `lem-user-items-types'.
SORT must be a member of `lem-sort-types' or if item is
\"comments\", then a member of `lem-comment-sort-types'.
LIMIT is max items to show.
CURRENT-USER means we are displaying the current user's profile."
  (let* ((sort (if (lem-user-view-sort-type-p sort)
                   sort
                 (lem-ui-view-default-sort 'user)))
         (user-json (lem-api-get-person-by-id id sort limit))
         ;; `lem-ui-view-default-sort' should take care of this now?
         ;; (sort (cond ((equal item "comments")
         ;;              (if (lem-comment-sort-type-p sort)
         ;;                  sort
         ;;                lem-default-comment-sort-type))
         ;;             (t
         ;;              (or sort
         ;;                  lem-default-sort-type))))
         (buf "*lem-user*")
         (bindings (lem-ui-view-options 'user)))
    (lem-ui-with-buffer buf 'lem-mode nil bindings
      ;; we have this on the 's' binding now so no need:
      (let-alist user-json
        (lem-ui-render-user user-json)
        (lem-ui-set-buffer-spec
         nil ; no listing type for users
         sort (if (eq id lem-user-id)
                  #'lem-ui-view-own-profile
                #'lem-ui-view-user)
         (or item "overview"))
        (lem-ui-widgets-create `("Sort" ,sort))
        (cond ((equal item "posts")
               (lem-ui-insert-heading "posts")
               (lem-ui-render-posts .posts :community :trim))
              ((equal item "comments")
               (lem-ui-insert-heading "comments")
               (lem-ui-render-comments .comments :details))
              (t ; no arg: overview
               (lem-ui-insert-heading "overview")
               (lem-ui-render-overview user-json)))
        (lem-ui--init-view)))))

(defun lem-ui-view-own-profile ()
  "View profile of the current user."
  (interactive)
  (lem-ui-view-user lem-user-id))

(defun lem-ui-view-item-user ()
  "View user of item at point."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((type (lem-ui--item-type))
           (id (cond ((or (eq type 'user)
                          (eq type 'person))
                      (lem-ui--property 'id))
                     ((or (eq type 'post)
                          (eq type 'comment)
                          (eq type 'comment-reply)
                          (eq type 'private-message))
                      (lem-ui--property 'creator-id))
                     (t
                      (user-error "Item has no user?")))))
      (lem-ui-view-user id "overview"))))

(defun lem-ui-message-user-at-point ()
  "Send private message to user at point."
  (interactive)
  (lem-ui-with-item 'all
    (let ((message (read-string "Private message: ")))
      (lem-send-private-message message id))))

(defun lem-ui-block-user ()
  "Block author of item at point."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((id (lem-ui--property 'creator-id))
           (json (lem-ui--property 'json))
           (name (alist-get 'name
                            (alist-get 'creator json))))
      (if (not name)
          (user-error "Looks like no user at point?")
        (when (y-or-n-p (format "Block %s?" name))
          (lem-ui-response-msg
           (lem-block-user id t)
           'blocked 't
           (format "User %s blocked!" name)))))))

(defun lem-ui-unblock-user ()
  "Prompt for a blocked user, and unblock them."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-blocked-users
   #'lem-ui--blocks-list
   "Unlbock user: "
   (lambda (id choice)
     (lem-ui-response-msg
      (lem-block-user id :json-false)
      'blocked :json-false
      (format "User %s unblocked!" choice)))))

;;; IMAGES

(defun lem-ui-insert-images ()
  "Insert any image-url images in the buffer with `shr-insert-image'.
It's a cheap hack, alas."
  (save-excursion
    (goto-char (point-min))
    (let (match
          (url-user-agent lem-user-agent))
      (while (setq match (text-property-search-forward 'image-url))
        (let ((beg (prop-match-beginning match))
              (end (prop-match-end match)))
          (goto-char beg)
          (lem-shr-insert-image beg end)
          (goto-char end))))))

(defun lem-shr-insert-image (start end)
  "Insert the image under point into the buffer.
START and END mark the region to replace."
  (interactive) ; cmd (bound to i) if images optional
  (let ((url (get-text-property (point) 'image-url))
        (shr-max-image-proportion 0.4 ))
    (if (not url)
	(message "No image under point")
      ;; (message "Inserting %s..." url) ; shut up shr.el!
      (with-demoted-errors "Error: %s"
        ;; in case of bad URL, e.g. relative link
        (url-retrieve url #'shr-image-fetched
		      (list (current-buffer)
                            start end) ; don't assume we are replacing "*"
                      `(:max-width 400 :max-height 400) ; max size
                      t)))))

(defun lem-ui-copy-item-url ()
  "Copy the URL (ap_id) of the post or comment at point."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((json (lem-ui--property 'json))
           (item (or (alist-get 'comment json)
                     (alist-get 'post json)))
           (url (alist-get 'ap_id item)))
      (if item
          (progn (kill-new url)
                 (message "url %s copied!" url))))))

(defun lem-ui-print-json ()
  "Fetch the JSON of item at point and pretty print it in a new buffer."
  (interactive)
  (let ((json (lem-ui-with-item 'all
                (lem-ui--property 'json)))
        (buf (get-buffer-create "*lem-json*")))
    (with-current-buffer buf
      (erase-buffer)
      (emacs-lisp-mode)
      (insert (prin1-to-string json))
      (pp-buffer)
      (goto-char (point-min))
      (switch-to-buffer-other-window buf))))

(provide 'lem-ui)
;;; lem-ui.el ends here
