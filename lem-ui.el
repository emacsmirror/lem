;;; lem-ui.el --- Basics for a lemmy interface -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
;; URL: https://codeberg.org/martianh/lem
;; Package-Requires: ((emacs "27.1") (fedi "0.1"))
;; Keywords: multimedia, comm, web, fediverse

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

(require 'lem-request)

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
  "Get json of thing at point, comment, or post."
  ;; FIXME up scotty, also just use 'json always then doesn't matter.
  (or
   (get-text-property (point) 'comment-json)
   (get-text-property (point) 'post-json)))

(defun lem-ui-id-from-json (slot json &optional string)
  "Return id as a string, from sub SLOT in JSON.
SLOT is a symbol, either 'post or 'comment.
STRING means return as string, else return number."
  ;; FIXME up scotty
  (let ((num (alist-get 'id
                        (alist-get slot json))))
    (if string
        (number-to-string num)
      num)))

;;; MACRO
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

;;; BUFFER DETAILS
(defvar-local lem-ui-buffer-spec nil
  "A plist containing details about the current lem buffer.")

(defun lem-ui-set-buffer-spec (sort listing-type) ; endpoint etc.
  "Set `lem-ui-buffer-spec' for the current buffer.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'."
  (setq lem-ui-buffer-spec
        `(:sort ,sort :listing-type ,listing-type)))

;;; INSTANCE

;; TODO: toggle posts or comments, and cycle Local, All, or Subscribed
(defun lem-ui-view-instance (&optional sort type limit)
  "View posts of current user's home instance.
SORT must be a member of `lem-sort-types'.
LISTING-TYPE must be member of `lem-listing-types'.
LIMIT is the amount of results to return."
  (let ((posts (lem-get-instance-posts type nil limit))) ; no sort here, its below
    (lem-ui-with-buffer (get-buffer-create"*lem*") 'lem-mode nil
      (lem-ui-render-posts posts nil sort)
      (lem-ui-set-buffer-spec sort type)))) ; no children

;; TODO refactor to also handle sort:
(defun lem-ui-cycle-instance-view-type ()
  "Cycle instance view between view listing types."
  (interactive)
  (let ((type (plist-get lem-ui-buffer-spec :type))
        (sort (plist-get lem-ui-buffer-spec :sort)))
    (cond ((equal type "All")
           (lem-ui-view-instance sort "Local"))
          ((equal type "Local")
           (lem-ui-view-instance sort "Subscribed"))
          ((equal type "Subscribed")
           (lem-ui-view-instance sort "All")))))

(defun lem-ui-list-subscriptions ()
  "."
  ;; TODO list subscriptions. Not in API yet? get-person-by-name doesn't contain
  )

(defun lem-ui-search ()
  "."
  ;; TODO: interactive search functionality, discover stuff from home view.
  )

;;; POSTS

(defun lem-ui-view-post-at-point ()
  "."
  (interactive)
  (let* ((post (lem-ui-thing-json))
         (id (lem-ui-id-from-json 'post post :string)))
    (lem-ui-view-post id)))

(defun lem-ui-view-post (id &optional sort limit)
  "View post with ID.
SORT.
LIMIT."
  (let* ((post-view (lem-get-post id))
         (post (alist-get 'post_view post-view)))
    (lem-ui-with-buffer (get-buffer-create"*lem-post*") 'lem-mode t
      (lem-ui-render-post post :children sort)
      (goto-char (point-min))))) ; limit

(defun lem-ui-top-byline (name score timestamp)
  "Format a top byline for post with NAME, SCORE and TIMESTAMP."
  ;; TODO: name link to user page, etc.
  (propertize (concat
               name " | "
               (lem-ui-symbol 'favourite)
               (number-to-string score) " | "
               timestamp)
              'face font-lock-comment-face))

(defun lem-ui-bt-byline (comments &optional id)
  "Format a bottom byline for a post or comment.
COMMENTS is the comments count to render.
ID is the item's id."
  (format "%s %s | %s" (lem-ui-symbol 'reply)
          (number-to-string comments)
          (propertize (concat "id: "
                              (number-to-string id))
                      'face font-lock-comment-face)))

(defun lem-ui-render-post (post &optional children sort)
  "Render single POST.
Optionally render its CHILDREN.
SORT must be a member of `lem-sort-types'."
  (let-alist post
    (insert
     (propertize
      (concat
       "\n"
       (lem-ui-top-byline .creator.name
                          .counts.score
                          .post.published)
       "\n"
       (propertize .post.name
                   'face '(:weight bold))
       "\n"
       (if .post.url
           (propertize .post.url
                       'face '(:underline t))
         "")
       (or .post.body "") ;; cap it for list views
       "\n"
       (lem-ui-bt-byline .counts.comments .post.id)
       "\n"
       ;; properties to add:
       ;; (number-to-string .post.id) "\n"
       ;; (number-to-string .post.creator_id) "\n"
       ;; (number-to-string .post.community_id) "\n"
       lem-ui-horiz-bar
       "\n")
      'post-json post))
    (when (and children
               (< 0 .counts.comments))
      (lem-ui-render-children .post.id sort))))
;; (unless (equal (buffer-name (current-buffer)) "*lem*")
;;   (switch-to-buffer-other-window "*lem*")
;;   (goto-char (point-min)))))

(defun lem-ui-render-children (id sort)
  "ID SORT."
  (let* ((id (number-to-string id))
         (comments (lem-get-post-comments id nil sort))
         (list (alist-get 'comments comments)))
    (cl-loop for x in list
             do (lem-ui-render-comment x :children sort))))

(defun lem-ui-render-posts (posts &optional children sort)
  "Render a list of abbreviated posts POSTS.
Used for communities posts or instance posts.
CHILDREN means also show post comments.
SORT is the kind of sorting to use."
  (let ((list (alist-get 'posts posts)))
    (with-current-buffer (get-buffer-create "*lem*")
      (cl-loop for x in list
               do (lem-ui-render-post x children sort))
      (goto-char (point-min)))))

;;; COMMUNITIES
(defun lem-ui-render-subscribed-communities ()
  "Render the communities subscribed to by the logged in user."
  (interactive)
  (let* ((json (lem-list-communities "Subscribed"))
         (list (alist-get 'communities json))
         (buffer "*lem-subscribed-communities*"))
    (lem-ui-with-buffer (get-buffer-create buffer) 'lem-mode t
      (cl-loop for c in list
               for id = (alist-get 'id (alist-get 'community c))
               for view = (lem-get-community-by-id (number-to-string id))
               do (lem-ui-render-community-header view buffer))
      (goto-char (point-min)))))

(defun lem-ui-view-community (name &optional sort limit)
  "View community with NAME, sorting by SORT.
SORT can be \"New\", \"Hot\", \"Old\", or \"Top\".
LIMIT is the max results to show."
  (interactive)
  (let* ((community (lem-get-community-by-name name))
         (id (lem-ui-get-community-id community :string))
         (posts (lem-list-posts id nil limit))) ; no sorting
    (lem-ui-with-buffer (get-buffer-create"*lem*") 'lem-mode t
      (lem-ui-render-community-header community)
      (lem-ui-render-posts posts nil sort)))) ; no children, ie comments


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

;; TODO: make this generic, for instance and post also:
(defun lem-ui-render-community-header (community &optional buffer)
  "Render header details for COMMUNITY.
BUFFER is the one to render in, a string."
  (with-current-buffer (get-buffer-create (or buffer "*lem*"))
    (let-alist (alist-get 'community_view community)
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
        'community-json community))
      ;; .community.id

      ;; .counts.subscribers
      ;; .counts.posts
      ;; .counts.comments
      ;; ;; ...
      )
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
              "\n\n"))))

;;; REPLIES

(defun lem-ui-reply-simple ()
  "Reply to post or comment at point.
Simple means we just read a string."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (post-id (if (alist-get 'post json)
                      (lem-ui-id-from-json 'post json)
                    (lem-ui-id-from-json 'post-id json)))
         (parent-id (when-let ((comment (alist-get 'comment json)))
                      (alist-get 'id comment)))
         (content (read-string "Reply: "))
         (response (lem-create-comment post-id content parent-id)))
    (when response
      (let-alist response
        (message "Comment created: %s" .comment_view.comment.content)))))

;;; COMMENTS

(defun lem-ui-render-comment (comment &optional children sort)
  "Render single COMMENT.
Optionally render its CHILDREN.
SORT can be \"New\", \"Hot\", \"Old\", or \"Top\"."
  (let-alist comment
    (insert
     (propertize
      (concat
       "\n"
       (lem-ui-top-byline .creator.name
                          .counts.score
                          .comment.published)
       "\n"
       (or .comment.content "")
       "\n"
       (lem-ui-bt-byline .counts.child_count .comment.id)
       "\n"
       ;; properties to add:
       ;; (number-to-string .post.id) "\n"
       ;; (number-to-string .comment.creator_id) "\n"
       ;; (number-to-string .post.community_id) "\n"
       lem-ui-horiz-bar
       "\n")
      'comment-json comment))
    (when (and children
               (< 0 .counts.child_count))
      (let* ((comments
              (setq lem-post-comments
                    (lem-get-comment-children
                     ;; (lem-get-post (number-to-string .post.id)
                     (number-to-string .comment.id)
                     nil sort)))
             (list (setq lem-post-comments-list
                         (alist-get 'comments comments))))
        ;; FIXME: comment children recursion is broken:
        (cl-loop for x in (setq lem-cmt-cdr-list (cdr list))
                 (lem-ui-render-comment x :children
                                        ;; nil
                                        sort))))))
;;)


;; redundant but trying to work threading out.
(defun lem-ui-get-comment-children-at-point (&optional type sort limit)
  "TYPE.
SORT
LIMIT."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (id (lem-ui-id-from-json 'comment json :string))
         (children
          (setq lem-test-children
                (alist-get 'comments
                           (lem-get-comment-children id type sort limit)))))
    (lem-ui-with-buffer (get-buffer-create "*lem-post*") 'lem-mode nil
      ;; (with-current-buffer (get-buffer-create "*lem-post*")
      (cl-loop for child in children
               do (lem-ui-render-comment child nil sort)))))

;; (setq lem-post-comments (lem-get-post-comments "1235982" "651145" "New"))
;; (setq lem-post-comments (lem-get-post-comments "1235982" nil "New"))

(provide 'lem-ui)
;;; lem-ui.el ends here
