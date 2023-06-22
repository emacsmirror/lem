;;; lem-ui.el --- Basics for a lemmy interface -*- lexical-binding: t; -*-

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

;; Some simple, unadorned, primitive, humble, basic, dashed-off functions for an interface to Lemmy, the federated link-aggregator and forum software. See <https://joinlemmy.org>.

;;; Code:

(require 'lem)

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
  ""
  (propertize str
              'face font-lock-comment-face))

(defun lem-ui-thing-json ()
  ""
  ;; FIXME up scotty, also just use 'json always then doesn't matter.
  (or
   (get-text-property (point) 'comment-json)
   (get-text-property (point) 'post-json)))

(defun lem-ui-id-from-json (slot json &optional string)
  "Return id as a string, from sub SLOT in JSON.
SLOT is a symbol, either 'post or 'comment."
  ;; FIXME up scotty
  (let ((num (alist-get 'id
                        (alist-get slot json))))
    (if string
        (number-to-string num)
      num)))

;;; ENTRYPOINT
(defun lem ()
  "Open lem, a Lemmy client.
Load current user's instance posts."
  (interactive)
  (unless lem-auth-token
    (lem-login-set-token))
  (lem-ui-view-instance "Top" "All")) ; add customize defaults

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

(defun lem-ui-set-buffer-spec (sort type) ; endpoint etc.
  "Set `lem-ui-buffer-spec' for the current buffer.
TYPE is the Lemmy ListingType, one of \"All\" \"Community\"
\"Local\" or \"Subscribed\"."
  (setq lem-ui-buffer-spec
        `(:sort ,sort
                :type ,type)))

;;; INSTANCE

;; TODO: toggle posts or comments, and cycle Local, All, or Subscribed
(defun lem-ui-view-instance (&optional sort type) ; limit
  "View posts of current user's home instance.
SORT can be \"New\", \"Hot\", \"Old\", or \"Top\".
TYPE is one of \"All\" \"Community\" \"Local\" or
\"Subscribed\"."
  (let ((posts (lem-get-instance-posts nil type))) ; no sort here, its below
    (lem-ui-with-buffer (get-buffer-create"*lem*") 'special-mode nil
      (lem-ui-render-posts posts nil sort)
      (lem-ui-set-buffer-spec sort type)))) ; no children

;; TODO refactor to also handle sort:
(defun lem-ui-cycle-instance-view-type ()
  ""
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
  ""
  ;; TODO list subscriptions. Not in API yet? get-person-by-name doesn't contain
  )

(defun lem-ui-search ()
  ""
  ;; TODO: interactive search functionality, discover stuff from home view.
  )

;;; POSTS

(defun lem-ui-view-post-at-point ()
  ""
  (interactive)
  (let* ((post (lem-ui-thing-json))
         (id (lem-ui-id-from-json 'post post :string)))
    (lem-ui-view-post id)))

(defun lem-ui-view-post (id &optional sort limit)
  ""
  (let* ((post-view (lem-get-post id))
         (post (alist-get 'post_view post-view)))
    (lem-ui-with-buffer (get-buffer-create"*lem-post*") 'special-mode t
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
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\"."
  (let-alist post
    ;; (lem-ui-with-buffer (get-buffer-create"*lem*") 'special-mode t
    ;; (with-current-buffer (get-buffer-create "*lem*")
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
      (let* ((id (number-to-string .post.id))
             (comms (lem-get-post-comments id nil sort))
             (list (alist-get 'comments comms)))
        (mapc (lambda (x)
                (lem-ui-render-comment x :children sort))
              list)))))
;; (unless (equal (buffer-name (current-buffer)) "*lem*")
;;   (switch-to-buffer-other-window "*lem*")
;;   (goto-char (point-min)))))

(defun lem-ui-render-posts (posts &optional children sort)
  "Render a list of abbreviated posts POSTS.
Used for communities posts or instance posts."
  (let ((list (alist-get 'posts posts)))
    ;; (lem-ui-with-buffer "*lem*" 'special-mode t
    ;; (lem-ui-render-post (car list)))))
    (with-current-buffer (get-buffer-create "*lem*")
      (mapc (lambda (x)
              (lem-ui-render-post x children sort))
            list)
      (goto-char (point-min)))))

;;; COMMUNITIES

(defun lem-ui-view-community (name &optional sort limit)
  "View community with NAME, sorting by SORT.
SORT can be \"New\", \"Hot\", \"Old\", or \"Top\"."
  (interactive)
  (let* ((community (lem-get-community-by-name name))
         (id (lem-ui-get-community-id community :string))
         (posts (lem-list-posts id nil limit))) ; no sorting
    (lem-ui-with-buffer (get-buffer-create"*lem*") 'special-mode t
      (lem-ui-render-community-header community)
      (lem-ui-render-posts posts nil sort)))) ; no children, ie comments


(defun lem-ui-get-community-id (community &optional string)
  "Returns ID of COMMUNITY as a string."
  (let ((id
         (alist-get 'id
                    (alist-get 'community
                               (alist-get 'community_view community)))))
    (if string
        (number-to-string id)
      id)))

(defun lem-ui-render-community-header (community)
  ""
  (with-current-buffer (get-buffer-create "*lem*")
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
              "\n"))))

;;; REPLIES

(defun lem-ui-reply-simple ()
  "Reply to post or comment at point
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
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\"."
  (let-alist comment
    ;; (lem-ui-with-buffer "*lem*" 'special-mode t
    ;; (with-current-buffer (get-buffer-create "*lem*")
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
      (let* ((comms (setq lem-post-comments
                          (lem-get-post-comments (number-to-string .post.id)
                                                 (number-to-string .comment.id)
                                                 sort)))
             (list (setq lem-post-comments-list
                         (alist-get 'comments comms))))
        ;; FIXME: comment children recursion is broken:
        ;; (mapc (lambda (x)
        ;;         (lem-ui-render-comment x :children
        ;;                                ;; nil
        ;;                                sort))
        (cdr list)))))
;;)

;; (setq lem-post-comments (lem-get-post-comments "1235982" "651145" "New"))
;; (setq lem-post-comments (lem-get-post-comments "1235982" nil "New"))

(provide 'lem-ui)
;;; lem-ui.el ends here
