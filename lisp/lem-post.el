;;; lem-post.el --- Posting for lem.el -*- lexical-binding: t; -*-

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

;;; Code:
(require 'fedi-post)
(require 'lem-api)
(require 'lem-ui)

(autoload 'ffap-url-p "ffap")

(defalias 'lem-post-cancel #'fedi-post-cancel)
(defalias 'lem-post-toggle-nsfw #'fedi-post-toggle-nsfw)
(defalias 'lem-post-set-post-language #'fedi-post-set-post-language)

(defvar lem-user-id)

(defvar-local lem-post-item-type nil)

(defvar-local lem-post-title nil)
(defvar-local lem-post-url nil)
(defvar-local lem-post-community-id nil)
(defvar-local lem-post-community-name nil)
(defvar-local lem-post-edit-id nil)
(defvar-local lem-post-comment-edit-id nil)

(defvar-local lem-post-comment-post-id nil)
(defvar-local lem-post-comment-comment-id nil)

(defvar-local lem-post-recipient-id nil)

(defvar lem-post-last-buffer nil)

(defvar-local lem-post-community-title nil)
(defvar-local lem-post-community-name nil)
(defvar-local lem-post-community-restricted-to-mods nil)

(defgroup lem-post
  nil
  "Posting for lem.el."
  :prefix "lem-post-"
  :group 'lem)

(defface lem-post-community-face
  '((t :inherit success))
  "Face used for community status field.")

(defface lem-post-title-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for post title in compose buffer.")

(defvar lem-post-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inheriting doesn't work for our post docs display
    ;; (set-keymap-parent map fedi-post-mode-map)
    (define-key map (kbd "C-c C-k") #'lem-post-cancel)
    (define-key map (kbd "C-c C-n") #'lem-post-toggle-nsfw)
    ;; (when (require 'emojify nil :noerror)
    ;; (define-key map (kbd "C-c C-e") #'lem-post-insert-emoji))
    (define-key map (kbd "C-c C-l") #'lem-post-set-post-language)
    (define-key map (kbd "C-c C-o") #'lem-post-select-community)
    (define-key map (kbd "C-c C-t") #'lem-post-read-title)
    (define-key map (kbd "C-c C-u") #'lem-post-read-url)
    (define-key map (kbd "C-c C-c") #'lem-post-submit)
    ;; (define-key map (kbd "C-c C-l") #'fedi-post-set-post-language)
    map)
  "Keymap for `lem-post-mode'.")

(defvar lem-post-comment-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'lem-post-cancel)
    (define-key map (kbd "C-c C-n") #'lem-post-toggle-nsfw)
    ;; (when (require 'emojify nil :noerror)
    ;; (define-key map (kbd "C-c C-e") #'lem-post-insert-emoji))
    (define-key map (kbd "C-c C-l") #'lem-post-set-post-language)
    (define-key map (kbd "C-c C-c") #'lem-post-submit)
    map)
  "Keymap for `lem-post-comment-mode'.")

(defvar lem-post-create-community-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'lem-post-cancel)
    (define-key map (kbd "C-c C-n") #'lem-post-toggle-nsfw)
    (define-key map (kbd "C-c C-t") #'lem-post-read-community-display-name)
    (define-key map (kbd "C-c C-m") #'lem-post-read-community-name)
    (define-key map (kbd "C-c C-r") #'lem-post-toggle-restricted-to-mods)
    (define-key map (kbd "C-c C-c") #'lem-post-submit)
    map)
  "Keymap for `lem-post-create-community-mode'.")

;;; COMPOSING POSTS

(defun lem-post-read-title ()
  "Read post title."
  (interactive)
  (setq lem-post-title
        (read-string "Post title: "
                     lem-post-title))
  (fedi-post--update-status-fields))

(defun lem-post-read-url ()
  "Read post URL."
  (interactive)
  (setq lem-post-url
        (read-string "Post URL: "
                     (or lem-post-url "https://")))
  (unless (ffap-url-p lem-post-url)
    (when (y-or-n-p "URL doesn't look too good. Try again?")
      (lem-post-read-url)))
  (fedi-post--update-status-fields))

(defun lem-post-select-community ()
  "Select community to post to."
  (interactive)
  (lem-ui-do-item-completing
   #'lem-api-get-subscribed-communities
   #'lem-ui--communities-list
   "Post to community: "
   (lambda (id choice)
     (setq lem-post-community-name choice)
     (setq lem-post-community-id id)
     (message "Posting to %s" choice)))
  (fedi-post--update-status-fields))

(defun lem-post-compose (&optional edit mode type)
  "Compose a new post.
EDIT means we are editing.
MODE is the lem.el minor mode to enable in the compose buffer.
TYPE is a symbol of what we are composing, it may be comment or
message."
  (interactive)
  (setq lem-post-last-buffer (buffer-name (current-buffer)))
  (fedi-post--compose-buffer edit
                             #'markdown-mode
                             (or mode #'lem-post-mode)
                             (when mode "lem-post")
                             (or type 'post)
                             (list #'lem-post--mentions-capf
                                   #'lem-post--comms-capf)
                             (unless type ; post
                               '(((name . "title")
                                  (no-label . t)
                                  (prop . post-title)
                                  (item-var . lem-post-title)
                                  (face . lem-post-title-face))
                                 ((name . "URL")
                                  (no-label . t)
                                  (prop . post-url)
                                  (item-var . lem-post-url)
                                  (face . link))
                                 ((name . "community")
                                  (no-label . t)
                                  (prop . post-community)
                                  (item-var . lem-post-community-name)
                                  (face . lem-post-community-face)))))
  (setq lem-post-item-type 'post))

(defun lem-post-compose-simple ()
  "Create and submit new post, reading strings in the minibuffer."
  (interactive)
  (let* ((name (read-string "Post title: "))
         (communities (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-list
                (alist-get 'communities communities)))
         (choice (completing-read "Community: " ; TODO: default to current view
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

;;; RESPONSE FUNCTIONS

(defun lem-ui-edit-comment-response (response)
  "Call response functions upon editing a comment.
RESPONSE is the comment_view data returned by the server."
  (with-current-buffer lem-post-last-buffer
    (let-alist response
      (let ((indent (length (lem-ui--property 'line-prefix)))
            (view (lem-ui-view-type)))
        (lem-ui-response-msg
         response 'comment_view :non-nil
         (format "Comment edited: %s" .comment_view.comment.content))
        (lem-ui--update-item-json .comment_view)
        (lem-ui-update-item-from-json
         'lem-type
         (lambda (_response)
           (lem-ui-format-comment .comment_view indent
                                  nil (unless (or (eq view 'post)
                                                  (eq view 'community))
                                        :details))))))))

(defun lem-ui-create-comment-response (response)
  "Call response functions upon editing a comment.
RESPONSE is the comment_view data returned by the server."
  (with-current-buffer lem-post-last-buffer
    (let-alist response
      (lem-ui-response-msg
       response 'comment_view :non-nil
       (format "Comment created: %s" .comment_view.comment.content))
      (lem-ui--update-item-json .comment_view)
      ;; its not clear if we should always dump new comment right after its
      ;; parent, or somewhere else in the tree.
      ;; just reload!
      ;; (lem-ui-insert-comment-after-parent response) ; parent-id)
      (lem-ui-reload-view)
      (when (eq (lem-ui-view-type) 'post)
        (lem-prev-item)))))

;;; SUBMITTING ITEMS

(defun lem-post-submit ()
  "Submit the post, comment, or community to lemmy.
Call response and update functions."
  (interactive)
  (let ((buf (buffer-name))
        ;; (parent-id lem-post-comment-comment-id)
        (type (cond (lem-post-comment-post-id 'new-comment)
                    (lem-post-comment-edit-id 'edit-comment)
                    (lem-post-edit-id 'edit-post)
                    (lem-post-recipient-id 'message)
                    ((eq lem-post-item-type 'community) 'community)
                    (t 'new-post))))
    (if (and (or (eq type 'new-post)
                 (eq type 'edit-post))
             (not (and lem-post-title
                       lem-post-community-id)))
        (message "You need to set at least a post name and community.")
      (let* ((body (fedi-post--remove-docs))
             (response
              (cond
               ((eq 'new-comment type)
                (lem-create-comment lem-post-comment-post-id
                                    body
                                    lem-post-comment-comment-id))
               ((eq 'edit-comment type)
                (lem-edit-comment lem-post-comment-edit-id body))
               ((eq 'edit-post type)
                (lem-edit-post lem-post-edit-id lem-post-title body
                               lem-post-url fedi-post-content-nsfw
                               fedi-post-language))
               ((eq 'message type)
                (lem-send-private-message body lem-post-recipient-id))
               ((eq 'community type)
                (lem-create-community lem-post-community-name
                                      lem-post-community-title
                                      nil ; banner
                                      body ; description
                                      nil ; langs
                                      nil ; icon
                                      fedi-post-content-nsfw
                                      lem-post-community-restricted-to-mods))
               (t ;; creating a post
                (lem-create-post lem-post-title lem-post-community-id body
                                 lem-post-url fedi-post-content-nsfw
                                 nil fedi-post-language))))) ; TODO: honeypot
        (when response
          (with-current-buffer buf
            (fedi-post-kill))
          (let-alist response
            (cond
             ((eq type 'new-comment)
              ;; after new comment: insert it into post view tree:
              (lem-ui-create-comment-response response)) ; parent-id))
             ((eq type 'edit-comment)
              ;; after edit comment: replace with updated item:
              (lem-ui-edit-comment-response response))
             ((eq type 'edit-post)
              ;; after edit post: reload previous view:
              (lem-ui-response-msg
               response 'post_view :non-nil
               (format "Post %s edited!" .post_view.post.name))
              (lem-ui-reload-view))
             ((eq type 'message)
              (lem-ui-response-msg
               response 'private_message_view :non-nil
               (format "Private message sent to %s!"
                       .private_message_view.recipient.name)))
             ((eq type 'community)
              ;; after create community: view community + message?
              (lem-ui-response-msg
               response 'community_view :non-nil
               (format "Community %s created!" .community_view.community.name))
              (lem-ui-view-community .community_view.community.id))
             (t ;; creating a post
              ;; after new post: view the post
              (lem-ui-post-post-submit response)))))))))

(defun lem-ui-reload-parent-community-view (community-id)
  "If community with COMMUNITY-ID is in `buffer-list', reload it."
  (let ((community-id (number-to-string community-id)))
    (cl-loop for b in (buffer-list)
             when (string-suffix-p (concat "-" community-id "*")
                                   (buffer-name b))
             return (with-current-buffer b
                      (lem-ui-reload-view)))))

(defun lem-ui-post-post-submit (response)
  "Handle post-creation RESPONSE.
Display response message, view post, and update post's community
view buffer if present."
  (let-alist response
    (lem-ui-response-msg
     response 'post_view :non-nil
     (format "Post %s created!" .post_view.post.name))
    (lem-ui-reload-parent-community-view .post_view.post.community_id)
    (lem-ui-view-post .post_view.post.id)))

;;; POSTING COMMENTS

(defun lem-post-comment ()
  "Reply to a post or comment."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (post (alist-get 'post json)))
    (if (eq t (alist-get 'deleted post))
        (message "You can't comment on deleted posts.")
      (let* ((type (lem-ui--item-type))
             (post-id (if (equal type 'post)
                          (lem-ui--id-from-prop)
                        (lem-ui--id-from-json json 'post)))
             (comment-id (when (equal type 'comment)
                           (lem-ui--id-from-json json 'comment))))
        (lem-post-compose nil #'lem-post-comment-mode 'comment)
        (setq lem-post-item-type 'comment)
        (setq lem-post-comment-post-id post-id)
        (setq lem-post-comment-comment-id comment-id)))))

(defun lem-post-comment-simple ()
  "Reply to post or comment at point.
Simple means we just read a string."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (type (lem-ui--item-type))
         (content (read-string "Reply: "))
         (post-id (if (equal type 'post)
                      (lem-ui--id-from-prop)
                    (lem-ui--id-from-json json 'post)))
         (comment-id (when (equal type 'comment)
                       (lem-ui--id-from-json json 'comment)))
         (response (lem-create-comment post-id content comment-id)))
    (when response
      (let-alist response
        (message "Comment created: %s" .comment_view.comment.content)
        (lem-ui-view-post (number-to-string post-id))))))

;;; PRIVATE MESSAGES

(defun lem-post-private-message (&optional recipient-id)
  "Send a private message to a user.
Optionally, message user with RECIPIENT-ID."
  (interactive)
  (let* ((json (unless recipient-id
                 (save-excursion
                   (goto-char (point-min))
                   (lem-ui-thing-json))))
         (person (unless recipient-id (alist-get 'person json)))
         (id (or recipient-id (alist-get 'id person))))
    (lem-post-compose nil #'lem-post-comment-mode 'message)
    (setq lem-post-recipient-id id)))

(defun lem-post-item-author-private-message ()
  "Send a private message to the author of item at point."
  (interactive)
  (lem-ui-with-item 'all
    (let* ((item (lem-ui-thing-json))
           (obj (or (alist-get 'post item)
                    (alist-get 'comment item)))
           (recipient-id (alist-get 'creator_id obj)))
      (lem-post-private-message recipient-id))))

;;; EDITING POSTS/COMMENTS

(defun lem-post--set-post-properties
    (post-id community-name community-id title url
             &optional initial-text _post-lang)
  "Set the properties for the current edited post.
POST-ID is the posts id, a number.
COMMUNITY-NAME is a string.
COMMUNITY-ID is a number.
TITLE is a string.
URL is a string.
INITIAL-TEXT is the post's original text to inject into the buffer.
POST-LANG is the post's language (Not yet implemented)"
  (with-current-buffer "*edit post*"
    (setq lem-post-edit-id post-id)
    (setq lem-post-community-name community-name)
    (setq lem-post-community-id community-id)
    (setq lem-post-url url)
    (setq lem-post-title title)
    (fedi-post--update-status-fields)
    (when initial-text
      (insert initial-text))))

(defun lem-post-edit ()
  "Edit the post at point if possible."
  (interactive)
  (lem-ui-with-own-item 'post
    (let-alist (lem-ui--property 'json)
      (lem-post-compose :edit)
      (lem-post--set-post-properties .post.id
                                     .community.name .community.id
                                     .post.name .post.url
                                     .post.body))))

(defun lem-post--set-comment-properties (comment-id &optional
                                                    initial-text _post-lang)
  "Set the properties for the edited comment.
COMMENT-ID is its id, a number.
POST-LANG is its language.
INITIAL-TEXT is the item's original text to inject into the edit buffer."
  (with-current-buffer "*edit comment*"
    (setq lem-post-comment-edit-id comment-id)
    (fedi-post--update-status-fields)
    (when initial-text
      (insert initial-text))))

(defun lem-post-edit-comment ()
  "Edit comment at point if possible."
  (interactive)
  (lem-ui-with-own-item 'comment
    (let* ((id (lem-ui--property 'id))
           (json (lem-ui--property 'json))
           (old-str (alist-get 'content (alist-get 'comment json))))
      (lem-post-compose :edit #'lem-post-comment-mode 'comment)
      (lem-post--set-comment-properties id old-str))))

(defun lem-post-edit-post-or-comment ()
  "Try to edit item at point.
Should be either comment or post, and owned by the current user."
  (interactive)
  (if (eq 'comment (lem-ui--property 'lem-type))
      (lem-post-edit-comment)
    (lem-post-edit)))

;;; CREATING COMMUNITIES

(defun lem-post-read-community-display-name ()
  "Read community display name (title - can be changed later)."
  (interactive)
  (setq lem-post-community-title
        (read-string "Display name (can be changed): "
                     lem-post-community-title))
  (fedi-post--update-status-fields))

(defun lem-post-read-community-name ()
  "Read community name (identifier - cannot be changed later)."
  (interactive)
  (setq lem-post-community-name
        (read-string "Name (cannot be changed): "
                     lem-post-community-name))
  (fedi-post--update-status-fields))

(defun lem-post-toggle-restricted-to-mods ()
  "Toggle `lem-post-community-restricted-to-mods'."
  (interactive)
  (setq lem-post-community-restricted-to-mods
        (not lem-post-community-restricted-to-mods))
  (message "Posting restricted to mods is now %s"
           (if lem-post-community-restricted-to-mods "on" "off"))
  (fedi-post--update-status-fields))

(defun lem-post-create-community (&optional edit)
  "Create a new community.
EDIT means we are editing it.
MODE is the lem.el minor mode to enable in the compose buffer."
  ;; TODO: add icon/banner as per mastodon.el attachments
  (interactive)
  (setq lem-post-last-buffer (buffer-name (current-buffer)))
  (fedi-post--compose-buffer edit
                             #'markdown-mode
                             'lem-post-create-community-mode
                             "lem-post"
                             'community
                             (list #'lem-post--mentions-capf
                                   #'lem-post--comms-capf)
                             '(((name . "display-name")
                                (prop . post-display-name)
                                (item-var . lem-post-community-title)
                                (face . lem-post-title-face))
                               ((name . "name")
                                (prop . post-name)
                                (item-var . lem-post-community-name)
                                (face . lem-post-community-face))
                               ((name . "restricted")
                                (prop . post-restricted)
                                (item-var . lem-post-community-restricted-to-mods)
                                (face . lem-post-community-face))))
  (setq lem-post-item-type 'community))

;;; COMPLETION

(defun lem-post--items-alist (items type prefix)
  "Return an alist of ITEMS, of TYPE, with PREFIX, a string.
ITEMS is a list returned by lem-api-search-$item.
TYPE is a symbol, either community or person.
Prefix is either \"@\" or \"!\"."
  (cl-loop for i in items
           for it = (alist-get type i)
           for acid = (alist-get 'actor_id it)
           for url = (url-generic-parse-url acid)
           for domain = (url-domain url)
           collect (cons (concat prefix
                                 (alist-get 'name it)
                                 "@"
                                 domain)
                         acid)))

(defun lem-post--users-alist (users)
  "Return an alist of USERS, each element a cons of name and URL."
  (lem-post--items-alist users 'person "@"))

(defun lem-post-comms-alist (comms)
  "Return an alist of communities COMMS, each element a cons of name and URL."
  (lem-post--items-alist comms 'community "!"))

(defun lem-post--mentions-fun (start end)
  "Given prefix str between START and END, return an alist of mentions for capf."
  (let* ((query (lem-api-search-users
                 (buffer-substring-no-properties (1+ start) ; cull '@'
                                                 end)
                 nil nil "50")) ; max limit
         (users (alist-get 'users query)))
    (lem-post--users-alist users)))

(defun lem-post--comms-fun (start end)
  "Given prefix str between START and END, return a list of communities for capf."
  (let* ((query (lem-api-search-communities
                 (buffer-substring-no-properties (1+ start) ; cull '!'
                                                 end)
                 nil nil "50")) ; max limit
         (communities (alist-get 'communities query)))
    (lem-post-comms-alist communities)))

(defun lem-post--mentions-annot-fun (candidate)
  "Given a completion CANDIDATE, return its annotation."
  (concat " " (cdr (assoc candidate fedi-post-completions))))

;; (defun lem-post--mentions-affix-fun (cands)
;;   ""
;;   (cl-loop for c in cands
;;            for link = (cdr (assoc candidate fedi-post-completions))
;;            collect (list c
;;                          "["
;;                          (concat "](" link ")"))))

(defun lem-post--md-link-exit-fun (str _status)
  "Turn completion STR into a markdown link."
  (save-excursion
    (backward-char (length str))
    (insert "["))
  (insert "]("
          (cdr (assoc str fedi-post-completions))
          ")"))

(defun lem-post--mentions-capf ()
  "Build a mentions completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf fedi-post-handle-regex
                          #'lem-post--mentions-fun
                          #'lem-post--mentions-annot-fun
                          nil ; #'lem-post--mentions-affix-fun
                          #'lem-post--md-link-exit-fun))

(defun lem-post--comms-capf ()
  "Build a communities completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf lem-ui-community-regex
                          #'lem-post--comms-fun
                          #'lem-post--mentions-annot-fun
                          nil ; #'lem-post--mentions-affix-fun
                          #'lem-post--md-link-exit-fun))

;;; MINOR MODES

(define-minor-mode lem-post-mode
  "Minor mode for submitting posts to lemmy."
  :keymap lem-post-mode-map
  :global nil
  :after-hook ; disable auto-fill-mode:
  (lambda ()
    (auto-fill-mode -1)))

(define-minor-mode lem-post-comment-mode
  "Minor mode for submitting comments to lemmy."
  :keymap lem-post-comment-mode-map
  :global nil
  :after-hook ; disable auto-fill-mode:
  (lambda ()
    (auto-fill-mode -1)))

(define-minor-mode lem-post-create-community-mode
  "Minor mode for creating new communities on lemmy."
  :keymap lem-post-create-community-mode-map
  :global nil
  :after-hook ; disable auto-fill-mode:
  (lambda ()
    (auto-fill-mode -1)))

(provide 'lem-post)
;;; lem-post.el ends here
