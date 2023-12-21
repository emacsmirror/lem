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

(defvar-local lem-post-title nil)
(defvar-local lem-post-url nil)
(defvar-local lem-post-community-id nil)
(defvar-local lem-post-community-name nil)
(defvar-local lem-post-edit-id nil)
(defvar-local lem-post-comment-edit-id nil)

(defvar-local lem-post-comment-post-id nil)
(defvar-local lem-post-comment-comment-id nil)

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
  (lem-ui-do-community-completing "Post to community: "
                                  (lambda (id choice)
                                    (setq lem-post-community-name choice)
                                    (setq lem-post-community-id id)
                                    (message "Posting to %s" choice))
                                  #'lem-api-get-subscribed-communities)
  (fedi-post--update-status-fields))

(defun lem-post-compose (&optional edit mode comment)
  "Compose a new post.
EDIT means we are editing.
MODE is the lem.el minor mode to enable in the compose buffer.
COMMENT means we are composing a comment."
  (interactive)
  (fedi-post--compose-buffer edit
                             #'markdown-mode
                             (or mode #'lem-post-mode)
                             (when mode "lem-post")
                             (or comment 'post)
                             (list #'lem-post--mentions-capf
                                   #'lem-post--comms-capf)
                             (unless comment
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
                                  (face . lem-post-community-face))))))

(defun lem-post-submit ()
  "Submit the post to lemmy."
  (interactive)
  (let ((buf (buffer-name)))
    (if (and (string-suffix-p "post*" buf)
             (not (and lem-post-title
                       lem-post-community-id)))
        (message "You need to set at least a post name and community.")
      (let* ((body (fedi-post--remove-docs))
             (response (cond (lem-post-comment-post-id ; creating a comment
                              (lem-create-comment lem-post-comment-post-id
                                                  body
                                                  lem-post-comment-comment-id))
                             (lem-post-edit-id ; editing a post
                              (lem-edit-post lem-post-edit-id lem-post-title body
                                             lem-post-url fedi-post-content-nsfw fedi-post-language))
                             (lem-post-comment-edit-id ; editing a comment
                              (lem-edit-comment lem-post-comment-edit-id body))
                             (t ; creating a post
                              (lem-create-post lem-post-title lem-post-community-id body
                                               lem-post-url fedi-post-content-nsfw
                                               nil fedi-post-language))))) ; TODO: honeypot
        (when response
          (let-alist response
            (cond (lem-post-comment-post-id
                   (message "Comment created: %s" .comment_view.comment.content))
                  (lem-post-comment-edit-id
                   (message "Comment edited: %s" .comment_view.comment.content))
                  ;; FIXME: prev window config + reload instead, coz maybe in a diff view:
                  ;; this breaks `fedi-post-kill'
                  ;; (lem-ui-view-post (number-to-string lem-post-comment-post-id)))
                  (lem-post-edit-id
                   (message "Post %s edited!" .post_view.post.name))
                  (t
                   (message "Post %s created!" .post_view.post.name))))
          (with-current-buffer buf
            ;; FIXME: we have to call this after using b-local
            ;; `lem-post-comment-post-id', but it baulks:
            (fedi-post-kill)))))))

(defun lem-post-compose-simple ()
  "Create and submit new post, reading strings in the minibuffer."
  (interactive)
  (let* ((name (read-string "Post title: "))
         (communities (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-alist
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

(provide 'lem-post)
;;; lem-post.el ends here
