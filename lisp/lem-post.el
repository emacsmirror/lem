;;; lem-post.el --- Posting for lem.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; Version: 0.1
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

(defalias 'lem-post-cancel #'fedi-post-cancel)
(defalias 'lem-post-toggle-nsfw #'fedi-post-toggle-nsfw)
(defalias 'lem-post-set-post-language #'fedi-post-set-post-language)

;; FIXME: not all post options are applicable when commenting!
;; maybe `fedi-post-mode-map' for comments?
(defvar lem-post-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inheriting doesn't work for our post docs display
    ;; (set-keymap-parent map fedi-post-mode-map)
    (define-key map (kbd "C-c C-k") #'lem-post-cancel)
    (define-key map (kbd "C-c C-n") #'lem-post-toggle-nsfw)
    (when (require 'emojify nil :noerror)
      (define-key map (kbd "C-c C-e") #'lem-post-insert-emoji))
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
    (when (require 'emojify nil :noerror)
      (define-key map (kbd "C-c C-e") #'lem-post-insert-emoji))
    (define-key map (kbd "C-c C-l") #'lem-post-set-post-language)
    (define-key map (kbd "C-c C-c") #'lem-post-submit)
    map)
  "Keymap for `lem-post-comment-mode'.")

(defvar-local lem-post-title nil)
(defvar-local lem-post-url nil)
(defvar-local lem-post-community-id nil)

(defvar-local lem-post-reply-post-id nil)
(defvar-local lem-post-reply-comment-id nil)

(defvar lem-post-community-regex
  (rx (| (any ?\( "\n" "\t "" ") bol) ; preceding things
      (group-n 2 (+ ?! (* (any ?- ?_ ?. "A-Z" "a-z" "0-9" ))) ; community
               (? ?@ (* (not (any "\n" "\t" " "))))) ; optional domain
      (| "'" word-boundary)))

(defun lem-post-read-title ()
  "Read post title."
  (interactive)
  (setq lem-post-title
        (read-string "Post title: "
                     lem-post-title))
  (message "%s" lem-post-title))

(defun lem-post-read-url ()
  "Read post URL."
  (interactive)
  ;; TODO: check against rough URL regex
  (setq lem-post-url
        (read-string "Post URL: "
                     lem-post-url))
  (message "%s" lem-post-url))

(defun lem-post-select-community ()
  "Select community to post to."
  (interactive)
  (let* ((communities (lem-api-get-subscribed-communities))
         ;; (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-alist communities))
         ;; (alist-get 'communities communities)))
         (choice (completing-read "Community: " ; TODO: default to current view
                                  list nil :match))
         (community-id (string-to-number
                        (alist-get choice list nil nil #'equal))))
    (setq lem-post-community-id community-id)
    (message "%s" choice)))

(defun lem-post-compose (&optional edit mode)
  "Compose a new post.
EDIT means we are editing.
MODE is the lem.el minor mode to enable in the compose buffer."
  (interactive)
  (fedi-post--compose-buffer edit
                             (or mode #'lem-post-mode)
                             (when mode "lem-post")
                             (list #'lem-post--mentions-capf
                                   #'lem-post--comms-capf)))

(defun lem-post-submit ()
  "Post the post to lemmy."
  (interactive)
  ;; TODO: check for title/url/comm-id first
  (let* ((body (fedi-post--remove-docs))
         (response (if lem-post-reply-post-id
                       (lem-create-comment lem-post-reply-post-id
                                           body
                                           lem-post-reply-comment-id)
                     (lem-create-post lem-post-title lem-post-community-id body
                                      lem-post-url fedi-post-content-nsfw
                                      nil fedi-post-language)))) ; TODO: honeypot
    (when response
      (let-alist response
        (if lem-post-reply-post-id
            (progn
              (message "Comment created: %s" .comment_view.comment.content)
              (lem-ui-view-post (number-to-string lem-post-reply-post-id)))
          (message "Post %s created!" .post_view.post.name)))
      (with-current-buffer "*new post*"
        ;; FIXME: we have to call this after using b-local
        ;; `lem-post-reply-post-id', but it baulks:
        (fedi-post-kill)))))

;; (defun lem-ui-new-post-simple ()
;;   "Create and submit new post."
;;   (interactive)
;;   (let* ((name (read-string "Post title: "))
;;          (communities (lem-list-communities "Subscribed"))
;;          (list (lem-ui--communities-alist
;;                 (alist-get 'communities communities)))
;;          (choice (completing-read "Community: " ; TODO: default to current view
;;                                   list))
;;          (community-id (string-to-number
;;                         (alist-get choice list nil nil #'equal)))
;;          (body (read-string "Post body [optional]: "))
;;          (url (read-string "URL [optional]: "))
;;          (response
;;           (lem-create-post name community-id body
;;                            (when (not (equal "" url))
;;                              url))))
;;     ;; TODO: nsfw, etc.
;;     (when response
;;       (let-alist response
;;         (message "Post %s created!" .post_view.post.name)))))

(defun lem-post-reply ()
  "Reply to a post or comment."
  (interactive)
  (let* ((json (lem-ui-thing-json))
         (type (lem-ui--item-type))
         (post-id (if (equal type 'post)
                      (lem-ui--id-from-prop)
                    (lem-ui--id-from-json json 'post)))
         (comment-id (when (equal type 'comment)
                       (lem-ui--id-from-json json 'comment))))
    (lem-post-compose nil #'lem-post-comment-mode)
    (setq lem-post-reply-post-id post-id)
    (setq lem-post-reply-comment-id comment-id)))

;; (defun lem-ui-reply-simple ()
;;   "Reply to post or comment at point.
;; Simple means we just read a string."
;;   (interactive)
;;   (let* ((json (lem-ui-thing-json))
;;          (type (lem-ui--item-type))
;;          (content (read-string "Reply: "))
;;          (post-id (if (equal type 'post)
;;                       (lem-ui--id-from-prop)
;;                     (lem-ui--id-from-json json 'post)))
;;          (comment-id (when (equal type 'comment)
;;                        (lem-ui--id-from-json json 'comment)))
;;          (response (lem-create-comment post-id content comment-id)))
;;     (when response
;;       (let-alist response
;;         (message "Comment created: %s" .comment_view.comment.content)
;;         (lem-ui-view-post (number-to-string post-id))))))

;;; COMPLETION

(defun lem-post-items-alist (items type prefix)
  "Return an alist of ITEMS, of TYPE, with PREFIX."
  (cl-loop for i in items
           for it = (alist-get type i)
           collect (cons (concat prefix (alist-get 'name it))
                         (alist-get 'actor_id it))))

(defun lem-post-users-alist (users)
  "Return an alist of USERS, each element a cons of name and URL."
  (lem-post-items-alist users 'person "@"))

(defun lem-post-comms-alist (comms)
  "Return an alist of communities COMMS, each element a cons of name and URL."
  (lem-post-items-alist comms 'community "!"))

(defun lem-post-mentions-fun (start end)
  "Given prefix str between START and END, return an alist of mentions for capf."
  (let* ((query (lem-api-search-users
                 (buffer-substring-no-properties (1+ start) ; cull '@'
                                                 end)
                 nil nil "50")) ; max limit
         (users (alist-get 'users query)))
    (lem-post-users-alist users)))

(defun lem-post-comms-fun (start end)
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
                          #'lem-post-mentions-fun
                          #'lem-post--mentions-annot-fun
                          nil ; #'lem-post--mentions-affix-fun
                          #'lem-post--md-link-exit-fun))

(defun lem-post--comms-capf ()
  "Build a communities completion backend for `completion-at-point-functions'."
  (fedi-post--return-capf lem-post-community-regex
                          #'lem-post-comms-fun
                          #'lem-post--mentions-annot-fun
                          nil ; #'lem-post--mentions-affix-fun
                          #'lem-post--md-link-exit-fun))

;; disable auto-fill-mode:
(add-hook 'lem-post-mode-hook
          (lambda ()
            (auto-fill-mode -1)))

(define-minor-mode lem-post-mode
  "Minor mode for submitting posts to lemmy."
  :keymap lem-post-mode-map
  :global nil)

(define-minor-mode lem-post-comment-mode
  "Minor mode for submitting comments to lemmy."
  :keymap lem-post-comment-mode-map
  :global nil)

(provide 'lem-post)
;;; lem-post.el ends here
