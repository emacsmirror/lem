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

(defvar-local lem-post-title nil)
(defvar-local lem-post-url nil)
(defvar-local lem-post-community-id nil)

(defun lem-post-read-title ()
  ""
  (interactive)
  (setq lem-post-title
        (read-string "Post title: ")))

(defun lem-post-read-url ()
  ""
  (interactive)
  ;; TODO: check against rough URL regex
  (setq lem-post-url
        (read-string "Post URL: ")))

(defun lem-post-select-community ()
  ""
  (interactive)
  (let* ((communities (lem-list-communities "Subscribed"))
         (list (lem-ui--communities-alist
                (alist-get 'communities communities)))
         (choice (completing-read "Community: " ; TODO: default to current view
                                  list))
         (community-id (string-to-number
                        (alist-get choice list nil nil #'equal))))
    (setq lem-post-community-id community-id)
    (message "posting to: %s" choice)))

(defun lem-post-compose
    (&optional reply-to-user reply-to-id reply-json initial-text edit)
  ""
  (interactive)
  (fedi-post--compose-buffer
   reply-to-user reply-to-id reply-json initial-text edit
   #'lem-post-mode lem-post-mode-map))

(defun lem-post-submit ()
  ""
  (interactive)
  ;; TODO: check for title/url/comm-id first
  (let* ((body (fedi-post--remove-docs))
         (response (lem-create-post lem-post-title lem-post-community-id body
                                    lem-post-url fedi-post-content-nsfw
                                    nil fedi-post-language))) ; TODO: honeypot
    (when response
      (let-alist response
        (message "Post %s created!" .post_view.post.name)))))

(defun lem-ui-new-post-simple ()
  "Create and submit new post."
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

(defun lem-ui-reply-simple ()
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


;; disable auto-fill-mode:
(add-hook 'lem-post-mode-hook
          (lambda ()
            (auto-fill-mode -1)))

(define-minor-mode lem-post-mode
  "Minor mode for posting to lemmy."
  :keymap lem-post-mode-map
  :global nil)

(provide 'lem-post)
;;; lem-ui.el ends here
