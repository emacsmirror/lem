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

;;; nothing yet

;;; Code:

(require 'lem)

(defvar lem-ui-horiz-bar
  (if (char-displayable-p ?―)
      (make-string 12 ?―)
    (make-string 12 ?-)))

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
  (if-let* ((symbol (alist-get name mastodon-tl--symbols)))
      (if (char-displayable-p (string-to-char (car symbol)))
          (car symbol)
        (cdr symbol))
    "?"))

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
  (format "%s %s %s" (lem-ui-symbol 'reply)
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
    (with-current-buffer "*lem*"
      (insert "\n"
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
      (when (and children
                 (< 0 .counts.comments))
        (let* ((id (number-to-string .post.id))
               (comms (lem-get-post-comments id nil sort))
               (list (alist-get 'comments comms)))
          (mapc (lambda (x)
                  (lem-ui-render-comment x :children sort))
                list))))))

(defun lem-ui-render-posts (posts &optional children sort)
  "Render a list of abbreviated posts POSTS.
Used for communities posts or instance posts."
  (let ((list (alist-get 'posts posts)))
    (lem-ui-with-buffer "*lem*" 'special-mode t
      ;; (lem-ui-render-post (car list)))))
      (mapc (lambda (x)
              (lem-ui-render-post x children sort))
            list)
      (goto-char (point-min)))))

(defun lem-ui-get-community-id (community)
  "Returns ID of COMMUNITY as a string."
  (number-to-string
   (alist-get 'id
              (alist-get 'community
                         (alist-get 'community_view community)))))

(defun lem-ui-view-community (name render-sort limit)
  "View community with NAME, sorting by SORT.
RENDER-SORT can be \"New\", \"Hot\", \"Old\", or \"Top\"."
  (let* ((community (lem-get-community-by-name name))
         (id (lem-ui-get-community-id community))
         (posts (lem-list-posts id nil limit)))
    (lem-ui-render-posts posts nil render-sort))) ; no children, ie comments

(defun lem-ui-render-comment (comment &optional children sort)
"Render single COMMENT.
Optionally render its CHILDREN.
Sort can be \"New\", \"Hot\", \"Old\", or \"Top\"."
(let-alist comment
  ;; (lem-ui-with-buffer "*lem*" 'special-mode t
  (with-current-buffer "*lem*"
    (insert "\n"
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
    (when (and children
               (< 0 .counts.child_count))
      (let* ((comms (setq lem-post-comments
                          (lem-get-post-comments (number-to-string .post.id)
                                                 (number-to-string .comment.id)
                                                 sort)))
             (list (setq lem-post-comments-list
                         (alist-get 'comments comms))))
        ;; (mapc (lambda (x)
        ;;         (lem-ui-render-comment x :children
        ;;                                ;; nil
        ;;                                sort))
        (cdr list))))))
;;)

;; (setq lem-post-comments (lem-get-post-comments "1235982" "651145" "New"))
;; (setq lem-post-comments (lem-get-post-comments "1235982" nil "New"))

(provide 'lem-ui)
;;; lem-ui.el ends here
