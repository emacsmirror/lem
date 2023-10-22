;;; lem-request.el --- Request macro for lem.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  martian hiatus
;; Copyright (C) 2023  martian hiatus
;; Author: martian hiatus <martianhiatus [a t] riseup [d o t] net>
;; URL: https://codeberg.org/martianh/lem.el
;; Keywords: multimedia

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

;; A macro to define easily define requests for lem.el.

;;; Code:
(require 'fedi)

(defvar lem-api-version)
(defvar lem-instance-url)

(defmacro lem-def-request
    (method name endpoint
            &optional args docstring params man-params headers
            unauthorized)
  "Create a http request function NAME, using http METHOD, for ENDPOINT.
ARGS are for the function.

PARAMS is a plain list of elements from which to build an alist
of form parameters to send with the request. The name of the
corresponding arg must match the key of the parameter (i.e. if
the API parameter is \"person_id\", the corresponding arg, and
param in the request must both be \"person-id\").

MAN-PARAMS is an alist, to append to the one created from PARAMS.
They are manual, meaning that that the key and arg don't have to
be the same. This can be used for boolean parameters. If the
request sends encoded JSON data (ie POST or PUT), MAN-PARAMS
should be formatted as plain Emacs Lisp: \='((\"boolean\" . t))',
if the request sends query string parameters (GET, etc.), then
MAN-PARAMS should be formatted as strings only: \='((\"boolean\" .
\"true\"))'.

HEADERS is an alist that will be bound as `url-request-extra-headers'.

This macro is designed to generate functions for fetching data
from JSON APIs.

To use it, you first need to set `lem-instance-url' to the URL of
a lemmy instance.

The name of functions generated with this will be the result of:
\(concat fedi-package-prefix \"-\" NAME).

The full URL for the endpoint is constructed by `fedi-http--api',
which see. ENDPOINT does not require a preceding slash.

For example, to define a GET request, called PKG-search to
endpoint /search:

\(lem-def-request \"get\" \"search\" \"search\"
  (q)
  \"Make a GET request.
Q is the search query.\"
  \=(q))."
  (declare (debug t)
           (indent 3))
  (let ((req-fun (intern (concat "fedi-http--" method))))
    `(defun ,(intern (concat "lem-" name)) ,args
       ,docstring
       (let* ((req-url (fedi-http--api ,endpoint lem-instance-url lem-api-version))
              (auth-token lem-auth-token)
              (auth-header `(("Authorization" .
                              ,(concat "Bearer " auth-token))))
              (url-request-method ,(upcase method))
              (url-request-extra-headers ,(if headers
                                              `(append ,headers auth-header)
                                            `auth-header))
              (url-user-agent lem-user-agent) ; lemmy.ml requres a non-nil agent
              ,(if unauthorized
                   `(_auth nil)
                 `(auth `(("auth" . ,auth-token))))
              (params-alist ,(when params
                               `(remove nil
                                        (list ,@(fedi-make-params-alist
                                                 params #'fedi-arg-when-expr
                                                 (when (equal method "get")
                                                   `:coerce))))))
              ;; `(list ,@(fedi-make-params-alist
              ;;           params #'fedi-arg-when-expr)))))
              (params ,(if man-params
                           `(append ,man-params params-alist)
                         `params-alist))
              (params ,(if unauthorized
                           `params
                         `(append auth params)))
              (response
               ,(if (or (equal method "post")
                        (equal method "put"))
                    ;; FIXME: deal with headers nil arg here:
                    `(funcall #',req-fun req-url params nil :json)
                  `(funcall #',req-fun req-url params :silent))))
         (fedi-http--triage response
                            (lambda ()
                              (with-current-buffer response
                                (fedi-http--process-json))))))))

(provide 'lem-request)
;;; lem-request.el ends here
