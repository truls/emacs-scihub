;;; scihub.el --- Download from scihub -*- lexical-binding: t -*-

;; Copyright (C) 2019 Truls Asheim

;; Author: Truls Asheim <truls@asheim.dk>
;; Maintainer: Truls Asheim <truls@asheim.dk>
;; Created: 25 Aug 2019
;; Keywords: convenience tools comm
;; Homepage: https://github.com/truls/emacs-scihub
;; Package-Version: 0.1
;; Package-Requires: ((aio "1.0") (request "0.1.3") (emacs "24.3") (dash "2.16.0") (s "1.12.0"))

;;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Provides an Emacs interface for downloading articles from
;; sci-hub. Supports prompting the user with a captcha.
;;
;; TODO:
;; - Handle HTTP 500 error codes occasionally returned when fetching
;; the actual PDF. Probably using retries
;; - Ensure that missing articles are handled correctly
;; - Validate function parameters. For example, check that given DOIs
;; are valid.
;; - Do sci-hub URL fallback in case the first one tried doesn't work
;; - Get sci-hub URL from whereisscihub.now.sh
;; - Handle captcha returned for entire page. This seems hard to reproduce.

;;; Code:

(require 'aio-request)
(require 'dom)
(require 'dash)
(require 'cl-lib)
(require 's)

;; TODO: Add support for getting url from whereisscihub.now.sh
(defcustom scihub--lookup-url?
  nil
  "Set to non-nil if current scihub URL should be looked up automatically")

(defcustom scihub--urls
  '("https://sci-hub.tw"
    "https://sci-hub.se"
    "http://80.82.77.84"
    "http://80.82.77.83")
  "List of scihub URLs to try. Set `scihub--lookup-url?' to `t'
  to attempt to locate the current scihub url dynamically")

(defcustom scihub--captcha-retries
  3
  "The number of tries given to enter the correct captcha.")

(defvar scihub--debug
  'error
  "Debug output level. Set to 'error (default) 'info or 'debug. Use in a let-bind functions before calling.")

(defun scihub--url (suffix)
  (format "%s/%s" (car scihub--urls) suffix))

(defun scihub--buffer-to-dom ()
  ;; The coding hackery here is to work around an issue where
  ;; the buffer containing the downloaded content doesn't have
  ;; the right encoding causing issues for the HTML parser when
  ;; encountering utf-8 chars. Not sure if this is the right way
  ;; to solve it.
  (-if-let* ((coding (detect-coding-region (point-min) (point-max)))
             (d (progn
                  (decode-coding-region (point-min) (point-max) (car coding))
                  (libxml-parse-html-region (point-min) (point-max)))))
      d
    (error "Unable to parse DOM")))

(defun scihub--content-type-is (response type)
  (s-contains? type (request-response-header response "content-type") t))

(defun scihub--tags-by-attr-eq (tags attr val)
  (-filter (lambda (t) (string= (dom-attr t attr) val)) tags))

(defun scihub--captcha-prompt (response)
  "Shows a captcha prompt by showing the image provided in the
body of RESPONSE in a temporary buffer named after IMAGE-FILENAME
and prompt for the captcha value. Returns the captcha value."
  (save-window-excursion
    (let ((body (request-response-data response))
          (buf (create-file-buffer "scihub-captcha/")))
      (with-current-buffer buf
        (insert body)
        (image-mode))
      (switch-to-buffer buf))
    (prog1
        (read-from-minibuffer "Enter captcha text: "))))

(aio-defun scihub--get-captcha-img (dom domain)
  (-if-let*
      ((captcha-file (dom-attr (car (dom-by-id dom "captcha")) 'src))
       (captcha-url (format "%s/%s" domain captcha-file)))
      (progn
        (message captcha-file)
        (aio-await (aio-request captcha-url
                                :parser #'buffer-string
                                ;; https://github.com/tkf/emacs-request/issues/134
                                :encoding 'no-conversion)))))

(aio-defun scihub--ask-captcha (response)
  (-if-let* ((dom (with-temp-buffer
                    (insert (request-response-data response))
                    (scihub--buffer-to-dom)))
             (captcha-id (dom-attr (car (scihub--tags-by-attr-eq
                                         (dom-by-tag dom 'input) 'name "id"))
                                   'value)))
      (progn
        (-if-let* ((img (aio-await (scihub--get-captcha-img dom
                                                            (scihub--url-domain-part
                                                             (request-response-url response)))))
                   (answer (scihub--captcha-prompt img)))
            `(("id" . ,captcha-id)
              ("answer" . ,answer))
          (error "Failed to get captcha image")))
    (error "Scihub captcha page parsing failed. Please report bug")))

(defun scihub--write-string-to-file (contents file)
  (with-temp-buffer
    (insert contents)
    (write-region (point-min) (point-max) file))
  t)

(defun scihub--url-domain-part (url)
  (car (s-match "https?://[^/]\*" url)))

;;; Cases
;; 1) We get the article
;; 2) Two we get a captcha
;; 3) Redirection to "trying to get article page"
;; 4) Unable to get article

(defun scihub--normalize-url (url)
  (if (not (s-starts-with? "http" url))
      (concat "https:" url)
    url))

(aio-defun scihub--try-get-pdf-url (dom file)
  (-if-let* ((pdf-url
              (scihub--normalize-url
               (car  (s-split
                      "#"
                      (dom-attr (car (dom-by-id dom "pdf")) 'src))))))
      (progn
        (message pdf-url)
        (aio-await (scihub--get-pdf-link pdf-url file))
        t)
    nil))

(aio-defun scihub--get-article-link-promise (url file)
  (-if-let* ((dom (request-response-data
                   (aio-await (aio-request url
                                           :parser #'scihub--buffer-to-dom))))
             (res (scihub--try-get-pdf-url dom file)))
      (aio-await res)
    ;; Check if we got the global captcha
    ;; TODO: This is currently untested and really hard to reproduce
    ;; as I've only seen it once.
    (-if-let*
        ((inputs (dom-by-tag dom 'input))
         (url (dom-attr (car (scihub--tags-by-attr-eq inputs 'name "url")) 'value))
         (captcha-id (dom-attr (car (scihub--tags-by-attr-eq inputs 'name "captchaId")) 'value))
         (img (scihub--get-captcha-img dom (scihub--url-domain-part url)))
         (answer (scihub--captcha-prompt "captcha"))
         (dom2 (request-response-data
                (aio-await (aio-request "https://sci-hub.tw/solve"
                                        ;;:method "POST"
                                        :data `(("url" . ,url)
                                                ("captchaId" . ,captcha-id)
                                                ("captcha_code" . answer)))))))
        (aio-await (scihub--try-get-pdf-url dom2 file))
      (error "Unable to get pdf url"))
    (error "Unable to get page")))

(aio-defun scihub--get-pdf-link (url file)
  (let ((downloaded nil)
        (captcha nil)
        (depth 0))
    ;; Recursion seems not to be working inside aio-defun so we do
    ;; this instead
    (while (and (not downloaded)
                (> scihub--captcha-retries depth))
      (-if-let*
          ((response-
            (if captcha
                (progn
                  (message "Requesting with captcha")
                  (prin1 captcha)
                  (let ((request-log-level 'debug)
                        (request-message-level 'debug))
                    (aio-request url
                                 ;; It is crucial that we do not
                                 ;; specify :type POST here as this
                                 ;; option will be passed on to CURL
                                 ;; as -X POST which overrides the
                                 ;; behavior specified by RFC2616 that
                                 ;; if a POST request returns 30{1..3}
                                 ;; the following request should be a
                                 ;; GET. Scihub will return a 302
                                 ;; following valid captcha
                                 ;; input and expects a subsequent GET
                                 ;; request.
                                 ;; https://github.com/curl/curl/issues/578
                                 :data captcha
                                 :parser #'buffer-string
                                 ;; https://github.com/tkf/emacs-request/issues/134
                                 :encoding 'no-conversion)))
              (progn
                (message "Requesting without captcha")
                (aio-request url
                             :parser #'buffer-string
                             ;; https://github.com/tkf/emacs-request/issues/134
                             :encoding 'no-conversion))))
           (response (aio-await response-)))
          (cond ((scihub--content-type-is response "text/html")
                 ;; Probably a captcha
                 (message "is captcha")
                 (message "%s" (request-response-data response))
                 (when (> depth 3) (error "recursion depth"))
                 (let ((answer (aio-await
                                (scihub--ask-captcha response))))
                   (setq captcha answer)))
                ((scihub--content-type-is response "application/pdf")
                 (let ((body (request-response-data response)))
                   (message "Got PDF")
                   (setq downloaded (scihub--write-string-to-file body file))))
                (t (error "Unrecognized content-type in response")))
        (progn
          (setq downloaded t)
          (error "Failed to download article")))
      (setq depth (+ depth 1)))))

(defun scihub--call (method f &rest args)
  (let ((request-log-level scihub--debug)
        (request-message-level scihub--debug))
    (cl-case method
      ('sync (aio-wait-for (apply f args)))
      ('async (aio-with-async (apply f args)))
      ('promise (apply f args))
      (otherwise (user-error "Invalid METHOD")))))

;;;###autoload
(cl-defun scihub-get-from-publisher-url (url dest &optional (method 'sync))
  "Download article from published URL and save to DEST using METHOD.

METHOD should be one of the following:


'sync              Fetch synchronously. Don't return until
                   fetch is complete. This is the default.
'async             Fetch asynchronously. Function call
                   returns immediately while the download
                   continues in the background.
'promise           Returns a promise for chaining with the aio library.
"
  (interactive "sPublisher URL to fetch using scihub: \nfSave to file: ")
  (scihub--call method #'scihub--get-article-link-promise (scihub--url url) dest))

;;;###autoload
(cl-defun scihub-get-from-doi (doi dest &optional (method 'sync))
  "Fetch article from DOI and save to DEST using METHOD.

'sync              Fetch synchronously. Don't return until
                   fetch is complete. This is the default.
'async             Fetch asynchronously. Function call
                   returns immediately while the download
                   continues in the background.
'promise           Returns a promise for chaining with the aio library.
"
  (interactive "sDOI of article to fetch: \nfSave to file: ")
  (scihub--call method #'scihub--get-article-link-promise (scihub--url doi) dest))

;;;###autoload
(cl-defun scihub-get-from-scihub-url (url dest &optional (method 'sync))
  "Downloads article specified by the URL. The URL given here
will be used unmodified and assumed to be pointing to
scihub. Save download to DEST using METHOD.

'sync              Fetch synchronously. Don't return until
                   fetch is complete. This is the default.
'async             Fetch asynchronously. Function call
                   returns immediately while the download
                   continues in the background.
'promise           Returns a promise for chaining with the aio library.
"
  (interactive "sFull sci-hub URL to fetch article from: \nfSave to file: ")
  (scihub--call method #'scihub--get-article-link-promise url dest))

(provide 'scihub)

;;; scihub.el ends here
