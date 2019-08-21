;;; -*- lexical-binding: t -*-

;;; scihub-fetcher.el

(require 'request)
(require 'request-deferred)
(require 'dom)
(require 'dash)


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

(defun buffer-to-dom ()
  ;; The coding hackery here is to work around an issue where
  ;; the buffer containing the downloaded content doesn't have
  ;; the right encoding causing issues for the HTML parser when
  ;; encountering utf-8 chars. NOt sure if this is the right way
  ;; to solve it.
  (-if-let* ((coding (detect-coding-region (point-min) (point-max)))
             (d (progn
                  (decode-coding-region (point-min) (point-max) (car coding))
                  (libxml-parse-html-region (point-min) (point-max)))))
      d
    (user-error "Unable to parse DOM")))

(defun content-type-is (response type)
  ;(message (request-response-header response "content-type"))
  (s-contains? type (request-response-header response "content-type") t))

(defun scihub--tags-by-attr-eq (tags attr val)
  (-filter (lambda (t) (eq (dom-attr t attr) val)) tags))

(defun scihub--captcha-prompt (response image-filename)
  "Shows a captcha prompt by showing the image provided in the
body of RESPONSE in a temporary buffer named after IMAGE-FILENAME
and prompt for the captcha value. Returns the captcha value."
  (save-window-excursion
    (let ((body (request-response-data response))
          (buf (create-file-buffer (format "scihub-captcha/%s" image-filename))))
      (with-current-buffer buf
        ;;(insert (decode-coding-string body 'raw-text))
        (insert body)
        (image-mode))
      (switch-to-buffer buf)
      (read-from-minibuffer "Enter captcha text: "))))

(defun scihub--ask-captcha (response)
  (-if-let* ((dom (with-temp-buffer
                    (insert (request-response-data response))
                    (buffer-to-dom)))
             (url-domain (url-domain-part (request-response-url response)))
             (captcha-id (dom-attr (car (dom-by-tag dom 'input)) 'value))
             (captcha-file (dom-attr (car (dom-by-id dom "captcha")) 'src))
             (captcha-url (format "%s/%s" url-domain captcha-file)))
      (progn
        (message "Captcha URL: %s" captcha-url)
        (deferred:$
          (request-deferred captcha-url :parser #'buffer-string)
          (deferred:nextc it
            (lambda (response)
              (scihub--captcha-prompt response captcha-file)))
          (deferred:nextc it
            (lambda (result)
              (message "Captcha was %s" result)
              '(("id" . captcha-id)
                ("answer" . result))))))
    (user-error "Scihub captcha page parsing failed. Please report bug")))

(defun scihub--get-pdf-post (url id answer file)
  (deferred:$
    (request-deferred url
                      :parser #'buffer-string
                      :type "POST"
                      :data `(("id" . ,id) ("answer" . ,answer)))
    (deferred:nextc it
      (lambda (response)
        (let (body (request-type-is-data response))
          w
          )))))

(defun scihub--write-string-to-file (contents file)
  (with-temp-buffer
    (insert contents)
    ;; (insert (decode-coding-string contents 'no-conversion))
    (write-region (point-min) (point-max) file)))

(defun url-domain-part (url)
  (car (s-match "https?://[^/]\*" url)))

;;; Cases
;; 1) We get the article
;; 2) Two we get a captcha
;; 3) Redirection to "trying to get article page"
;; 4) Unable to get article

(defun scihub--get-article-link (url file)
  (let ((pdf-url))
    (deferred:$
      (request-deferred url :parser #'buffer-to-dom)
      (deferred:nextc it
        (lambda (response)
          (-if-let* ((dom (request-response-data response))
                     (pdf-url (dom-attr (car (dom-by-id dom "pdf")) 'src))
                     pdf-url))
          (user-error "Unable to get pdf link")))
      (deferred:nextc it
        (lambda (pdf-url)
          (deferred:nextc (scihub--get-pdf-link pdf-url file)

        ;; (deferred:nextc it
        ;;   (lambda (response)
        ;;     (message "In response")
        ;;     (let ((body (request-response-data response))
        ;;           (buf (create-file-buffer "file.pdf")))
        ;;       (with-current-buffer buf
        ;;         (insert body))
        ;;       (switch-to-buffer buf))))
        ;;                ))))))
        )))

(defun scihub--get-article-link (url-file)
  (deferred:$
    (request-deferred url :parser #'buffer-to-dom)
    (deferred:nextc it
      (lambda (response)
        (-if-let* ((dom (request-response-data response))
                   (pdf-url (dom-attr (car (dom-by-id dom "pdf")) 'src)))
            (scihub--get-pdf-link pdf-url file)
          (user-error "Unable to get pdf link"))))


(defun scihub--get-pdf-link (url file &optional captcha)
  (deferred:$
    ;; (if captcha
    ;;     (request-deferred url
    ;;                       :type "POST"
    ;;                       :params captcha
    ;;                       :parser #'buffer-string)
      (request-deferred url :parser #'buffer-string);)
    (deferred:nextc it
      (lambda (response)
        (cond ((content-type-is response "text/html")
               ;; Probably a captcha
               (scihub--get-pdf-link url file (scihub--ask-captcha response)))
              ((content-type-is response "application/pdf")
               (let ((body (request-response-data response)))
                 (message "Got PDF")
                 (scihub--write-string-to-file body file)))
              (t (message "Got other"))))))
    t)

;;(scihub--get-article-link "https://sci-hub.tw/10.1145/1190183.1190190" "~/download.pdf")

(scihub--get-article-link "https://sci-hub.tw/10.1145/1190183.1190190" "~/download.pdf")

;(defconst scihub--debug-show

;; (with-temp-buffer
;;   (insert-file "file.html")
;;   (-if-let (d (libxml-parse-html-region (point-min) (point-max)))
;;       (message "%s" (dom-attr (car (dom-by-id d "pdf")) 'src))
;;     (error "Parsing failed")))

;; (request "http://sci-hub.tw/10.1145/1190183.1190189"
;;          :type "GET"
;;          :parser (lambda ()
;;                    (-if-let* ((c (buffer-string))
;;                               (d (progn
;;                                         ;(with-temp-buffer
;;                                    (let* ((save-buffer-coding-system 'utf-8-unix)
;;                                           (buf (create-file-buffer "foo")))
;;                                      (switch-to-buffer buf)
;;                                      (message "%s"(detect-coding-string c))
;;                                      (insert (decode-coding-string c
;;                                                                    (car (detect-coding-string c))))
;;                                      (message "%s" (car (detect-coding-region (point-min) (point-max))))
;;                                      (libxml-parse-html-region (point-min) (point-max))))))
;;                        (message "%s" d)
;;                        (message "%s" (dom-attr (car (dom-by-id d "pdf")) 'src))
;;                      (error "Parsing failed"))))


;(scihub--get-article-link "http://google.com")

 ;;(defun scihub--try-scihub-urls)
;;;###autoload
(defun scihub--get-from-url (url dest)
  "Download article from published URL."
  (interactive "sPublisher URL to fetch using scihub: \nfSave to file: ")
  (scihub--get-article-link url det))

;;;###autoload
(defun scihub--get-from-doi (doi dest)
  "Fetch article from DOI."
  (interactive "sDOI of article to fetch: \nfSave to file: ")
  nil)

;;;###autoload
(defun scihub--get-from-full-scihub-url (url dest)
    "Downloads article specified by the URL. The URL given here
    will be used unmodified and assumed to be pointing to
    scihub."
    (interactive "sFull sci-hub URL to fetch article from: \nfSave to file: ")
    nil)

(provide 'scihub)
