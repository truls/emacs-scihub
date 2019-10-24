;; -*- lexical-binding: t -*-

(setq test-list '((:name "Article 1"
                   :doi "10.1109/JSSC.2019.2923081"
                   :publisher-url "https://ieeexplore.ieee.org/document/8746588"
                   :scihub-url "https://sci-hub.tw/https://ieeexplore.ieee.org/document/8746588"
                   :file "dc1a7acb367a1b7df9a181d92ee0bac77ecfcffa")
                  (:name "Article 2"
                   :doi "10.1016/j.commatsci.2009.12.006"
                   :publisher-url "https://www.sciencedirect.com/science/article/pii/S0927025609004558"
                   :scihub-url "https://sci-hub.tw/https://www.sciencedirect.com/science/article/pii/S0927025609004558"
                   :file "0cb38750104d2ea0e48a21c5018c70b4a0ac512a")
                  (:name "Article 3"
                   :doi "10.1109/HOTCHIPS.2011.7477491"
                   :publisher-url "https://ieeexplore.ieee.org/document/7477491"
                   :scihub-url "https://sci-hub.tw/https://ieeexplore.ieee.org/document/7477491"
                   :file "4dbaef9bb0c7adeee0e96f132a56fafab0c39132")
                  (:name "Article 4"
                   :doi "10.1145/1190183.1190190"
                   :publisher-url "https://dl.acm.org/citation.cfm?doid=1190183.1190190"
                   :scihub-url "https://sci-hub.tw/https://dl.acm.org/citation.cfm?doid=1190183.1190190"
                   :file "a0029008ceb2adf368f59394e7eb91faa78d5d8e")
                  (:name "Article 5"
                   :doi "10.1007/s12599-017-0504-2"
                   :publisher-url "https://link.springer.com/article/10.1007/s12599-017-0504-2"
                   :scihub-url "https://sci-hub.tw/https://link.springer.com/article/10.1007/s12599-017-0504-2"
                   :file "4acb082f8130a0a539d03112f0a37c07352d11b7"
                   )
      ))

(require 'scihub)
(require 'cl-lib)

(defun concat-list (list)
  (apply #'concatenate 'string list))

(buttercup-define-matcher
 :file-sha1-is (fname expected)
 (let* ((fname (funcall fname))
        (hash (with-temp-buffer
                (insert-file-contents-literally fname)
                (secure-hash 'sha1 (current-buffer))))
        (expected (funcall expected)))
   (if (string= hash expected)
       t
     `(nil . ,(format "\nFile hashed to %s but expected %s. File is saved in %s\n" hash expected fname)))))


(describe "Articles"
 (dolist (test test-list)
   (dolist (method '((scihub-get-from-doi :doi)
                     (scihub-get-from-publisher-url :publisher-url)
                     (scihub-get-from-scihub-url :scihub-url)))
     (cl-destructuring-bind (f attr) method
       (let ((file (concat "~/" (plist-get test :file) ".pdf")))
         (it (concat "Fetches " (plist-get test :name) " by " (symbol-name attr))
             (expect
              (prog1
                  file
                (apply f `(,(plist-get test attr) ,file)))
              :file-sha1-is
              (plist-get test :file))))))))
