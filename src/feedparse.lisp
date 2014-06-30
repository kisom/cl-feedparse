#|
  This file is a part of feedparse project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage feedparse
  (:use :cl)
  (:export :parse-feed
           :parse-feed-string
           :feed :feed-title :feed-kind :feed-link :feed-items
           :feed-item :item-title :item-date :item-link :item-body))
(in-package :feedparse)

(setf s-xml:*ignore-namespaces* t)

(defun read-file-string (path)
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

(defun byte-string-p (str)
  (and (arrayp str)
       (or (standard-char-p (elt str 0))
           (characterp (elt str 0)))))

(defclass feed ()
  ((title :initarg :title :reader feed-title :type string
          :documentation "Return the feed's title.")
   (kind  :initarg :kind  :reader feed-kind  :type keyword
          :documentation "Return the feed's kind as a keyword (:RSS
          or :ATOM).")
   (link  :initarg :link  :reader feed-link  :type string
          :documentation "Return the link to the feed's source.")
   (items :initarg :items :reader feed-items :type list
          :documentation "Return the list of entries in the feed."))
  (:documentation "feed is a generic container for storing feed
  items."))

(defmethod describe-object ((feed feed) stream)
  (let ((kind (cond ((eql (feed-kind feed) :rss) "RSS")
                    ((eql (feed-kind feed) :atom) "Atom")
                    (:else "Unknown"))))
    (format stream "~A (~A feed)~%Link: ~A~%Item count: ~A~%~{  Entry: ~A~%~}~%" 
            (feed-title feed) kind
            (feed-link feed)
            (length (feed-items feed))
            (mapcar #'item-title (feed-items feed)))))

(defclass feed-item ()
  ((title :initarg :title :reader item-title
          :documentation "Return the item's title.")
   (date  :initarg :date  :reader item-date
          :documentation "Return the date the item was published.")
   (link  :initarg :link  :reader item-link
          :documentation "Return the link pointing to the entry.")
   (body  :initarg :body  :reader item-body
          :documentation "Return the body of the feed entry, as stored
          in the entry's description field."))
  (:documentation "A feed item stores a single item in a feed."))

(defmethod describe-object ((item feed-item) stream)
  (format stream "Entry: ~A (published ~A)~%Link: ~A~%Description:~%~A~%"
          (item-title item) (item-date item)
          (item-link item) (item-body item)))

(defun make-feed-item (title date link body)
  (make-instance 'feed-item
                 :title title
                 :date date
                 :link link
                 :body body))

(defun parser-dispatch (feed-xml)
    (cond
      ((assoc :|rss| feed-xml) (parse-rss feed-xml))
      ((assoc '|feed| feed-xml) (parse-atom feed-xml))
      (:else (error "Unknown feed type."))))

(defun parse-feed (url)
  "Fetch the feed described in URL over HTTP, and parse the
feed. Returns a feed object."
  (parse-feed-string (drakma:http-request url)))

(defun parse-feed-string (str)
  "Parse the feed stored in the string or array of octets, returning a
feed object."
  (parser-dispatch
   (s-xml:parse-xml-string
    (cond
      ((stringp str) str)
      ((byte-string-p str) (flexi-streams:octets-to-string str))
      (:else (error "Unknown content-type."))))))

(defun get-xml-element (element item)
  (second (assoc element item)))

(defun parse-rss (feed-xml)
  (let ((channel (cdr (assoc :|channel| feed-xml))))
    (make-instance 'feed
                   :title (get-xml-element :|title| channel)
                   :kind  :rss
                   :link  (get-xml-element :|link| channel)
                   :items (extract-rss-items channel))))

(defun parse-rss-item (item)
  (make-feed-item (get-xml-element :|title| item)
                  (get-xml-element :|pubDate| item)
                  (get-xml-element :|link| item)
                  (get-xml-element :|description| item)))

(defun build-rss-item-list (channel)
  (mapcar #'cdr
          (remove-if-not (lambda (elt)
                           (equal (first elt) :|item|))
                         channel)))

(defun extract-rss-items (channel)
  (mapcar #'parse-rss-item
          (build-rss-item-list channel)))


(defun build-atom-item-list (feed-xml)
  (mapcar #'cdr
          (remove-if-not (lambda (elt)
                           (equal (first elt) :|entry|))
                         feed-xml)))

(defun parse-atom (feed-xml)
  (make-instance 'feed
                 :title (get-xml-element :|title| feed-xml)
                 :kind :atom
                 :items nil))


