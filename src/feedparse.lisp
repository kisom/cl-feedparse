#|
  This file is a part of feedparse project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage feedparse
  (:use :cl)
  (:export :parse-feed
           :feed :feed-title :feed-kind :feed-items
           :feed-item :item-title :item-date :item-link :item-body))
(in-package :feedparse)

(setf s-xml:*ignore-namespaces* t)

(defclass feed ()
  ((title :initarg :title :reader feed-title :type string)
   (kind  :initarg :kind  :reader feed-kind  :type keyword)
   (items :initarg :items :reader feed-items :type list))
  (:documentation "feed is a generic container for storing feed
  items."))

(defmethod describe-object ((feed feed) stream)
  (let ((kind (cond ((eql (feed-kind feed) :rss) "RSS")
                    ((eql (feed-kind feed) :atom) "Atom")
                    (:else "Unknown"))))
    (format stream "~A (~A feed)~%Item count: ~A~%~{  Entry: ~A~%~}~%" 
            (feed-title feed) kind
            (length (feed-items feed))
            (mapcar #'item-title (feed-items feed)))))

(defclass feed-item ()
  ((title :initarg :title :reader item-title)
   (date  :initarg :date  :reader item-date)
   (link  :initarg :link  :reader item-link)
   (body  :initarg :body  :reader item-body))
  (:documentation "A feed item is a single item in a feed."))

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
  (let ((feed-xml (fetch-feed url)))
    (parser-dispatch feed-xml)))

(defun fetch-feed (url)
  (let ((body (drakma:http-request url)))
    (s-xml:parse-xml-string
     (cond
       ((stringp body) body)
       (:else (flexi-streams:octets-to-string body))))))

(defmacro get-xml-element (element item)
  `(second (assoc ,element ,item)))

(defun parse-rss (feed-xml)
  (let ((channel (cdr (assoc :|channel| feed-xml))))
    (make-instance 'feed
                   :title (get-xml-element :|title| channel)
                   :kind  :rss
                   :items (extract-rss-items channel))))

(defun parse-rss-item (item)
  (make-feed-item :title (get-xml-element :|title| item)
                  :date  (get-xml-element :|pubDate| item)
                  :body  (get-xml-element :|description| item)
                  :link  (get-xml-element :|link| item)))

(defun extract-rss-items (channel)
  (mapcar #'parse-rss-item
          (mapcar #'cdr
                  (remove-if-not (lambda (elt)
                                   (equal (first elt) :|item|))
                                 channel))))


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


