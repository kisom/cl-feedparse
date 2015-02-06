;;;
;;;  This file is a part of the feedparse project.
;;;  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
;;;

(in-package :cl-user)
(defpackage feedparse
  (:use :cl)
  (:export :parse-feed
           :parse-feed-string
           :feed :feed-title :feed-kind :feed-link :feed-items
           :feed-item :item-title :item-date :item-link :item-body))
(in-package :feedparse)

(setf s-xml:*ignore-namespaces* t)

;;; `read-file-string` is a utility function to load RSS or Atom feeds
;;; from disk. This is mostly useful in testing, or perhaps for
;;; locally-stored feeds.
(defun read-file-string (path)
  (with-open-file (s path)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      data)))

;;; A predicate that returns true if the string is a byte string.
(defun byte-string-p (str)
  (and (arrayp str)
       (every #'integerp str)))

;;; A feed contains a title, a kind (either `:RSS` or `:ATOM`), a
;;; string containing the link to the feed's source, and a list of
;;; items retrieved from the feed.
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

;;; Overloading `describe-object` allows a more useful display of a
;;; feed.
(defmethod describe-object ((feed feed) stream)
  (let ((kind (cond ((eql (feed-kind feed) :rss) "RSS")
                    ((eql (feed-kind feed) :atom) "Atom")
                    (:else "Unknown"))))
    (format stream "~A (~A feed)~%Link: ~A~%Item count: ~A~%~{  Entry: ~A~%~}~%" 
            (feed-title feed) kind
            (feed-link feed)
            (length (feed-items feed))
            (mapcar #'item-title (feed-items feed)))))

;;; A feed item contains the entry's title, date, link, and body text.
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

;;; `describe-object` is also overloaded for a feed item to provide a
;;; more useful display.
(defmethod describe-object ((item feed-item) stream)
  (format stream "Entry: ~A (published ~A)~%Link: ~A~%Description:~%~A~%"
          (item-title item) (item-date item)
          (item-link item) (item-body item)))

;;; `make-feed-item` is a utility function that builds a feed from the
;;; title, date, link, and body.
(defun make-feed-item (title date link body)
  (make-instance 'feed-item
                 :title title
                 :date date
                 :link link
                 :body body))

;;; `parser-dispatch` loads the appropriate parser for Atom and RSS
;;; feeds.
(defun parser-dispatch (feed-xml)
    (cond
      ((assoc :|rss| feed-xml) (parse-rss feed-xml))
      ((assoc '|feed| feed-xml) (parse-atom feed-xml))
      ((assoc :|feed| feed-xml) (parse-atom feed-xml))
      (:else (error "Unknown feed type."))))

;;; `parse-feed` downloads the feed as a byte string and passes it off
;;; to be parsed.
(defun parse-feed (url)
  "Fetch the feed described in URL over HTTP, and parse the
feed. Returns a feed object."
  (parse-feed-string (drakma:http-request url)))

;;; In order to parse a string containing a feed, it must be parsed as
;;; XML and then handed off to `parser-dispatch`.
(defun parse-feed-string (str)
  "Parse the feed stored in the string or array of octets, returning a
feed object."
  (parser-dispatch
   (s-xml:parse-xml-string
    (cond
      ((stringp str) str)
      ((byte-string-p str) (flexi-streams:octets-to-string str))
      (:else (error "Unknown content-type."))))))

;;; Just give me the XML. This is useful in SLiME to understand the
;;; structure of the data.
(defun parse-xml (str)
  (s-xml:parse-xml-string
   (cond
     ((stringp str) str)
     ((byte-string-p str) (flexi-streams:octets-to-string str))
     (:else (error "Unknown content-type.")))))

;;; Let's take a look-see at this here XML. Or whatever.
(defun pretty-print-xml (url)
  (let ((str (drakma:http-request url)))
    (pprint
     (s-xml:parse-xml-string
      (cond
        ((stringp str) str)
        ((byte-string-p str) (flexi-streams:octets-to-string str))
        (:else (error "Unknown content-type.")))))))

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

(defun atom-entry-p (elt)
  (equalp (first elt) :|entry|))

(defun build-atom-entry (entry)
  (let ((body (rest entry)))
    (make-feed-item (get-xml-element :|title|     body)
                    (get-xml-element :|published| body)
                    (get-xml-element :|link|      body)
                    (get-xml-element :|summary|   body))))

;;; this is a horrifying hack. really need to find a more elegant way
;;; to do this!
(defun match-atom-title (elt)
  (and (listp elt)
       (listp (first elt))
       (equalp :|link| (first (first elt)))))

(defun parse-atom (feed-xml)
  (make-instance 'feed
                 :title (get-xml-element :|title| feed-xml)
                 :kind :atom
                 :link (third ; may cthulhu have mercy on my soul
                        (caar
                         (remove-if-not #'match-atom-title feed-xml)))
                 :items (mapcar #'build-atom-entry
                                (remove-if-not #'atom-entry-p
                                               (cdr feed-xml)))))
