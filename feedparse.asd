#|
  This file is a part of feedparse project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

#|
  Utility for parsing Atom and RSS feeds.

  Author: K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage feedparse-asd
  (:use :cl :asdf))
(in-package :feedparse-asd)

(defsystem feedparse
  :version "0.1"
  :author "K. Isom"
  :license "ISC"
  :depends-on (:s-xml :flexi-streams :drakma)
  :components ((:module "src"
                :components
                ((:file "feedparse"))))
  :description "Utility for parsing Atom and RSS feeds."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op feedparse-test))))
