#|
  This file is a part of feedparse project.
  Copyright (c) 2014 K. Isom (kyle@tyrfingr.is)
|#

(in-package :cl-user)
(defpackage feedparse-test-asd
  (:use :cl :asdf))
(in-package :feedparse-test-asd)

(defsystem feedparse-test
  :author "K. Isom"
  :license "ISC"
  :depends-on (:feedparse
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "feedparse"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
