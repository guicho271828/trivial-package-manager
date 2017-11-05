#|
  This file is a part of trivial-package-manager project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage trivial-package-manager.test-asd
  (:use :cl :asdf))
(in-package :trivial-package-manager.test-asd)


(defsystem trivial-package-manager.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of trivial-package-manager"
  :license "LLGPL"
  :depends-on (:trivial-package-manager
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval
 (read-from-string
  "(5am:run! :trivial-package-manager)"))
))
