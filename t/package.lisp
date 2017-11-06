#|
  This file is a part of trivial-package-manager project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :trivial-package-manager.test
  (:use :cl
        :trivial-package-manager
        :fiveam :alexandria))
(in-package :trivial-package-manager.test)



(def-suite :trivial-package-manager)
(in-suite :trivial-package-manager)

;; run test with (run! test-name) 

(test trivial-package-manager
  )



