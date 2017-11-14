
(in-package :cl-user)
(defpackage trivial-package-manager-asd
  (:use :cl :asdf))
(in-package :trivial-package-manager-asd)


(defsystem trivial-package-manager
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria :trivial-features)
  :pathname "src"
  :components ((:file "package"))
  :description "Functions for installing packages from distro-specific package manager."
  :in-order-to ((test-op (test-op :trivial-package-manager.test))))
