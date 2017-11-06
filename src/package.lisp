#|
  This file is a part of trivial-package-manager project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivial-package-manager
  (:use :cl :trivia :alexandria :iterate)
  (:export
   #:ensure-program
   #:ensure-library
   #:do-install))
(in-package :trivial-package-manager)

;; blah blah blah.

(defun which (name)
  (zerop
   (nth-value
    2
    (uiop:run-program `("which" ,(string-downcase (string name)))
                      :ignore-error-status t))))

(assert (which "which") nil "This system does not have a 'which' command required by trivial-package-manager.")

(defun pkg-config (name)
  (assert (which "pkg-config") nil "This system does not have a 'pkg-config' command required by trivial-package-manager.")
  (zerop
   (nth-value
    2
    (uiop:run-program `("pkg-config" ,(string-downcase (string name)))
                      :ignore-error-status t))))

(defun sudo ()
  (cond ((and (which "gksudo") (uiop:getenv "DISPLAY")) "gksudo")
        ((which "sudo") "sudo")
        (t
         (error "you don't have sudo right?"))))

(defun ensure-program (program &rest rest &key apt dnf yum pacman yaourt brew choco)
  "PROGRAM is a program name to be checked by WHICH command.
 Each keyword argument specifies the package names to be passed on to the corresponding package manager."
  (declare (ignorable apt dnf yum pacman yaourt brew choco))
  (unless (which program)
    (apply #'do-install rest)))

(defun ensure-library (library &rest rest &key apt dnf yum pacman yaourt brew choco)
  "Library is a shared library name to be checked by PKG-CONFIG command.
 Each keyword argument specifies the package names to be passed on to the corresponding package manager."
  (declare (ignorable apt dnf yum pacman yaourt brew choco))
  (unless (pkg-config program)
    (apply #'do-install rest)))

(defun do-install (&key apt dnf yum pacman yaourt brew choco)
  (cond
    #+unix
    ((which "apt") (uiop:run-program `(,(sudo) "apt-get" "install" "-y" ,@apt) :output t :error-output t :input t))
    #+unix
    ((which "dnf") (uiop:run-program `(,(sudo) "dnf" "install" "-y" ,@dnf) :output t :error-output t :input t))
    #+unix
    ((which "yum") (uiop:run-program `(,(sudo) "yum" "install" "-y" ,@yum) :output t :error-output t :input t))
    #+unix
    ((which "yaourt") (uiop:run-program `(,(sudo) "yaourt" "-S" "--noconfirm" ,@yaourt) :output t :error-output t :input t))
    #+unix
    ((which "pacman") (uiop:run-program `(,(sudo) "packman" "-S" "--noconfirm" ,@pacman) :output t :error-output t :input t))
    #+(or unix osx)
    ((which "brew") (uiop:run-program `("brew" "install" ,@brew) :output t :error-output t :input t))
    #+windows
    ((which "choco") (uiop:run-program `("choco" "install" ,@choco) :output t :error-output t :input t))
    (t (error "none of the installation options are available! Supported packaging systems:~%~a"
              '(:apt :dnf :yum :pacman :yaourt :brew :choco)))))

