#|
  This file is a part of trivial-package-manager project.
  Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage trivial-package-manager
  (:use :cl :alexandria)
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

(defun sudo (commands)
  (cond ((and (which "gksudo") (uiop:getenv "DISPLAY"))
         `("gksudo" ;; "-d" ,(format nil "Installing packages via: ~a" commands)
           ,(format nil "~{~a ~}" commands)))
        ((which "sudo")
         `("sudo" ,(format nil "~{~a ~}" commands)))
        (t
         (error "you don't have sudo right?"))))

(defun ensure-program (program &rest rest &key apt dnf yum pacman yaourt brew choco)
  "PROGRAM is a program name to be checked by WHICH command.
Each keyword argument specifies the package names to be passed on to the corresponding package manager.
The value of each argument can be a string or a list of strings.

Example:

 (ensure-program \"gnome-mines\" :apt \"gnome-mines\")
 (ensure-program \"gnome-mines\" :apt '(\"gnome-mines\")) ; both are ok
"
  (declare (ignorable apt dnf yum pacman yaourt brew choco))
  (if (which program)
      (format t "~&~a is already installed.~%" program)
      (apply #'do-install rest)))

(defun ensure-library (library &rest rest &key apt dnf yum pacman yaourt brew choco)
  "Library is a shared library name to be checked by PKG-CONFIG command.
Each keyword argument specifies the package names to be passed on to the corresponding package manager.
The value of each argument can be a string or a list of strings.

Example:

 (ensure-library \"libcurl\" :apt \"libcurl4-openssl-dev\")
 (ensure-library \"libcurl\" :apt '(\"libcurl4-openssl-dev\")) ; both are ok
"
  (declare (ignorable apt dnf yum pacman yaourt brew choco))
  (if (pkg-config library)
      (format t "~&~a is already installed.~%" program)
      (apply #'do-install rest)))

(defun %run (args)
  "Simple wrapper for run-program, input/error/output are inherited."
  (uiop:run-program args
                    :output :interactive
                    :error-output :interactive
                    :input :interactive))

(defun do-install (&key apt dnf yum pacman yaourt brew choco)
  "Install the specified packages when the corresponding package manager is present in the system.
Managers are detected simply by `which` command."
  (declare (ignorable apt dnf yum pacman yaourt brew choco))
  (cond
    #+unix
    ((which "apt-get") (%run (sudo `("apt-get" "install" "-y" ,@(ensure-list apt)))))
    #+unix
    ((which "dnf") (%run (sudo `("dnf" "install" "-y" ,@(ensure-list dnf)))))
    #+unix
    ((which "yum") (%run (sudo `("yum" "install" "-y" ,@(ensure-list yum)))))
    #+unix
    ((which "yaourt") (%run (sudo `("yaourt" "-S" "--noconfirm" ,@(ensure-list yaourt)))))
    #+unix
    ((which "pacman") (%run (sudo `("packman" "-S" "--noconfirm" ,@(ensure-list pacman)))))
    #+(or unix osx)
    ((which "brew") (%run `("brew" "install" ,@(ensure-list brew))))
    #+windows
    ((which "choco") (%run `("choco" "install" ,@(ensure-list choco))))
    (t (error "none of the installation options are available! Supported packaging systems:~%~a"
              '(:apt :dnf :yum :pacman :yaourt :brew :choco)))))

