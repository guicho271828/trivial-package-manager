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
   #:do-install
   #:which
   #:pkg-config))
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

(defun ensure-program (program &rest rest &key apt dnf yum pacman yaourt brew choco from-source)
  "PROGRAM is a program name to be checked by WHICH command.
Each keyword argument specifies the package names to be passed on to the corresponding package manager.
The value of each argument can be a string or a list of strings.

If none of the package managers are available / when the package is missing (e.g. older distro),
FROM-SOURCE argument can specify the shell command for fetching/building/installing the program from
the source code. The command is executed in the *default-pathname-defaults*,
 so care must be taken to bind the appropriate value to the variable.

Example:

 (ensure-program \"gnome-mines\" :apt \"gnome-mines\")
 (ensure-program \"gnome-mines\" :apt '(\"gnome-mines\")) ; both are ok
"
  (declare (ignorable apt dnf yum pacman yaourt brew choco from-source))
  (if (which program)
      (format t "~&~a is already installed.~%" program)
      (apply #'do-install rest)))

(defun ensure-library (library &rest rest &key apt dnf yum pacman yaourt brew choco from-source)
  "Library is a shared library name to be checked by PKG-CONFIG command.
Each keyword argument specifies the package names to be passed on to the corresponding package manager.
The value of each argument can be a string or a list of strings.

If none of the package managers are available / when the package is missing (e.g. older distro),
FROM-SOURCE argument can specify the shell command for fetching/building/installing the program from
the source code. The command is executed in the *default-pathname-defaults*,
 so care must be taken to bind the appropriate value to the variable.

Example:

 (ensure-library \"libcurl\" :apt \"libcurl4-openssl-dev\")
 (ensure-library \"libcurl\" :apt '(\"libcurl4-openssl-dev\")) ; both are ok
"
  (declare (ignorable apt dnf yum pacman yaourt brew choco from-source))
  (if (pkg-config library)
      (format t "~&~a is already installed.~%" library)
      (apply #'do-install rest)))

(defun %run (args)
  "Simple wrapper for run-program, input/error/output are inherited."
  (uiop:run-program args
                    :output :interactive
                    :error-output :interactive
                    :input :interactive))

(defun do-install (&key apt dnf yum pacman yaourt brew choco from-source)
  "Install the specified packages when the corresponding package manager is present in the system.
Managers are detected simply by `which` command."
  (declare (ignorable apt dnf yum pacman yaourt brew choco from-source))
  (macrolet ((try-return (form)
               `(handler-case (return ,form)
                  (uiop:subprocess-error (c)
                    (warn "Command failed with code ~a: ~a"
                          (uiop:subprocess-error-code c)
                          (uiop:subprocess-error-command c))))))
    (block nil
      #+unix
      (when (and apt (which "apt-get"))
        (try-return (%run (sudo `("apt-get" "install" "-y" ,@(ensure-list apt))))))
      #+unix
      (when (and dnf (which "dnf"))
        (try-return (%run (sudo `("dnf" "install" "-y" ,@(ensure-list dnf))))))
      #+unix
      (when (and yum (which "yum"))
        (try-return (%run (sudo `("yum" "install" "-y" ,@(ensure-list yum))))))
      #+unix
      (when (and yaourt (which "yaourt"))
        (try-return (%run (sudo `("yaourt" "-S" "--noconfirm" ,@(ensure-list yaourt))))))
      #+unix
      (when (and pacman (which "pacman"))
        (try-return (%run (sudo `("packman" "-S" "--noconfirm" ,@(ensure-list pacman))))))
      #+(or unix dawrin bsd)
      (when (and brew (which "brew"))
        (try-return (%run `("brew" "install" ,@(ensure-list brew)))))
      #+windows
      (when (and choco (which "choco"))
        (try-return (%run `("choco" "install" ,@(ensure-list choco)))))
      (when from-source
        (try-return (%run `("sh" "-c" ,@(ensure-list brew)))))
      (t (error "none of the installation options are available! Supported packaging systems:~%~a"
                '(:apt :dnf :yum :pacman :yaourt :brew :choco))))))

