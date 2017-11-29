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
   #:pkg-config
   #:browse-package
   #:*search-engines*))
(in-package :trivial-package-manager)

;; blah blah blah.

(defun %run-p (args)
  (ignore-errors
    (progn (uiop:run-program args) t)))

(defun which (name)
  (%run-p `("which" ,(string-downcase (string name)))))

(assert (which "which") nil "This system does not have a 'which' command required by trivial-package-manager.")

(defun pkg-config (name)
  (assert (which "pkg-config") nil "This system does not have a 'pkg-config' command required by trivial-package-manager.")
  (%run-p `("pkg-config" ,(string-downcase (string name)))))

(defun sudo (commands)
  (cond ((and (which "gksudo") (uiop:getenv "DISPLAY"))
         `("gksudo" ;; "-d" ,(format nil "Installing packages via: ~a" commands)
           ,(format nil "~{~a ~}" commands)))
        ((and (which "sudo") (%run-p '("sudo" "-v")))
         `("sudo" "sh" "-c" ,(format nil "~{~a ~}" commands)))
        (t
         (warn "you don't have sudo, right? Trying to run it without sudo")
         ;; Directly executing a program produces a different error,
         ;; which does not produce subprocess-error which can be captured
         #+(or)
         `(,(format nil "~{~a ~}" commands))
         ;; so we wrap it in a shell process
         `("sh" "-c" ,(format nil "~{~a ~}" commands)))))

(defun call-with-env (env-alist fn)
  (let ((oldvals (mapcar (lambda (pair)
                           (destructuring-bind (var . newval) pair
                             (prog1 (uiop:getenv var)
                               (setf (uiop:getenv var) newval))))
                         env-alist)))
    (unwind-protect
         (funcall fn)
      (mapcar (lambda (pair oldval)
                (setf (uiop:getenv (car pair)) oldval))
              env-alist
              oldvals))))

(defun ensure-program (program &rest rest &key apt dnf yum pacman yaourt brew macports fink choco from-source env-alist)
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
  (declare (ignorable apt dnf yum pacman yaourt brew macports fink choco from-source))
  (call-with-env env-alist
                 (lambda ()
                   (if (which program)
                       (format t "~&~a is already installed.~%" program)
                       (apply #'do-install rest)))))


(defun ensure-library (library &rest rest &key apt dnf yum pacman yaourt brew macports fink choco from-source env-alist)
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
  (declare (ignorable apt dnf yum pacman yaourt brew macports fink choco from-source))
  (call-with-env env-alist
                 (lambda ()
                   (if (pkg-config library)
                       (format t "~&~a is already installed.~%" library)
                       (apply #'do-install rest)))))

(defun %run (args)
  "Simple wrapper for run-program, input/error/output are inherited."
  (uiop:run-program args
                    :output :interactive
                    :error-output :interactive
                    :input :interactive))

(defun do-install (&key apt dnf yum pacman yaourt brew macports fink choco from-source)
  "Install the specified packages when the corresponding package manager is present in the system.
Managers are detected simply by `which` command."
  (declare (ignorable apt dnf yum pacman yaourt brew macports fink choco from-source))
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
      #+(or unix darwin)
      (when (and brew (which "brew"))
        (dolist (str (ensure-list brew))
          (let ((slashes (count #\/ str)))
            (case slashes
              (0 (try-return (%run `("brew" "install" ,str))))
              (1 (error "Invalid number of /'s in the formula specifier!~%~
                         Data: ~a~%~
                         Expecting <user>/<repo>/<formula> "
                        str))
              (t (let* ((pos (position #\/ str))
                        (pos2 (position #\/ str :start (1+ pos)))
                        (pos3 (position #\Space str)))
                   (assert (or (null pos3) (< pos2 pos3))
                           nil
                           "Found a whitespace before the second / !~%~
                            Data: ~a~%~
                            Expecting:~%~
  '<user>/<repo>/<formula>' or
  '<user>/<repo>/<formula> <URL>'"
                           str)
                   (try-return (progn (if pos3
                                          ;; url
                                          (%run `("brew" "tap" ,(subseq str 0 pos2) ,(subseq str (1+ pos3))))
                                          (%run `("brew" "tap" ,(subseq str 0 pos2))))
                                      (%run `("brew" "install" ,str))))))))))
      #+(or darwin)
      (when (and macports (which "port"))
        (try-return (%run (sudo `("port" "install" ,@(ensure-list macports))))))
      #+(or darwin)
      (when (and fink (which "fink"))
        (try-return (%run `("fink" "install" ,@(ensure-list fink)))))
      #+windows
      (when (and choco (which "choco"))
        (try-return (%run `("choco" "install" ,@(ensure-list choco)))))
      (when from-source
        (warn "Installing from the source file / tarball. Requries the build requisites.")
        (try-return (%run `("sh" "-c" ,@(ensure-list from-source)))))
      (error "none of the installation options are available! Supported packaging systems:~%~a"
             '(:apt :dnf :yum :pacman :yaourt :brew :macports :fink :choco)))))

(defvar *search-engines*
  '("http://formulae.brew.sh/search/~a" ; brew
    "https://packages.ubuntu.com/search?keywords=~a&suite=artful&section=all&searchon=all&arch=any" ;ubuntu
    "https://packages.debian.org/search?keywords=~a&suite=stable&section=all&searchon=all" ;debian
    "https://www.archlinux.jp/packages/?name=~a" ;arch
    "https://admin.fedoraproject.org/pkgdb/packages/%2A~a%2A/" ;fedora
    "https://pkgs.org/download/~a" ;rpm
    "https://chocolatey.org/packages?q=~a" ; chocolatey
    "https://www.macports.org/ports.php?by=name&substr=~a" ; ports
    "http://pdb.finkproject.org/pdb/browse.php?summary=~a" ; fink
    ))
    
(defun browse-package (query-string)
  "Query-String is a string designator.
Open several package search engines on a browser"
  (dolist (url *search-engines*)
    (trivial-open-browser:open-browser
     (format nil url query-string))))

