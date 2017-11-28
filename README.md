
# Trivial-Package-Manager

This library provides two simple functions for detecting an available package manager and
install the binary package. Paired with ASDF, it solves the problem of external library dependency.

## Use it in a ASDF system definition

Sophisticated ASDF integration is still on the way, but you can do this, for example:

```common-lisp
(defsystem cl-sat.minisat
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:trivia :alexandria :iterate :cl-sat)
  :components ((:module "src"
                        :components
                        ((:file "package"))))
  :description "Common Lisp API to minisat"
  :in-order-to ((test-op (test-op :cl-sat.minisat.test)))
  :defsystem-depends-on (:trivial-package-manager)
  :perform
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "minisat"
                             :apt "minisat"
                             :dnf "minisat2"
                             :yum "minisat2"
                             :brew "minisat"
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :cl-sat.minisat)))))
```

## API

    Package trivial-package-manager:
    
    ensure-program (program &key apt dnf yum pacman yaourt brew choco from-source)
    ensure-library (library &key apt dnf yum pacman yaourt brew choco from-source)
    do-install             (&key apt dnf yum pacman yaourt brew choco from-source)
    browse-package (query-string)

    PROGRAM is a program name (string) to be checked by `which` command.
    LIBRARY is a library name (string) to be checked by `pkg-config` command. Includes "lib" i.e. libcurl.

    Each keyword argument specifies the package names to be passed on to the corresponding package manager.
    The value of each argument can be a string or a list of strings.

    Specified packages are installed when the program/library is missing.
    DO-INSTALL installs the packages unconditionally.
    
    It uses `gksudo` or `sudo` when necessary, and may ask the user of passwords.
    
    If none of the package managers are available / when the package is missing (e.g. older distro),
    FROM-SOURCE argument can specify the shell command for fetching/building/installing the program from
    the source code. The command is executed in the *default-pathname-defaults*,
    so care must be taken to bind the appropriate value to the variable.

    Example:
    
     (ensure-program "gnome-mines" :apt "gnome-mines")
     (ensure-program "gnome-mines" :apt '("gnome-mines")) ; both are ok
     (ensure-library "libcurl" :apt "libcurl4-openssl-dev"
                               :dnf "curl"
                               :yum "curl"
                               :pacman "curl"
                               :brew "curl")



## Dependencies
This library is at least tested on implementation listed below:

+ SBCL 1.4.0 on X86-64 Linux 4.10.0-38-generic (author's environment)
+ CCL 1.11

Also, it depends on the following libraries:

+ alexandria by *Nikodemus Siivola <nikodemus@sb-studio.net>, and others.* :
    Alexandria is a collection of portable public domain utilities.

## Installation

## Author

* Masataro Asai (guicho2.71828@gmail.com)

## Copyright

Copyright (c) 2017 Masataro Asai (guicho2.71828@gmail.com)

# License

Licensed under the LLGPL License.


