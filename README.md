
# Trivial-Package-Manager

This library provides two simple functions which auto-detects the distro-specific package manager and
install the binary package.

## Usage

    Package trivial-package-manager:
    
    ensure-program (program &key apt dnf yum pacman yaourt brew choco)
    ensure-library (library &key apt dnf yum pacman yaourt brew choco)
    do-install             (&key apt dnf yum pacman yaourt brew choco)

    PROGRAM is a program name (string) to be checked by `which` command.
    LIBRARY is a library name (string) to be checked by `pkg-config` command. Includes "lib" i.e. libcurl.

    Each keyword argument specifies the package names to be passed on to the corresponding package manager.
    The value of each argument can be a string or a list of strings.

    Specified packages are installed when the program/library is missing.
    DO-INSTALL installs the packages unconditionally.
    
    It uses `gksudo` or `sudo` when necessary, and may ask the user of passwords.
    
    Example:
    
     (ensure-program "gnome-mines" :apt "gnome-mines")
     (ensure-program "gnome-mines" :apt '("gnome-mines")) ; both are ok
     (ensure-library "libcurl" :apt "libcurl4-openssl-dev"
                               :dnf "curl"
                               :yum "curl"
                               :pacman "curl"
                               :brew "curl")

## Using it in the asdf system

ASDF integration is not included yet, but you can do this:

```common-lisp
(defsystem trivial-package-manager.dummy
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of trivial-package-manager"
  :license "LLGPL"
  :defsystem-depends-on (:trivial-package-manager)
  :perform (load-op :before (op c)
                    (uiop:symbol-call :trivial-package-manager
                                      :ensure-program
                                      "gnome-mines" :apt "gnome-mines")))
```


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


