
;; Work in progress. Perhaps never materialize...

(defpackage trivial-package-manager.asdf
  (:use :cl :trivia :alexandria :iterate :trivial-package-manager :asdf))
(in-package :trivial-package-manager.asdf)

(defclass asdf::os-package (component)
  ((apt :initarg :apt :initform nil)
   (dnf :initarg :dnf :initform nil)
   (yum :initarg :yum :initform nil)
   (pacman :initarg :pacman :initform nil)
   (yaourt :initarg :yaourt :initform nil)
   (brew :initarg :brew :initform nil)
   (choco :initarg :choco :initform nil)))

(defmethod component-pathname ((c asdf::os-package))
  #p"")

(defclass asdf::os-packaged-program (asdf::os-package) ())
(defclass asdf::os-packaged-library (asdf::os-package) ())

(defmethod perform ((operation load-op) (component asdf::os-packaged-program))
  (with-slots (name apt dnf yum pacman yaourt brew choco) component
    (ensure-program name :apt apt :dnf dnf :yum yum :pacman pacman :yaourt yaourt :brew brew :choco choco)))

(defmethod perform ((operation load-op) (component asdf::os-packaged-library))
  (with-slots (name apt dnf yum pacman yaourt brew choco) component
    (ensure-program name :apt apt :dnf dnf :yum yum :pacman pacman :yaourt yaourt :brew brew :choco choco)))

