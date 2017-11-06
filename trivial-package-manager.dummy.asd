
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
