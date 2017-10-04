(asdf:defsystem #:xet
  :description "Describe xet here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cl-opengl
               #:sdl2)
  :serial t
  :components ((:file "package")
               (:file "xet")))
