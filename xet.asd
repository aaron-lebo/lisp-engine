;;;; xet.asd

(asdf:defsystem #:xet
  :description "Describe xet here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:sdl2
               #:cl-opengl)
  :serial t
  :components ((:file "package")
               (:file "xet")))

