(asdf:defsystem "opengl-practice"
  :description "Various programs written to practice using modern OpenGL with Common Lisp"
  :version "0.1"
  :author "Christopher Segale"
  :license "MIT"
  :depends-on (:sdl2 :cl-opengl :create-shader)
  :serial t
  :components ((:module "util"
		:components ((:file "package")
			     (:file "util" :depends-on ("package"))))
	       (:module "triangle" :depends-on ("util")
		:components ((:file "package")
			     (:file "triangle" :depends-on ("package"))))
	       (:module "square" :depends-on ("util")
		:components ((:file "package")
			     (:file "square" :depends-on ("package"))))
	       (:module "geometry" :depends-on ("util")
		:components ((:file "package")
			     (:file "geometry" :depends-on ("package"))))))
