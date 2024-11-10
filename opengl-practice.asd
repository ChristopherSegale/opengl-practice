(asdf:defsystem "opengl-practice"
  :description "Various programs written to practice using modern OpenGL with Common Lisp"
  :version "0.1"
  :author "Christopher Segale"
  :license "MIT"
  :depends-on (:sdl2 :cl-opengl :create-shader)
  :serial t
  :components ((:module "triangle"
		:components ((:file "package")
			     (:file "triangle" :depends-on ("package"))))
	       (:module "square"
		:components ((:file "package")
			     (:file "square" :depends-on ("package"))))
	       (:module "geometry"
		:components ((:file "package")
			     (:file "geometry" :depends-on ("package"))))))
