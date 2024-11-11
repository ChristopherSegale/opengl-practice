(in-package :square)

(defvar *vertex-source* (create-shader (:version 330)
				       (:in (:layout (:location 0)) :vec2 v-position)
				       (:in (:layout (:location 1)) :vec3 in-color)
				       (:out :vec4 out-color)
				       (:function :void main nil
						  (:set "gl_Position" (:vec4 v-position 0 1.0))
						  (:set out-color (:vec4 in-color 1.0)))))

(defvar *fragment-source* (create-shader (:version 330)
					 (:in :vec4 out-color)
					 (:out :vec4 frag-color)
					 (:function :void main nil
						    (:set frag-color out-color))))

                   ;vertex  ;color
(defvar *inputs* #(-0.5 0.5 1.0 0.5 0.2   ;top-left point
                   0.5 0.5 1.0 0.5 0.2    ;top-right point
		   0.5 -0.5 1.0 0.5 0.2   ;bottom-right point
		   0.5 -0.5 1.0 0.5 0.2   ;bottom-right point
		   -0.5 -0.5 1.0 0.5 0.2  ;bottom-left point
		   -0.5 0.5 1.0 0.5 0.2)) ;top-left point

(defun create-shader-program ()
  (let ((vs (gl:create-shader :vertex-shader))
	(fs (gl:create-shader :fragment-shader))
	(program (gl:create-program)))

    ;load and compile shader sources
    (gl:shader-source vs *vertex-source*)
    (gl:compile-shader vs)
    (gl:shader-source fs *fragment-source*)
    (gl:compile-shader fs)

    ;attach shaders to programs and return shader program
    (gl:attach-shader program vs)
    (gl:attach-shader program fs)
    (gl:link-program program)
    program))

;;Initialize opengl and return buffer object
(defun initialize-gl ()
  (gl:clear-color 1 1 1 1)
  (gl:clear :color-buffer-bit)
  (let ((position-buffer-object (car (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer position-buffer-object)
    (create-gl-array *inputs*)
    (gl:bind-buffer :array-buffer 0)
    (gl:use-program (create-shader-program))
    position-buffer-object))

(defun assign-vertex-attributes (buffer)
  (gl:bind-buffer :array-buffer buffer)
  (gl:enable-vertex-attrib-array 0)
  (gl:enable-vertex-attrib-array 1)
  (gl:vertex-attrib-pointer 0 2 :float :false (float-steps 5) 0)
  (gl:vertex-attrib-pointer 1 3 :float :false (float-steps 5) (float-steps 2)))

(defun rendering-code (win)
  (gl:clear :color-buffer-bit)
  (gl:draw-arrays :triangles 0 6)
  (sdl2:gl-swap-window win))

(defun main ()
  (run-window :sdl2-init-flags (:video :timer)
	      :title "Square"
	      :buffer-object-value (initialize-gl)
	      :render-function (lambda (b w)
				 (assign-vertex-attributes b)
				 (rendering-code w))
	      :render-args (util::buffer util::win)))
