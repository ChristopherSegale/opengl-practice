(in-package :triangle)

(defvar *vertex-source* (create-shader
			  (:version 330)
			  (:in (:layout (:location 0)) :vec2 v-position)
			  (:function :void main nil
				     (:set "gl_Position" (:vec4 v-position 0.0 1.0)))))

(defvar *fragment-source* (create-shader
			    (:version 330)
			    (:out :vec4 frag-color)
			    (:function :void main nil
				       (:set frag-color (:vec4 1.0 0.5 0.2 1.0)))))

(defvar *vertices* #(-0.5 -0.5
		     0.5 -0.5
		     0.0 0.5))

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
  (let ((position-buffer-object (first (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer position-buffer-object)
    (create-gl-array *vertices*)
    (gl:bind-buffer :array-buffer 0)
    (gl:use-program (create-shader-program))
    position-buffer-object))

(defun main ()
  (run-window :sdl2-init-flags (:video :timer)
	      :title "Triangle"
	      :buffer-object-value (initialize-gl)
	      :render-function (lambda (b w)
				 (gl:bind-buffer :array-buffer b)
				 (gl:enable-vertex-attrib-array 0)
				 (gl:vertex-attrib-pointer 0 2 :float :false 0 0)
				 (gl:clear :color-buffer-bit)
				 (gl:draw-arrays :triangles 0 3)
				 (sdl2:gl-swap-window w))
	      :render-args (util::buffer util::win)))
