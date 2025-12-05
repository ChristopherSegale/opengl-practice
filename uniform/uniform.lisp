(in-package :uniform)

(defvar *vertex-source* "#version 330
layout (location = 0) in vec2 vPos;
uniform mat4 projection;

void main()
{
    gl_Position = projection * vec4(vPos, 0.0, 1.0);
}
")

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
(defun initialize-gl (transformation)
  (let (gl-program u-location position-buffer-object)
    (gl:clear-color 1 1 1 1)
    (gl:clear :color-buffer-bit)
    (setf position-buffer-object (first (gl:gen-buffers 1)))
    (gl:bind-buffer :array-buffer position-buffer-object)
    (create-gl-array *vertices*)
    (gl:bind-buffer :array-buffer 0)
    (setf gl-program (create-shader-program))
    (gl:use-program gl-program)
    (setf u-location (gl:get-uniform-location gl-program "projection"))
    (if (= -1 u-location)
	(error "Uniform ~a not found in program ~a" "projection" gl-program)
	(gl:uniform-matrix-4fv u-location transformation nil))
    position-buffer-object))

(defun main ()
  (run-window :sdl2-init-flags (:video :timer)
  	      :title "Uniform"
	      :buffer-object-value (initialize-gl #(1.0 0.0 0.0 0.0
						    0.0 1.0 0.0 0.0
						    0.0 0.0 1.0 0.0
						    0.0 0.0 0.0 1.0))
	      :render-function (lambda (b w)
				 (gl:bind-buffer :array-buffer b)
				 (gl:enable-vertex-attrib-array 0)
				 (gl:vertex-attrib-pointer 0 2 :float :false 0 0)
				 (gl:clear :color-buffer-bit)
				 (gl:draw-arrays :triangles 0 3)
				 (sdl2:gl-swap-window w))
	      :render-args (util::buffer util::win)))
