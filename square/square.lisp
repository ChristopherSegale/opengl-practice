(in-package :square)

(defvar *width* 800)
(defvar *height* 600)
(defvar *float-size* 4)

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

(defun float-steps (n)
  (* n *float-size*))

;;Allocate and store buffer data
(defun create-gl-array (cl-vert)
  (let ((arr (gl:alloc-gl-array :float (length cl-vert))))
    (dotimes (i (length cl-vert))
      (setf (gl:glaref arr i) (aref cl-vert i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)))

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
  (gl:vertex-attrib-pointer 1 3 :float :false (float-steps 6) (float-steps 3)))

(defun rendering-code (win)
  (gl:clear :color-buffer-bit)
  (gl:draw-arrays :triangles 0 6)
  (sdl2:gl-swap-window win))

(defun main ()
  (sdl2:with-init (:video :timer)
    (sdl2:with-window (win :title "Square" :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-con win)
	(let ((position-buffer-object (initialize-gl)))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:idle ()
		   (assign-vertex-attributes position-buffer-object)
		   (rendering-code win))
	    (:quit () t)))))))
