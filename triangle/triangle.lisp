(in-package :triangle)

(defvar *width* 800)
(defvar *height* 600)

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
  (let ((position-buffer-object (first (gl:gen-buffers 1))))
    (gl:bind-buffer :array-buffer position-buffer-object)
    (create-gl-array *vertices*)
    (gl:bind-buffer :array-buffer 0)
    (gl:use-program (create-shader-program))
    position-buffer-object))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Triangle" :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-con win)
	(let ((position-buffer-object (initialize-gl)))
	  (sdl2:with-event-loop (:method :poll)
	    (:keyup
	     (:keysym keysym)
	     (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	       (sdl2:push-event :quit)))
	    (:idle ()
		   (gl:bind-buffer :array-buffer position-buffer-object)
		   (gl:enable-vertex-attrib-array 0)
		   (gl:vertex-attrib-pointer 0 2 :float :false 0 0)
		   (gl:draw-arrays :triangles 0 3)
		   (sdl2:gl-swap-window win))
	    (:quit () t)))))))
