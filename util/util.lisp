(in-package :util)

(defvar *width* 800)
(defvar *height* 600)

(defvar *float-size* 4)

(defun float-steps (n)
  (* n *float-size*))

;;Allocate and store buffer data
(defun create-gl-array (cl-vert)
  (let ((arr (gl:alloc-gl-array :float (length cl-vert))))
    (dotimes (i (length cl-vert))
      (setf (gl:glaref arr i) (aref cl-vert i)))
    (gl:buffer-data :array-buffer :static-draw arr)
    (gl:free-gl-array arr)))

(defun compile-shader (shader source)
  (gl:shader-source shader source)
  (gl:compile-shader shader))

(defmacro run-window (&key (sdl2-init-flags '(:everything))
			(window 'win)
			(title "example")
			(sdl2-window-flags '(:shown :opengl))
			(gl-context 'gl-con)
			(buffer-object-name 'buffer)
			buffer-object-value
			render-function
			(render-args '(win)))
  (let ((keysym (gensym "keysym")))
    `(sdl2:with-init ,sdl2-init-flags
       (sdl2:with-window (,window :title ,title :w *width* :h *height* :flags ',sdl2-window-flags)
	 (sdl2:with-gl-context (,gl-context ,window)
	   (let ((,buffer-object-name ,buffer-object-value))
	     (sdl2:with-event-loop (:method :poll)
	       (:keyup
		(:keysym ,keysym)
		(when (sdl2:scancode= (sdl2:scancode-value ,keysym) :scancode-escape)
		  (sdl2:push-event :quit)))
	       (:idle ()
		      (funcall ,render-function ,@render-args))
	       (:quit () t))))))))
	      
