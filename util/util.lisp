(in-package :util)

(defvar *width* 800)
(defvar *height* 600)

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
	      
