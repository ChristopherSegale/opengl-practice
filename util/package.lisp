(defpackage :util
  (:use :cl)
  (:export :float-steps :create-gl-array :foreign-assign :compile-shader :run-window))

(in-package :util)

(declaim (inline float-steps
		 foreign-assign
		 compile-shader))
