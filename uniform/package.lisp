(defpackage :uniform
  (:use :cl)
  (:import-from :create-shader :create-shader)
  (:import-from :util :create-gl-array :foreign-assign :run-window)
  (:export :main))
