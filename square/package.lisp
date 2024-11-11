(defpackage :square
  (:use :cl)
  (:import-from :create-shader :create-shader)
  (:import-from :util :float-steps :create-gl-array :run-window)
  (:export :main))
