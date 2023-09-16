(in-package :cl)

(defpackage lowmogecage
  (:use :cl)
  (:local-nicknames (#:gk #:trivial-gamekit)
		    (#:bg #:cl-bodge)
                   (#:math #:bodge-math))
  (:export :moge
	   :atk-dir))
