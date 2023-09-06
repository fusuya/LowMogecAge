(in-package :cl)

(defpackage lowmogecage
  (:use :cl)
  (:local-nicknames (#:gk #:trivial-gamekit)
                   (#:math #:bodge-math))
  (:export :moge
	   :atk-dir))
