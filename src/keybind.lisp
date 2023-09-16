(in-package :lowmogecage)

(defun bind-mouse-event ()
  (with-slots ((mouse-x x) (mouse-y y) left right) *mouse*
    (gamekit:bind-cursor (lambda (x y)
			   "Save cursor position"
			   (setf mouse-x (/ x *scale-w*)
				 mouse-y (/ y *scale-h*))))
				 ;;x-for-obj (/ mouse-x *scale-obj-w*)
				 ;;y-for-obj (/ mouse-y *scale-obj-h*))))
    (gk:bind-button :mouse-left :pressed
		    (lambda ()
                      ;;(print "nnggngn")
		      (setf left t)))
    (gk:bind-button :mouse-left :released
		    (lambda ()
		      (setf left nil)))
    (gk:bind-button :mouse-right :pressed
		    (lambda ()
		      (setf right t)))
    (gk:bind-button :mouse-right :released
		    (lambda ()
		      (setf right nil)))))

(defun bind-key-event ()
  (with-slots (space1 a key1 key2 key3 key4 key5 key6 key7 key8 key9 key0 w s d c z x v b) *keystate*
    (gk:bind-button :space :pressed
		    (lambda ()
		      (setf space1 t)))
    (gk:bind-button :space :released
		    (lambda ()
		      (setf space1 nil)))
    (gk:bind-button :z :pressed
		    (lambda ()
		      (setf z t)))
    (gk:bind-button :z :released
		    (lambda ()
		      (setf z nil)))
    (gk:bind-button :v :pressed
		    (lambda ()
		      (setf v t)))
    (gk:bind-button :v :released
		    (lambda ()
		      (setf v nil)))
    (gk:bind-button :b :pressed
		    (lambda ()
		      (setf b t)))
    (gk:bind-button :b :released
		    (lambda ()
		      (setf b nil)))
    (gk:bind-button :x :pressed
		    (lambda ()
		      (setf x t)))
    (gk:bind-button :x :released
		    (lambda ()
		      (setf x nil)))
    (gk:bind-button :a :pressed
		    (lambda ()
		      (setf a t)))
    (gk:bind-button :a :released
		    (lambda ()
		      (setf a nil)))
    (gk:bind-button :c :pressed
		    (lambda ()
		      (setf c t)))
    (gk:bind-button :c :released
		    (lambda ()
		      (setf c nil)))
    (gk:bind-button :w :pressed
		    (lambda ()
		      (setf w t)))
    (gk:bind-button :w :released
		    (lambda ()
		      (setf w nil)))
    (gk:bind-button :s :pressed
		    (lambda ()
		      (setf s t)))
    (gk:bind-button :s :released
		    (lambda ()
		      (setf s nil)))
    (gk:bind-button :d :pressed
		    (lambda ()
		      (setf d t)))
    (gk:bind-button :d :released
		    (lambda ()
		      (setf d nil)))
    (gk:bind-button :1 :pressed
		    (lambda ()
		      (setf key1 t)))
    (gk:bind-button :1 :released
		    (lambda ()
		      (setf key1 nil)))
    (gk:bind-button :2 :pressed
		    (lambda ()
		      (setf key2 t)))
    (gk:bind-button :2 :released
		    (lambda ()
		      (setf key2 nil)))
    (gk:bind-button :3 :pressed
		    (lambda ()
		      (setf key3 t)))
    (gk:bind-button :3 :released
		    (lambda ()
		      (setf key3 nil)))
    (gk:bind-button :4 :pressed
		    (lambda ()
		      (setf key4 t)))
    (gk:bind-button :4 :released
		    (lambda ()
		      (setf key4 nil)))
    (gk:bind-button :5 :pressed
		    (lambda ()
		      (setf key5 t)))
    (gk:bind-button :5 :released
		    (lambda ()
		      (setf key5 nil)))
    (gk:bind-button :6 :pressed
		    (lambda ()
		      (setf key6 t)))
    (gk:bind-button :6 :released
		    (lambda ()
		      (setf key6 nil)))
    (gk:bind-button :7 :pressed
		    (lambda ()
		      (setf key7 t)))
    (gk:bind-button :7 :released
		    (lambda ()
		      (setf key7 nil)))
    (gk:bind-button :8 :pressed
		    (lambda ()
		      (setf key8 t)))
    (gk:bind-button :8 :released
		    (lambda ()
		      (setf key8 nil)))
    (gk:bind-button :9 :pressed
		    (lambda ()
		      (setf key9 t)))
    (gk:bind-button :9 :released
		    (lambda ()
		      (setf key9 nil)))
    (gk:bind-button :0 :pressed
		    (lambda ()
		      (setf key0 t)))
    (gk:bind-button :0 :released
		    (lambda ()
		      (setf key0 nil)))
))
