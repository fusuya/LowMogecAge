(in-package :lowmogecage)
;;-------------A-star--------------------------------------------------------------------
(defstruct node
  (pos nil)
  (stable 0)
  (id 0)
  (g 0)
  (h 0)
  (f 0)
  (parent nil))




#|
CL-USER 10 > (minimum '((a 1) (b -1) (c -2)) #'< #'second)
(C -2)
|#
(defun minimum (list predicate key)
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

;;マンハッタン距離
(defun manhatan (pos goal)
  (+ (abs (- (car pos) (car goal))) (abs (- (cadr pos) (cadr goal)))))
;;ノードから道順をとり出す
(defun node-pick-pos (end pos-l)
  (if (null (node-parent end))
      pos-l
      (node-pick-pos (node-parent end) (cons (node-pos end) pos-l))))

(defun astar (start goal cells movecost block-cell route)
  (with-slots (tate yoko) *battle-field*
  (let ((open (list (make-node :pos start :g 0 :h (manhatan start goal)
			       :f (manhatan start goal))))
        (close nil))
    (loop for i from 0 do
	 (when (null open)
	   (return "hoge")) ;;ゴールまでの道がない
	 (let ((n (minimum open #'< #'node-f)))
	   (setf open (remove n open :test #'equalp))
	   (push n close)
	   (if (equal (node-pos n) goal)
	       (return (values (node-f n)
			       (node-pick-pos n '()))))
	   (setf (node-g n) (- (node-f n) (node-h n)))
	   (loop for v in route do
	        (let ((next (mapcar #'+ (node-pos n) v)))
	          (if (and (>= (1- yoko) (car next) 0)
			   (>= (1- tate) (cadr next) 0)
			   (not (find next block-cell :test #'equal)))
		      (let ((m (find next open :test #'equal :key #'node-pos))
			    (dist (aref movecost (aref cells (cadr next) (car next)))))
			(when (>= dist 0)
			  (if m
              		      (if (> (node-f m) (+ (node-g n) (node-h m) dist))
				  (setf (node-f m) (+ (node-g n) (node-h m) dist)
					(node-parent m) n))
              		      (progn
				(setf m (find next close :test #'equal :key #'node-pos))
				(if m
				    (if (> (node-f m) (+ (node-g n) (node-h m) dist))
					(progn
					  (setf (node-f m) (+ (node-g n) (node-h m) dist)
						(node-parent m) n)
					  (setf close (remove m close :test #'equalp))
					  (push m open)))
				    (progn
				      (setf m (make-node))
				      (setf (node-pos m) next)
				      (setf (node-g m) dist)
				      (setf (node-h m) (manhatan next goal))
				      (setf (node-f m) (+ (node-g n) (node-h m) (node-g m)))
				      (setf (node-parent m) n)
				      (push m open)))))))))))))))
;;--------------------------------------------------------------------------------
(defun world-astar (start goal cells movecost block-cell route tate yoko)
;;  (with-slots (tate yoko) *battle-field*
  (let ((open (list (make-node :pos start :g 0 :h (manhatan start goal)
			       :f (manhatan start goal))))
        (close nil))
    (loop for i from 0 do
	 (when (null open)
	   (return "hoge")) ;;ゴールまでの道がない
	 (let ((n (minimum open #'< #'node-f)))
	   (setf open (remove n open :test #'equalp))
	   (push n close)
	   (if (equal (node-pos n) goal)
	       (return (values (node-f n)
			       (node-pick-pos n '()))))
	   (setf (node-g n) (- (node-f n) (node-h n)))
	   (loop for v in route do
	        (let ((next (mapcar #'+ (node-pos n) v)))
	          (if (and (>= (1- yoko) (car next) 0)
			   (>= (1- tate) (cadr next) 0)
			   (not (find next block-cell :test #'equal)))
		      (let ((m (find next open :test #'equal :key #'node-pos))
			    (dist (aref movecost (aref cells (cadr next) (car next)))))
			(when (>= dist 0)
			  (if m
              		      (if (> (node-f m) (+ (node-g n) (node-h m) dist))
				  (setf (node-f m) (+ (node-g n) (node-h m) dist)
					(node-parent m) n))
              		      (progn
				(setf m (find next close :test #'equal :key #'node-pos))
				(if m
				    (if (> (node-f m) (+ (node-g n) (node-h m) dist))
					(progn
					  (setf (node-f m) (+ (node-g n) (node-h m) dist)
						(node-parent m) n)
					  (setf close (remove m close :test #'equalp))
					  (push m open)))
				    (progn
				      (setf m (make-node))
				      (setf (node-pos m) next)
				      (setf (node-g m) dist)
				      (setf (node-h m) (manhatan next goal))
				      (setf (node-f m) (+ (node-g n) (node-h m) (node-g m)))
				      (setf (node-parent m) n)
				      (push m open))))))))))))))
