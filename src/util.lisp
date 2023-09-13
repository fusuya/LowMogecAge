(in-package :lowmogecage)


(defun random-minmax (mini maxa)
  (+ mini (1+ (random (max 1 (- maxa  mini))))))


(defun dice (n maxi)
  (let ((hoge 0))
    (dotimes (i n)
      (incf hoge (1+ (random maxi))))
    hoge))

(defun get-ability-bonus (n)
  (cond
    ((>= 5 n 0) 0)
    ((>= 11 n 6) 1)
    ((>= 17 n 12) 2)
    ((>= 23 n 18) 3)
    ((>= 29 n 24) 4)
    ((>= 35 n 30) 5)))

;;重み付け抽選-----------------------------------------------
(defun rnd-pick (i rnd lst len)
  (if (= i len)
      (1- i)
      (if (< rnd (nth i lst))
	  i
	  (rnd-pick (1+ i) (- rnd (nth i lst)) lst len))))
;;lst = *copy-buki*
(defun weightpick (lst)
  (let* ((lst1 (mapcar #'cdr lst))
	 (total-weight (apply #'+ lst1))
	 (len (length lst1))
	 (rnd (random total-weight)))
    (car (nth (rnd-pick 0 rnd lst1 len) lst))))
;;------------------------------------------------------------

(defun manhatan-x-y (x1 x2 y1 y2)
  (let ((diffx (- x1 x2))
	(diffy (- y1 y2)))
    (+ (abs diffx) (abs diffy))))
