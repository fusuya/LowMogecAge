;;ドンジョンのデータも
(in-package :mogetical)


(defun youbi (day)
  (case day
    (0 "月")
    (1 "火")
    (2 "水")
    (3 "木")
    (4 "金")
    (5 "土")
    (6 "日")))

;;chest用
(defun save-obj-data (objs)
  (loop :for p :in objs
     :collect (list (x p) (y p) (posx p) (posy p) (img p))))

(defun save-units-data (units)
  (loop :for p :in units
     :collect (list (name p) (job p) (hp p) (maxhp p) (agi p)
		    (str p) (vit p) (res p) (int p) (expe p)
		    (lvup-exp p) (img-h p) (w p) (h p) (moto-w p) (moto-h p)
		    (x p) (y p) (posx p) (posy p) (cell p)
		    (itemnum (buki p))
		    (itemnum (armor p)))))


(defun save-party-item-data ()
  (loop :for item :in (item *p*)
     :collect (list (itemnum item) (equiped item))))

(defun save-player-data ()
  (list (save-units-data (party *p*)) (save-party-item-data)))

(defun save-donjon-data ()
  (with-slots (donjonnum stage enemies drop-item chest kaidan
			 warrior-weapon sorcerer-weapon priest-weapon archer-weapon
			 thief-weapon knight-weapon) *donjon*
    (list donjonnum stage (save-units-data enemies) drop-item (save-obj-data chest) kaidan
	  warrior-weapon sorcerer-weapon priest-weapon archer-weapon
			      thief-weapon knight-weapon)))

;;セーブする
(defun save-suru (slot)
  (let* ((str (concatenate 'string "save" (write-to-string slot)))
	 (pathstr (namestring (merge-pathnames str *save-root*))))
    (multiple-value-bind (sec min hr day mon year dow daylight-p zone)
	(decode-universal-time (get-universal-time ))
      (declare (ignore daylight-p zone))
      (let ((saveday (format nil "~d階 ~4,'0d年~2,'0d月~2,'0d日 (~a)  ~2,'0d:~2,'0d:~2,'0d"
			     (stage *donjon*) year mon day (youbi dow)  hr min sec)))
	(case slot
	  (1 (setf *save1-day* saveday))
	  (2 (setf *save2-day* saveday))
	  (3 (setf *save3-day* saveday)))
      (with-open-file (out pathstr :direction :output
			   :if-exists :supersede)
	
	(format out "(in-package mogetical)~%")
	(format out "(setf *load-game-data~d* '~s)~%" slot (list saveday (save-player-data) (save-donjon-data))))))))

;;ダンジョンーデータをロードしてセット
(defun load-donjon-data (data)
  (destructuring-bind (donjonnum stage enemies drop-item chest kaidan
				 warrior-weapon sorcerer-weapon priest-weapon archer-weapon
				 thief-weapon knight-weapon) data
    (let* ((donjon (copy-tree (nth donjonnum *stage-list*))))
      (setf (field *donjon*) (make-array (list 15 15) :initial-contents (getf donjon :field))
	    (player-init-pos *donjon*) (getf donjon :player-init-pos)
	    (enemy-init-pos *donjon*)  (getf donjon :enemy-init-pos)
	    (kaidan-init-pos *donjon*) (getf donjon :kaidan-init-pos)
	    (chest-init-pos *donjon*)  (getf donjon :chest-init-pos)
	    (chest-max *donjon*)       (getf donjon :chest-max)
	    (enemies *donjon*)
	    (loop :for e :in enemies
	       :collect (destructuring-bind (name job hp maxhp agi str vit res int expe lvup-exp
						  img-h w h moto-w moto-h x y posx posy cell buki armor) e
			  (let ((b (item-make buki))
				(a (item-make armor)))
			    (make-instance 'unit :name name :job job
					   :hp hp :maxhp maxhp :agi agi :str str
					   :vit vit :res res :int int :expe expe
					   :lvup-exp lvup-exp :img-h img-h :w w
					   :h h :moto-w moto-w :moto-h moto-h
					   :x x :y y :posx posx :posy posy :cell cell
					   :buki b :armor a
					   :team :enemy))))
	    (drop-item *donjon*) drop-item
	    (aref (field *donjon*) (cadr kaidan) (car kaidan)) +kaidan+
	    (warrior-weapon *donjon*) warrior-weapon
	    (sorcerer-weapon *donjon*) sorcerer-weapon
	    (thief-weapon *donjon*) thief-weapon
	    (archer-weapon *donjon*) archer-weapon
	    (priest-weapon *donjon*) priest-weapon
	    (knight-weapon *donjon*) knight-weapon
	    (stage *donjon*) stage
	    (chest *donjon*)
	    (loop :for c :in chest
	       :collect (destructuring-bind (x y posx posy img) c
			  (make-instance 'obj :x x :y y :posx posx :posy posy :img img)))))))

;;ロードしてセットする
(defun load-player-data (data)
  (destructuring-bind (units items) data
    (setf (party *p*)
	  (loop :for u :in units
	     :collect (destructuring-bind (name job hp maxhp agi str vit res int expe lvup-exp
						img-h w h moto-w moto-h x y posx posy cell
						buki armor) u
			(declare (ignore buki armor))
			(make-instance 'unit :name name :job job
				       :hp hp :maxhp maxhp :agi agi :str str
				       :vit vit :res res :int int :expe expe
				       :lvup-exp lvup-exp :img-h img-h :w w
				       :h h :moto-w moto-w :moto-h moto-h
				       :x x :y y :posx posx :posy posy :cell cell
				       :team :ally)))
	  (item *p*)
	  (loop :for i :in items
	     :collect (let* ((item (item-make (car i)))
			     (name (cadr i))
			     (unit (find name (party *p*) :test #'equal :key #'name)))
			(setf (equiped item) name)
			(when unit
			  (cond
			    ((eq (categoly item) :armor)
			     (setf (armor unit) item))
			    (t (setf (buki unit) item))))
			item)))))

;;ロードデータをセット
(defun load-suru (slot)
  (let ((data (case slot
		(1 *load-game-data1*)
		(2 *load-game-data2*)
		(3 *load-game-data3*))))
    (destructuring-bind (day player-data donjon-data) data
      (declare (ignore day))
      (load-player-data player-data)
      (load-donjon-data donjon-data)
      (setf (state *p*) :battle-preparation))))


;;データロードしておく
(defun load-data ()
  (let* ((files (map 'list #'pathname-name  (directory (namestring (merge-pathnames "*" *save-root*))))))
    (loop :for name :in '("save1" "save2" "save3")
       :do (when (find name files :test #'equal)
	     (let ((filepath (namestring (merge-pathnames name *save-root*))))
	       (load filepath))))))
	     
