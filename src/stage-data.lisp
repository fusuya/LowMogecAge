(in-package :lowmogecage)

(defparameter *test* `((,+forest+ . 10) (,+mtlow+ . 0) (,+mthigh+ . 0) (,+water+ . 0) (,+fort+ . 0) (,+plain+ . 90)))

;;TODO
(defparameter *cell-rate*
  `(:forest-field (:rate ((,+forest+ . 20) (,+mtlow+ . 1) (,+mthigh+ . 0) (,+water+ . 1) (,+fort+ . 0) (,+plain+ . 90))
		   :forest-add-rate 35 :mtlow-add-rate 0 :mthigh-add-rate 0 :water-add-rate 0)
    :mounten-field (:rate ((,+forest+ . 0) (,+mtlow+ . 4) (,+mthigh+ . 1) (,+water+ . 0) (,+fort+ . 0) (,+plain+ . 90))
		    :forest-add-rate 0 :mtlow-add-rate 40 :mthigh-add-rate 30 :water-add-rate 0)
    :mt-forest-field (:rate ((,+forest+ . 5) (,+mtlow+ . 3) (,+mthigh+ . 1) (,+water+ . 0) (,+fort+ . 0) (,+plain+ . 91))
		      :forest-add-rate 30 :mtlow-add-rate 30 :mthigh-add-rate 20 :water-add-rate 0)
    :plain-field (:rate ((,+forest+ . 1) (,+mtlow+ . 1) (,+mthigh+ . 0) (,+water+ . 2) (,+fort+ . 0) (,+plain+ . 100))
		  :forest-add-rate 10 :mtlow-add-rate 5 :mthigh-add-rate 0 :water-add-rate 5)
    :river-field (:rate ((,+forest+ . 5) (,+mtlow+ . 3) (,+mthigh+ . 0) (,+water+ . 15) (,+fort+ . 0) (,+plain+ . 90))
		   :forest-add-rate 10 :mtlow-add-rate 10 :mthigh-add-rate 0 :water-add-rate 40)))

(defparameter *appear-enemy-rate-list*
  '((:slime . 100) (:orc . 90) (:brigand . 80) (:hydra . 70) (:goron . 60)
    (:warrior . 50) (:sorcerer . 40) (:priest . 40) (:archer . 50) (:thief . 40)
    (:knight . 30) (:pknight . 30) (:dragon . 10)))

(defparameter *appear-enemy-rate-monster*
  '((:slime . 100) (:orc . 90) (:brigand . 80) (:hydra . 70) (:dragon . 10)))


;;アイテムリストの確率部分を20以上のモノを1下げる
(defun rate-decf (rate-lst)
  (loop :for i :in rate-lst
     :collect (if (> (cdr i) 20)
		  (cons (car i) (decf (cdr i)))
		  i)))

;;絵画進むごとに敵が強い武器装備する確率を上げる
(defun adjust-enemy-equip-rate ()
  (with-slots (warrior-weapon sorcerer-weapon priest-weapon archer-weapon
			      thief-weapon knight-weapon) *donjon*
  (setf warrior-weapon  (rate-decf warrior-weapon)
	sorcerer-weapon (rate-decf sorcerer-weapon)
	priest-weapon   (rate-decf priest-weapon)
	thief-weapon    (rate-decf thief-weapon)
	archer-weapon   (rate-decf archer-weapon)
	knight-weapon   (rate-decf knight-weapon))))


(defun adjust-appear-enemy ()
  (setf (game/enemy-rate *game*)
	(rate-decf (game/enemy-rate *game*))))

;;出現する敵 階層によって出現率を変える
(defun appear-enemy ()
  (weightpick (game/enemy-rate *game*)))

;;階層+-２
(defun set-enemy-level ()
  (max 1
       (+ (stage *battle-field*) (if (= (random 2) 0)
				     (random 3) (- (random 3))))))

;;敵生成時に装備してる武器 アイテムのドロップ率と同じ
(defun enemy-equip-weapon (e-type)
  (with-slots (warrior-weapon sorcerer-weapon priest-weapon archer-weapon
			      thief-weapon knight-weapon) *game*
    (case e-type
      ((slime orc brigand hydra dragon yote1 goron)
       (job-init-weapon e-type))
      (warrior
       (item-make  (weightpick warrior-weapon)))
      (sorcerer
       (item-make  (weightpick sorcerer-weapon)))
      (priest
       (item-make  (weightpick priest-weapon)))
      (thief
       (item-make  (weightpick thief-weapon)))
      (archer
       (item-make  (weightpick archer-weapon)))
      ((knight pknight)
       (item-make  (weightpick knight-weapon))))))

;;敵生成時に装備する防具
(defun enemy-equip-armor ()
  (item-make (weightpick *armor-list*)))


;;レヴェルアップ時ステータス上昇
(defun status-up (atker)
  (let ((lvup-rate (get-job-data (job atker) :lvuprate)))
    (when (>= (getf lvup-rate :hp) (random 100))
      (incf (maxhp atker))
      (setf (hp atker) (maxhp atker)))
    (when (>= (getf lvup-rate :str) (random 100))
      (incf (str atker)))
    (when (>= (getf lvup-rate :vit) (random 100))
      (incf (vit atker)))
    (when (>= (getf lvup-rate :int) (random 100))
      (incf (int atker)))
    (when (>= (getf lvup-rate :res) (random 100))
      (incf (res atker)))
    (when (>= (getf lvup-rate :agi) (random 100))
      (incf (agi atker)))))


(defun create-enemy-data (e-class cell hp str def int res agi expe job)
  (let* ((level (set-enemy-level))
	 (enemy-w *origin-obj-w*)
	 (enemy-h *origin-obj-h*)
	 (e (make-instance e-class :x (x cell) :y (y cell)
				   ;; :posx (posx cell) :posy (posy cell)
				   ;;:posx2 (posx2 cell)   :posy2 (posy2 cell)
				   :pos (math:copy-vec2  (pos cell))
				   :name (nth (random (length *name-list*)) *name-list*)
				   :level level
				   :weapon (enemy-equip-weapon e-class)
			 ;; :atk-spd 10 :buki (enemy-equip-weapon e-type job)
			 ;; :armor (enemy-equip-armor)
			  ;;:moto-w *obj-w* :moto-h *obj-h*
			  ;;:str str :vit def :hp hp :maxhp hp :int int :res res
			  ;;:expe (+ expe level) :agi agi :state :wait
				   :w enemy-w :h enemy-h :sight (+ 3 (random 3))
			  ;;:w/2 (floor *obj-w* 2) :h/2 (floor *obj-h* 2)
			  ;;:obj-type e-type :img-h job
			  :team :enemy :job job
				  ;; :img 1
				   )))
    ;;(dotimes (i (level e))
    ;;  (status-up e))
    e))

;;プレイヤーのいる階層で敵の強さが変わる hp str def int res agi expe job
(defun create-enemy (e-pos e-type)
  (case e-type
    (:slime    (create-enemy-data 'slime e-pos 6  1  1 1  1  1   3  +job_slime+))
    (:orc      (create-enemy-data 'orc e-pos 10 4  1 1  1  2   5  +job_orc+))
    (:brigand  (create-enemy-data 'brigand e-pos 6  2  2 7  4  3   7  +job_brigand+ ))
    (:hydra    (create-enemy-data 'hydra e-pos 12 2  5 2  3  3  10  +job_hydra+ ))
    (:dragon   (create-enemy-data 'dragon e-pos 20 5  6 5  6  5  20  +job_dragon+ ))
    (:yote1    (create-enemy-data e-type e-pos 3  3 50 3 50 33 300  +job_yote1+ ))
    (:goron    (create-enemy-data e-type e-pos 5  2  3 3  3  3   4  +job_goron+ ))
    (:warrior  (create-enemy-data 'warrior e-pos 10 4  5 1  2  2   8  +job_warrior+))
    (:sorcerer (create-enemy-data e-type e-pos 5  1  2 7  5  1   8  +job_sorcerer+))
    (:priest   (create-enemy-data e-type e-pos 5  1  4 4  8  3   8  +job_priest+))
    (:thief    (create-enemy-data e-type e-pos 6  3  3 3  3  9   8  +job_thief+ ))
    (:archer   (create-enemy-data e-type e-pos 5  3  3 3  3  3   8  +job_archer+ ))
    (:knight   (create-enemy-data e-type e-pos 12 6  4 3  5  3   8  +job_s_knight+ ))
    (:pknight  (create-enemy-data e-type e-pos 10 7  2 3  2  2   8  +job_p_knight+ ))
    ))

(defun get-battle-init-pos-dir ()
  (case (random 4)
    (0 (values :up :down))
    (1 (values :down :up))
    (2 (values :right :left))
    (3 (values :left :right))))

;;出撃ポジション posのlistゲット
(Defun get-battle-init-pos (dir xrange yrange)
  (with-slots (tate yoko) *battle-field*
    (let ((xmin 0)
	  (xmax 0)
	  (ymin 0)
	  (ymax 0))
      (case dir
	(:down (setf ymin 0
		     ymax (1- yrange)
		     xmin (random (- yoko xrange))
		     xmax (+ xmin (1- xrange))))
	(:up (setf ymin (- tate yrange)
		   ymax (1- tate)
		   xmin (random (- yoko xrange))
		   xmax (+ xmin (1- xrange))))
	(:left (setf xmin 0
		     xmax (1- xrange)
		     ymin (random (- tate yrange))
		     ymax (+ ymin (1- yrange))))
	(:right (setf xmin (- yoko xrange)
		      xmax (1- yoko)
		      ymin (random (- tate yrange))
		      ymax (+ ymin (1- yrange)))))
      (loop :for x :from xmin :to xmax
	    :append  (loop :for y :from  ymin :to ymax
			   :collect (list x y))))))
      ;;(list :xmin xmin :ymin ymin :xmax xmax :ymax ymax))))

;;プレイヤーの初期配置位置
(defun set-battle-init-pos ()
  (with-slots (player-init-pos enemy-init-pos tate yoko) *battle-field*
      (multiple-value-bind (player-dir enemy-dir) (get-battle-init-pos-dir)
	(setf player-init-pos (get-battle-init-pos player-dir 4 4)
	      enemy-init-pos (get-battle-init-pos enemy-dir (floor yoko 2) (floor tate 2))))))


;;範囲内ランダム初期位置
(defun get-enemy-init-pos ()
  (with-slots (enemy-init-pos) *battle-field*
    (let ((cell (nth (random (length enemy-init-pos)) enemy-init-pos)))
      (setf enemy-init-pos (remove cell enemy-init-pos :test #'equal))
      cell)))

;;敵を配置する
(defun set-enemies ()
  (with-slots (enemy-init-pos enemies field stage) *battle-field*
    (let ((enemy-num (+ 3 (random (+ 3 (floor stage 5)))))) ;;1フロアに出る敵の数
      (loop
	 :repeat enemy-num
	 :do
	   (let* ((e-type (appear-enemy))
		  (cell (get-enemy-init-pos))
		  (e (create-enemy cell e-type)))
	     (push e enemies)))
      (setf enemy-init-pos nil) ;;終わったらnilにしとく
      )))

;;階段セット
(defun set-kaidan (donjon)
  (with-slots (field kaidan-init-pos kaidan) donjon
    (let* ((xmin (getf kaidan-init-pos :xmin)) (xmax (getf kaidan-init-pos :xmax))
	   (ymin (getf kaidan-init-pos :ymin)) (ymax (getf kaidan-init-pos :ymax)))
      (destructuring-bind (x y)
	  (get-init-pos nil xmin xmax ymin ymax)
	(setf (aref field y x) +kaidan+
	      kaidan (list x y))))))

;;宝箱セット
(defun set-chest (donjon)
  (with-slots (field chest-init-pos chest-max chest) donjon
    (let* ((xmin (getf chest-init-pos :xmin)) (xmax (getf chest-init-pos :xmax))
	   (ymin (getf chest-init-pos :ymin)) (ymax (getf chest-init-pos :ymax))
	   (chest-pos nil))
      (dotimes (i (+ (random chest-max) 2))
	(destructuring-bind (x y)
	    (get-init-pos chest-pos xmin xmax ymin ymax)
	  (push (make-instance 'obj :x x :y y :posx (* x *obj-w*) :posy (* y *obj-h*)
			       :img +chest+)
		chest)
	  (push (list x y) chest-pos))))))


(defun check-arround-cell (y x)
  (with-slots (field) *battle-field*
    (let ((left-cell +plain+)
	  (down-cell +plain+))
      (when (> x 0)
	(let ((cell (find-if #'(lambda (cell) (and (= (x cell) (1- x)) (= (y cell) y))) field)))
	  (setf left-cell (cell-num cell))))
      (when (> y 0)
	(let ((cell (find-if #'(lambda (cell) (and (= (x cell)  x) (= (y cell) (1- y)))) field)))
	  (setf down-cell (cell-num cell))))
      (values left-cell down-cell))))

;;TODO
(defun get-new-field-type (y x field-type)
  (let ((new-field-rate (copy-tree (getf field-type :rate)))
	(forest-add-rate (getf field-type :forest-add-rate ))
	(mtlow-add-rate (getf field-type :mtlow-add-rate ))
	(mthigh-add-rate (getf field-type :mthigh-add-rate ))
	(water-add-rate (getf field-type :water-add-rate)))
    (multiple-value-bind (left-cell down-cell) (check-arround-cell y x)
      (cond
	((= left-cell +forest+)
	 (incf (cdr (find left-cell new-field-rate :key #'car)) forest-add-rate))
	((= left-cell +mtlow+)
	 (incf (cdr (find left-cell new-field-rate :key #'car)) mtlow-add-rate))
	((= left-cell +mthigh+)
	 (incf (cdr (find left-cell new-field-rate :key #'car)) mthigh-add-rate))
	((= left-cell +water+)
	 (incf (cdr (find left-cell new-field-rate :key #'car)) water-add-rate))
	)
      (cond
	((= down-cell +forest+)
	 (incf (cdr (find down-cell new-field-rate :key #'car)) forest-add-rate))
	((= down-cell +mtlow+)
	 (incf (cdr (find down-cell new-field-rate :key #'car)) mtlow-add-rate))
	((= down-cell +mthigh+)
	 (incf (cdr (find down-cell new-field-rate :key #'car)) mthigh-add-rate))
	((= down-cell +water+)
	 (incf (cdr (find left-cell new-field-rate :key #'car)) water-add-rate)))
      new-field-rate)))
	

;;地形のデータ return '(name heal def avoid) 
(defun get-cell-data (cell)
  (cond
    ((= cell +plain+) (values "平原" 0 0 0))
    ((= cell +mtlow+) (values "山" 0 10 10 ))
    ((= cell +mthigh+) (values "高山" 0 10 10))
    ((= cell +fort+) (values "砦" 20 10 10))
    ((= cell +water+) (values "川" 0 10 0))
    ((= cell +forest+) (values "森" 0 5 5))))

;;ワールド上の地形ゲット ;;TODO pos
(defun get-world-cell (pos)
  (let* ((arr-x (floor (gk:x pos) 32))
	 (arr-y (floor (gk:y pos) 32)))
    (aref *world-map-data* arr-y arr-x)))

;;ワールドマップでモンスターシンボルとぶつかった場所のマップタイプ
(defun get-field-type ()
  (with-slots (world-pos) *game*
    (let ((world-cell (get-world-cell world-pos)))
      (cond
	((= world-cell 1) :plain-field)
	((= world-cell 2) (if (= (random 2) 0) :forest-field :mt-forest-field))
	((= world-cell 3) :river-field)
	((= world-cell 4) (if (= (random 2) 0) :mounten-field :mt-forest-field))))))

;;地形セット TODO
(Defun set-field-cell ()
  (with-slots (field tate yoko player-init-pos enemy-init-pos field-array) *battle-field*
    (multiple-value-bind (player-dir enemy-dir) (get-battle-init-pos-dir)
	(let* ((player-init-xy-list (get-battle-init-pos player-dir 4 4))
	      (enemy-init-xy-list (get-battle-init-pos enemy-dir (floor yoko 2) (floor tate 2)))
	      (field-type (get-field-type))
	       (field-type-data (getf *cell-rate* field-type)))
	  (print field-type)
	  (loop :for y :from 0 :below tate
		:do (loop :for x :from 0 :below yoko
			  :do
			     (let* ((new-field-type (get-new-field-type y x field-type-data))
				    (cell (weightpick new-field-type))
				    (cell-w (* *origin-obj-w* 1))
				    (cell-h (* *origin-obj-h* 1))
				    (origin (gk:vec2 (* cell *origin-obj-w*) 0))
				    (posx (* x *origin-obj-w*))
				    (posy (* y *origin-obj-h*)))
			       (multiple-value-bind (name heal def avoid) (Get-cell-data cell)
				 (let ((cell-data (make-instance 'cell :name name :heal heal :def def :avoid avoid
								       :cell-num cell
								       :w cell-w :h cell-h :img-id :obj-img
								       :x x :y y :origin origin
								       ;; :posx posx :posy posy 
								       ;; :posx2 (+ posx cell-w) :posy2 (+ posy cell-h)
								       :pos (gk:vec2 posx posy))))
				   (push cell-data field)
				   (setf (aref field-array y x) cell)
				   (cond
				     ((find (list x y) player-init-xy-list :test #'equal)
				      (push cell-data player-init-pos))
				     ((find (list x y) enemy-init-xy-list :test #'equal)
				      (push cell-data enemy-init-pos))))))))))))


;;ドロップアイテムセット
(defun adjust-drop-item-rate (donjon)
  (with-slots (stage drop-item) donjon
    ;;(when (= (floor stage 2) 0)
      (setf drop-item
	    (rate-decf drop-item))))

(defun create-battle-field (&optional (num nil))
  (with-slots (tate yoko field field-type battle-field-border-x battle-field-border-y field-array) *battle-field*
    (let* ((cell-w 32)
	   (cell-h 32))
      (setf tate 17 ;;(random-minmax 15 15)
	    yoko 27 ;;(random-minmax 15 20)
	    battle-field-border-x (+ (* yoko cell-w)  2)
	    battle-field-border-y (+ (* tate cell-h)  2)
	    field-array (make-array (list tate yoko)))
	    ;;field-type (getf *cell-rate* :mt-forest-field)) ;; TODO
      (set-field-cell)
      ;;(set-battle-init-pos)
      (set-enemies))))
	  ;; (player-init-pos *donjon*) (getf stage :player-init-pos)
	  ;; (enemy-init-pos *donjon*)  (getf stage :enemy-init-pos)
	  ;; (kaidan-init-pos *donjon*) (getf stage :kaidan-init-pos)
	  ;; (chest-init-pos *donjon*)  (getf stage :chest-init-pos)
	  ;; (chest-max *donjon*)       (getf stage :chest-max)
	  ;; (donjonnum *donjon*)       num
	  ;; (chest *donjon*)           nil
	  ;; (enemies *donjon*)         nil)
    ;; (set-kaidan *donjon*)
    ;; (set-chest *donjon*)
    ;; (set-enemies *donjon*)))
