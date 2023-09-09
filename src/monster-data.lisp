(in-package :lowmogecage)


;;モンスター番号
(my-enum  +img-slime+ +img-orc+ +img-yote1+ +img-hydra+ +img-dragon+ +img-brigand+ +img-arrow-hood+ +img-dagger-hood+
	  +img-sabel-hood+ +img-shield-hood+ +img-skeleton+ +img-revenant+ +img-zombie+ +img-dry-corpse+ +img-ghost+ +img-phantom+
	  +img-gust+ +img-gust-knight+ )


(defclass arrow-hood (monster)
  ())
(defmethod initialize-instance :after ((e arrow-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "アローーフッド"
	  move 5 hit-value 3 atk-point 1 avoid-value 0 def 1
	  res-bonus 3 vit-bonus 2 hp 10 maxhp 10 mp 10 maxmp 10
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-arrow-hood+))
	  id :arrow-hood)))

(defclass dagger-hood (monster)
  ())
(defmethod initialize-instance :after ((e dagger-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "ダガーフッド"
	  move 5 hit-value 3 atk-point 2 avoid-value 1 def 1
	  res-bonus 3 vit-bonus 2 hp 12 maxhp 12 mp 10 maxmp 10
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-dagger-hood+))
	  id :dagger-hood)))

(defclass sabel-hood (monster)
  ())
(defmethod initialize-instance :after ((e sabel-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "サーベルフッド"
	  move 5 hit-value 4 atk-point 2 avoid-value 2 def 2
	  res-bonus 4 vit-bonus 3 hp 13 maxhp 13 mp 11 maxmp 11
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-sabel-hood+))
	  id :sabel-hood)))

(defclass shield-hood (monster)
  ())
(defmethod initialize-instance :after ((e shield-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "シールドフッド"
	  move 5 hit-value 5 atk-point 2 avoid-value 3 def 5
	  res-bonus 5 vit-bonus 4 hp 19 maxhp 19 mp 12 maxmp 12
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-shield-hood+))
	  id :shield-hood)))

(defclass skeleton (monster)
  ())
(defmethod initialize-instance :after ((e skeleton) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "スケルトン"
	  move 4 hit-value 2 atk-point 0 avoid-value 2 def 3
	  res-bonus 4 vit-bonus 2 hp 15 maxhp 15 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-skeleton+))
	  id :skeleton)))

(defclass revenant (monster)
  ())
(defmethod initialize-instance :after ((e revenant) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "レブナント"
	  move 4 hit-value 2 atk-point 2 avoid-value 2 def 1
	  res-bonus 3 vit-bonus 3 hp 20 maxhp 20 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-revenant+))
	  id :revenant)))

(defclass zombie (monster)
  ())
(defmethod initialize-instance :after ((e zombie) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp) e
    (setf job-name "ゾンビ"
	  move 5 hit-value 3 atk-point 3 avoid-value 3 def 1
	  res-bonus 4 vit-bonus 4 hp 25 maxhp 25 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-zombie+))
	  id :zombie)))

(defclass orc (monster)
  ())

(defmethod initialize-instance :after ((e orc) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type) e
    (setf job-name "オーク"
	  move 4 hit-value 4 atk-point 3 avoid-value 4 def 2
	  res-bonus 5 vit-bonus 4 hp 28 maxhp 28 mp 15 maxmp 15
	  rangemin 1 rangemax 1 atking-type :short
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-orc+))
	  id :orc)))


(defclass hydra (monster)
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (deg          :accessor deg        :initform 10  :initarg :deg)))

(defmethod initialize-instance :after ((e hydra) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type) e
    (setf job-name "ヒドラ"
	  move 3 hit-value 14 atk-point 15 avoid-value 13 def 13
	  res-bonus 13 vit-bonus 14 hp 106 maxhp 106 mp 44 maxmp 44
	  rangemin 1 rangemax 1 atking-type :short
	  movecost #(1 -1 -1 2 3 3 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-hydra+))
	  id :hydara )))

(defclass brigand (monster)
  ())
(defmethod initialize-instance :after ((e brigand) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type) e
    (setf job-name "ブリガンド"
	  hit-value 4 atk-point 2 avoid-value 3 def 3
	  res-bonus 3 vit-bonus 4 hp 20 maxhp 20 mp 10 maxmp 10
	  move 3 rangemin 2 rangemax 5 atking-type :long
	  movecost  #(1 -1 -1 2 2 3 -1 2 1)
	  origin (gk:vec2 0 (* 32 +img-brigand+))
	  id :brigand )))

(Defclass slime (monster)
  ())
(defmethod initialize-instance :after ((e slime) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type) e
    (setf job-name "スライム"
	  move 3 hit-value 3 atk-point 1 avoid-value 4 def 3
	  res-bonus 3 vit-bonus 3 hp 13 maxhp 13 mp 0 maxmp 0
	  rangemin 1 rangemax 1 atking-type :short
	  movecost  #(1 -1 -1 2 2 2 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-slime+))
	  id :slime)))


(defclass dragon (monster)
  ())
(defmethod initialize-instance :after ((e dragon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type) e
    (setf job-name "ドラゴン"
	  move 4 hit-value 17 atk-point 18 avoid-value 15 def 14
	  res-bonus 17 vit-bonus 17 hp 133 maxhp 13 mp 84 maxmp 84
	  rangemin 1 rangemax 1 atking-type :short
	  movecost  #(1 -1 -1 2 2 2 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-dragon+))
	  id :dragon)))
