(in-package :lowmogecage)


;;モンスター番号
(my-enum  +img-slime+ +img-orc+ +img-yote1+ +img-hydra+ +img-dragon+ +img-brigand+ +img-arrow-hood+ +img-dagger-hood+
	  +img-sabel-hood+ +img-shield-hood+ +img-skeleton+ +img-revenant+ +img-zombie+ +img-dry-corpse+ +img-ghost+ +img-phantom+
	  +img-gust+ +img-gust-knight+ +img-gargoyle+ +img-monster-max+)

;;レベル別モンスターリスト
(defparameter *monster-list-by-level
  '(1 (arrow-hood dagger-hood  skeleton revenant gust slime)
    2 (sabel-hood zombie ghost brigand)
    3 (shield-hood dry-corpse gargoyle orc)
    4 (gust-knight)
    5 (phantom )
    10 (hydra)
    11 (dragon)
    ))

(defclass arrow-hood (monster)
  ())
(defmethod initialize-instance :after ((e arrow-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move
	       vit-bonus res-bonus hp maxhp mp maxmp level rangemin rangemax drop) e
    (setf job-name "アローーフッド" level 1 rangemin 2 rangemax 5
	  move 5 hit-value 3 atk-point 1 avoid-value 0 def 1
	  res-bonus 3 vit-bonus 2 hp 10 maxhp 10 mp 10 maxmp 10
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-arrow-hood+))
	  id :arrow-hood
	  drop (lambda (n) (cond ((>= 7 n 2) (list +c_crude_weapon+))
				 ((>= n 8) (list +c_crude_weapon+ +c_dirty_hood+)))))))

(defclass dagger-hood (monster)
  ())
(defmethod initialize-instance :after ((e dagger-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ダガーフッド" level 1
	  move 5 hit-value 3 atk-point 2 avoid-value 1 def 1
	  res-bonus 3 vit-bonus 2 hp 12 maxhp 12 mp 10 maxmp 10
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-dagger-hood+))
	  id :dagger-hood
	  drop (lambda (n) (cond ((>= 7 n 2) (list +c_crude_weapon+))
				 ((>= n 8) (list +c_crude_weapon+ +c_dirty_hood+)))))))

(defclass sabel-hood (monster)
  ())
(defmethod initialize-instance :after ((e sabel-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "サーベルフッド" level 2
	  move 5 hit-value 4 atk-point 2 avoid-value 2 def 2
	  res-bonus 4 vit-bonus 3 hp 13 maxhp 13 mp 11 maxmp 11
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-sabel-hood+))
	  id :sabel-hood
	  drop (lambda (n) (cond ((>= 6 n 2) (list +c_crude_weapon+))
				 ((>= 11 n 7) (list +c_crude_weapon+ +c_dirty_hood+))
				 ((>= n 12) (list +c_crude_weapon+ +c_well_worn_hood+)))))))

(defclass shield-hood (monster)
  ())
(defmethod initialize-instance :after ((e shield-hood) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "シールドフッド" level 3
	  move 5 hit-value 5 atk-point 2 avoid-value 3 def 5
	  res-bonus 5 vit-bonus 4 hp 19 maxhp 19 mp 12 maxmp 12
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-shield-hood+))
	  id :shield-hood
	  drop (lambda (n) (cond ((>= 4 n 2) (list +c_crude_shield+))
				 ((>= 10 n 5) (list +c_crude_shield+ +c_dirty_hood+))
				 ((>= n 11) (list +c_crude_shield+ +c_well_worn_hood+)))))))

(defclass skeleton (monster)
  ())
(defmethod initialize-instance :after ((e skeleton) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "スケルトン" level 1
	  move 4 hit-value 2 atk-point 0 avoid-value 2 def 3
	  res-bonus 4 vit-bonus 2 hp 15 maxhp 15 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-skeleton+))
	  id :skeleton
	  drop (lambda (n) (cond ((>= 5 n 2) nil)
				 ((>= 10 n 6) (list +c_sturdy_bone+))
				 ((>= n 11) (list +c_magic_bone+)))))))

(defclass revenant (monster)
  ())
(defmethod initialize-instance :after ((e revenant) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "レブナント" level 1
	  move 4 hit-value 2 atk-point 2 avoid-value 2 def 1
	  res-bonus 3 vit-bonus 3 hp 20 maxhp 20 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-revenant+))
	  id :revenant
	  drop (lambda (n) (cond ((>= 5 n 2) nil)
				 ((>= 10 n 6) (list +c_sullied_bone+))
				 ((>= n 11) (list +c_unholy_skull+)))))))

(defclass zombie (monster)
  ())
(defmethod initialize-instance :after ((e zombie) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ゾンビ" level 2
	  move 5 hit-value 3 atk-point 3 avoid-value 3 def 1
	  res-bonus 4 vit-bonus 4 hp 25 maxhp 25 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-zombie+))
	  id :zombie
	  drop (lambda (n) (cond ((= n 2) nil)
				 ((>= 4 n 3) (list +c_zombie_eyeball+))
				 ((>= n 5) (list +c_zombie_eyeball+ +c_zombie_eyeball+)))))))

(defclass dry-corpse (monster)
  ())
(defmethod initialize-instance :after ((e dry-corpse) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ドライコープス" level 3
	  move 8 hit-value 5 atk-point 2 avoid-value 5 def 2
	  res-bonus 5 vit-bonus 5 hp 25 maxhp 25 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-dry-corpse+))
	  id :dry-corpse
	  drop (lambda (n) (cond ((>= 10 n 2) (list +c_sullied_bone+))
				 ((>= n 11) (list +c_unholy_skull+)))))))

(defclass ghost (monster)
  ())
(defmethod initialize-instance :after ((e ghost) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ゴースト" level 2
	  move 5 hit-value 3 atk-point 0 avoid-value 3 def 0
	  res-bonus 4 vit-bonus 5 hp 20 maxhp 20 mp 0 maxmp 0
	  movecost #(1 -1 -1 1 1 1 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-ghost+))
	  id :ghost
	  drop (lambda (n) (cond ((>= 11 n 2) nil)
				 ((>= n 12) (list +c_fruits_resentment_tears+)))))))

(defclass phantom (monster)
  ())
(defmethod initialize-instance :after ((e phantom) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ファントム" level 5
	  move 6 hit-value 6 atk-point 4 avoid-value 7 def 0
	  res-bonus 8 vit-bonus 6 hp 42 maxhp 42 mp 16 maxmp 16
	  movecost #(1 -1 -1 1 1 1 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-phantom+))
	  id :phantom
	  drop (lambda (n) (cond ((>= 6 n 2) nil)
				 ((>= 10 n 7) (list +c_old_cloak+))
				 ((>= n 11) (list +c_year_old_cape+)))))))

(defclass gust (monster)
  ())
(defmethod initialize-instance :after ((e gust) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level ) e
    (setf job-name "ガスト" level 1
	  move 7 hit-value 3 atk-point 2 avoid-value 2 def 0
	  res-bonus 2 vit-bonus 2 hp 14 maxhp 14 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 2 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-gust+))
	  id :gust
	  drop (lambda (n) (cond ((>= 9 n 2) nil)
				 ((>= n 10) (list +c_magic_stone+)))))))

(defclass gust-knight (monster)
  ())
(defmethod initialize-instance :after ((e gust-knight) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ガストナイト" level 4
	  move 4 hit-value 6 atk-point 8 avoid-value 5 def 2
	  res-bonus 4 vit-bonus 7 hp 32 maxhp 32 mp 0 maxmp 0
	  movecost #(1 -1 -1 2 2 2 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-gust-knight+))
	  id :gust-knight
	  drop (lambda (n) (cond ((>= 6 n 2) nil)
				 ((>= 11 n 7) (list +c_magic_stone+))
				 ((>= n 12) (list +c_high_density_magic_stone+)))))))

(defclass gargoyle (monster)
  ())
(defmethod initialize-instance :after ((e gargoyle) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp level) e
    (setf job-name "ガーゴイル" level 3
	  move 10 hit-value 5 atk-point 4 avoid-value 4 def 3
	  res-bonus 4 vit-bonus 4 hp 26 maxhp 26 mp 0 maxmp 0
	  movecost #(1 -1 -1 1 1 1 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-gargoyle+))
	  id :gargoyle
	  drop (lambda (n) (cond ((>= 7 n 2) (list +c_sharp_stone+))
				 ((>= n 8) (list +c_magic_stone+)))))))

(defclass orc (monster)
  ())

(defmethod initialize-instance :after ((e orc) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type level) e
    (setf job-name "オーク" level 3
	  move 4 hit-value 4 atk-point 3 avoid-value 4 def 2
	  res-bonus 5 vit-bonus 4 hp 28 maxhp 28 mp 15 maxmp 15
	  rangemin 1 rangemax 1 atking-type :short
	  movecost #(1 -1 -1 2 2 3 -1 1 1)
	  origin (gk:vec2 0 (* 32 +img-orc+))
	  id :orc
	  drop (lambda (n) (cond ((>= 3 n 2) nil)
				 ((>= 8 n 4) (list +c_copper_bag+))
				 ((>= 10 n 9) (list +c_silver_bag+))
				 ((>= n 11) (list +c_gems+)))))))


(defclass hydra (monster)
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (deg          :accessor deg        :initform 10  :initarg :deg)))

(defmethod initialize-instance :after ((e hydra) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type level ) e
    (setf job-name "ヒドラ" level 10
	  move 3 hit-value 14 atk-point 15 avoid-value 13 def 13
	  res-bonus 13 vit-bonus 14 hp 106 maxhp 106 mp 44 maxmp 44
	  rangemin 1 rangemax 1 atking-type :short
	  movecost #(1 -1 -1 2 3 3 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-hydra+))
	  id :hydara
	  drop (lambda (n) (cond ((>= 3 n 2) nil)
				 ((>= 8 n 4) (list +c_copper_bag+))
				 ((>= 10 n 9) (list +c_silver_bag+))
				 ((>= n 11) (list +c_gems+)))))))

(defclass brigand (monster)
  ())
(defmethod initialize-instance :after ((e brigand) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type level) e
    (setf job-name "ブリガンド" level 2
	  hit-value 4 atk-point 2 avoid-value 3 def 3
	  res-bonus 3 vit-bonus 4 hp 20 maxhp 20 mp 10 maxmp 10
	  move 3 rangemin 2 rangemax 5 atking-type :long
	  movecost  #(1 -1 -1 2 2 3 -1 2 1)
	  origin (gk:vec2 0 (* 32 +img-brigand+))
	  id :brigand
	  drop (lambda (n) (cond ((>= 6 n 2) nil)
				 ((>= 10 n 7) (list +c_copper_bag+))
				 ((>= n 11) (list +c_silver_bag+)))))))

(Defclass slime (monster)
  ())
(defmethod initialize-instance :after ((e slime) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type level) e
    (setf job-name "スライム" level 1
	  move 3 hit-value 3 atk-point 1 avoid-value 4 def 3
	  res-bonus 3 vit-bonus 3 hp 13 maxhp 13 mp 0 maxmp 0
	  rangemin 1 rangemax 1 atking-type :short
	  movecost  #(1 -1 -1 2 2 2 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-slime+))
	  id :slime
	  drop (lambda (n) (cond ((>= 6 n 2) nil)
				 ((>= 10 n 7) (list +c_copper_bag+))
				 ((>= n 11) (list +c_silver_bag+)))))))


(defclass dragon (monster)
  ())
(defmethod initialize-instance :after ((e dragon) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost def hit-value avoid-value atk-point id origin move drop
	       vit-bonus res-bonus hp maxhp mp maxmp rangemin rangemax atking-type level ) e
    (setf job-name "ドラゴン" level 11
	  move 4 hit-value 17 atk-point 18 avoid-value 15 def 14
	  res-bonus 17 vit-bonus 17 hp 133 maxhp 133 mp 84 maxmp 84
	  rangemin 1 rangemax 1 atking-type :short
	  movecost  #(1 -1 -1 2 2 2 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-dragon+))
	  id :dragon
	  drop (lambda (n) (cond ((>= 6 n 2) (list +c_fragment_sword+))
				 ((>= 10 n 7) (list +c_fragment_sword+ +c_fragment_sword+))
				 ((>= n 11) (list +c_fragment_sword+ +c_fragment_sword+ +c_fragment_sword+)))))))
