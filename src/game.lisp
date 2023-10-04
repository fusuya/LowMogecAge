(in-package :lowmogecage)
;;TODO クエスト 
;; スキル増やす スキルのターゲット
(gk:defgame lowmogecage () ()
  (:viewport-width *window-w*)
  (:viewport-height *window-h*))


(gk::define-event-handler hoge ((ev ge.host:viewport-size-change-event) width height)
  (setf *scale-w* (* 1 (/ width *window-w*))
        *scale-h* (* 1 (/ height *window-h*))
	*scale-window-w*  width
	*scale-window-h* height
	*scale-window-center* (/ width 2)))



(defun init-mouse ()
  (with-slots (left right) *mouse*
    (setf left nil right nil)))

(defun init-key ()
  (with-slots (space1 a key1 key2 key3 key4 key5 key0 w s d c e r f g) *keystate*
    (setf space1 nil c nil e nil r nil f nil g nil
	  ;;a nil
	  ;;key1 nil key2 nil key3 nil key4 nil key5 nil key0 nil
	  ;;w nil  d nil s nil
	  )))



;;キャラクターを追加 debug
(defun push-chara-init-party (num unit &optional skill1 use-item)
  (with-slots (party item) *game*
    (when (> 5 (length party))
      (let* ((weapon (job-init-weapon unit))
	     (armor (item-make  +a_cloth_armor+))
	     (chara (make-instance unit :job num :w *battle-obj-w* :h *battle-obj-h*
					:lvup-exp 100 ;;test
					:state :inaction
					:name (nth (random (length *name-list*)) *name-list*))))
	(setf (equiped weapon) (name chara)
	      (equiped armor)  (name chara)
	      (weapon chara) weapon
	      (armor chara) armor
	      party (append party (list chara)))
	(when skill1
	  (setf (skill chara) skill1))
	(when use-item
	  (loop :for x :in use-item
		:do (setf (equiped x) (name chara))
		    (push x (use-item chara))
		    (push x item)))
	(push weapon item)
	(push armor item)))))
;; debug
(defun test-create-party-chara (string)
  (cond
    ((string= string "戦士")
     (push-chara-init-party +job_warrior+ 'p-fighter (list :first-aid))) ;;(get-use-item-data (list :healing-potion)))
    ((string= string "魔術師")
     (push-chara-init-party +job_sorcerer+ 'p-sorcerer (list :first-aid :fire)))
    ((string= string "神官")
     (push-chara-init-party +job_priest+ 'p-priest (list :first-aid :heal)))
    ((string= string "騎士")
     (push-chara-init-party +job_s_knight+ 'p-s-knight (list :first-aid)))
    ((string= string "射手")
     (push-chara-init-party +job_archer+ 'p-ranger (list :first-aid)))
    ((string= string "天馬騎士")
     (push-chara-init-party +job_p_knight+ 'p-p-knight (list :first-aid)))
    ((string= string "盗賊")
     (push-chara-init-party +job_thief+ 'p-scout (list :first-aid)))))


;;初期パーティデータ作成
(defun create-init-party-data ()
  (with-slots (temp-init-party party) *game*
    (loop :for temp :in temp-init-party
	  :do (with-slots (string) temp
		(test-create-party-chara string)))))


;;開始前の初期化
(defmethod gk:post-initialize ((app lowmogecage))
  (set-font)
  (bind-key-event)
  (setf *game* (make-instance 'game :state :title
			      :money 3000
			      :item (create-item-n +w_max+)
			      :world-pos (gk:vec2 358 520)
				    :scroll (gk:vec2 0 0)
				    :enemy-rate (copy-tree *appear-enemy-rate-monster*) ;; todo
                      		    :btn-list (list (make-instance 'game-start-btn :pos (gk:vec2 550 200)
                                                				   :string "はじめる" :color (gk:vec4 1 0.5 0.5 1)
										   :w 190 :h 54
                                                				   :font *font64*)
                                    		    (make-instance 'game-end-btn :pos (gk:vec2 570 120)
                                                  				 :font *font64*
                                                  				 :string "おわる" :color (gk:vec4 1 0.5 0.4 1)
										 :w 145 :h 54)))
	*mouse* (make-instance 'mouse)
	*ability-dice* (make-instance 'ability-dice))
  ;;(test-create-party-chara)
  (bind-mouse-event))






;;視界 表示できる地形座標ゲット
(defun get-show-cell-coord ()
  (with-slots (party) *game*
    (with-slots (tate yoko p-sight-coord) *battle-field*
      (setf p-sight-coord nil
	    p-sight-coord (loop :for p :in party
				:append (with-slots (sight) p
					  (get-area p sight t nil)))))))


(defun set-battle-field ()
  (setf *battle-field* (make-instance 'donjon))
  (create-battle-field))

;;------------------------------------------------------------------------------
;;マウスのxy座標
(defun get-mouse-coord ()
  (with-slots (x y) *mouse*
    (values (floor x *battle-obj-w*) (floor y *battle-obj-h*))))
;;------------------------------------------------------------------------------
;;種族ごとのランダム初期能力値
(defun get-ability-dice-by-race (race)
  (cond
    ((string= race "人間")
     (values (dice 2 6) (dice 2 6) (dice 2 6) (dice 2 6) (dice 2 6) (dice 2 6)))
    ((string= race "エルフ")
     (values (dice 2 6) (dice 2 6) (dice 1 6) (dice 2 6) (dice 2 6) (dice 2 6)))
    ((string= race "ドワーフ")
     (values (+ (dice 2 6) 6) (dice 1 6) (dice 1 6) (dice 2 6) (dice 1 6) (+ (dice 2 6) 6)))
    ((string= race "タビット")
     (values (dice 1 6) (dice 1 6) (dice 1 6) (dice 2 6) (+ (dice 2 6) 6) (dice 2 6)))
    ((string= race "ルーンフォーク")
     (values (dice 2 6) (dice 1 6) (dice 2 6) (dice 2 6) (dice 2 6) (dice 1 6)))
    ((string= race "ナイトメア")
     (values (dice 2 6) (dice 2 6) (dice 1 6) (dice 1 6) (dice 2 6) (dice 2 6)))
    ((string= race "リカント")
     (values (dice 1 6) (+ (dice 1 6) 3) (dice 2 6) (dice 2 6) (+ (dice 1 6) 6) (dice 1 6)))))
;;------------------------------------------------------------------------------
;;当たり判定
(defmethod collide-p ((mouse mouse) obj)
  (with-slots (x y) mouse
    (with-slots (pos w h) obj
      (let* ((x1 (gk:x pos))
             (y1 (gk:y pos))
             (x2 (+ x1 w))
             (y2 (+ y1 h)))
        (and (>= x2 x x1)
             (>= y2 y y1))))))

;; (defmethod collide-p ((mouse mouse) (btn command-btn))
;;   (with-slots (x-for-obj y-for-obj) mouse
;;     (with-slots (pos w h) btn
;;       (let* ((x1 (gk:x pos))
;;              (y1 (gk:y pos))
;;              (x2 (+ x1 w))
;;              (y2 (+ y1 h)))
;;         (and (>= x2 x-for-obj x1)
;;              (>= y2 y-for-obj y1))))))

;; (defmethod collide-p ((mouse mouse) (btn button))
;;   (with-slots (x y) mouse
;;     (with-slots (pos w h) btn
;;       (let* ((x1 (-  (gk:x pos) 3))
;;              (y1 (-  (gk:y pos) 7))
;;              (x2 (+ x1 w))
;;              (y2 (+ y1 h)))
;;         (and (>= x2 x x1)
;;              (>= y2 y y1))))))

;; area 座標リスト
(defmethod collide-p ((mouse mouse) (area list))
  (multiple-value-bind (mouse-x mouse-y) (get-mouse-coord)
    (let* ((mouse-xy (list mouse-x mouse-y)))
      (find mouse-xy area :test #'equal))))
;;------------------------------------------------------------------------------------
;;レベルアップ時のステータスアップボタン
(defun create-status-up-btn ()
  (with-slots (btn-list) *game*
    (let ((string-list (loop :repeat 2
			       :collect (let ((dice1 (dice 1 6)))
					  (case dice1
					    (1 "器用度")
					    (2 "敏捷度")
					    (3 "筋力")
					    (4 "生命力")
					    (5 "知力")
					    (6 "精神力"))))))
      (loop :for string :in string-list
	    :for posx :from 340 :to 900 :by 300
	    :for posy = 550
	    :do (push (make-instance 'status-up-btn :pos (gk:vec2 posx posy) :box? t
						:string string :w 200 :h 60
						:font *font64* :color (gk:vec4 1 0 1 1))
		      btn-list)))))

;;------------------------------------------------------------------------------------
;;ランダム種族ゲット
(defun get-random-race-tag ()
  (nth (random (length *all-race-tag-list*)) *all-race-tag-list*))

;;ゲット種族の名前
(defun get-race-name (race-data)
  (getf race-data :name))

;;ユニットに種族の名前セット
(defun set-race-name (unit race-data)
  (setf (race unit) (get-race-name race-data)))

;;ランダム初期技能ゲット
(defun get-random-init-job-tag (race-data)
  (let ((job-list (getf race-data :init-job-list)))
    (nth (random (length job-list)) job-list)))

;;初期技能レベルアップ
(defun init-job-level-up (unit init-job-tag)
  (with-slots (job-level-list) unit
    (cond
      ((eq init-job-tag :none)
       nil)
      ((listp init-job-tag)
       (dolist (tag init-job-tag)
	 (incf (getf job-level-list tag))))
      (t
       (incf (getf job-level-list init-job-tag))))))

;;初期基本能力値セット
(defun set-init-ability-value (unit init-job-data)
  (with-slots (con mnd tec) unit
    (setf con (getf init-job-data :con)
	  mnd (getf init-job-data :mnd)
	  tec (getf init-job-data :tec))))

;;画像セット
(defun set-job-img (unit init-job-data)
  (let ((img (getf init-job-data :img)))
    (setf (origin unit) (gk:vec2 0 (* img *origin-obj-h*))
	  (translate-y unit) (- (* img *battle-obj-h*)))))

;;初期経験点セット
(defun set-init-exp-point (unit init-job-data)
  (setf (exp-point unit) (getf init-job-data :exp-point)))

;;初期ランダム能力値セット
(defun set-random-ability-value (unit)
  (with-slots (race con mnd tec str dex agi vit res int move) unit
    (multiple-value-bind (dex-value agi-value str-value vit-value int-value res-value) (get-ability-dice-by-race race)
      (setf str (+ con str-value) dex (+ tec dex-value) int (+ mnd int-value)
	    agi (+ tec agi-value) vit (+ con vit-value) res (+ mnd res-value)
	    move (max 4 (floor agi 3))))))

;;初期技能ランダムレベルアップ
(defun init-random-job-level-up (unit)
  (with-slots (exp-point job-level-list) unit
    (loop :for i :from 0
	  :do
	     (let* ((level-up-available-list
		      (remove nil (loop :for job-tag :in *all-job-list*
					:collect  (let* ((job-data (getf *all-job-tag-and-data-list* job-tag))
							 (exp-table (getf job-data :exp-point-table))
							 (now-job-level (getf job-level-list job-tag))
							 (required-exp-point (getf exp-table (1+ now-job-level))))
						    (when (>= exp-point required-exp-point)
						      (list job-tag required-exp-point)))))))
	       (if level-up-available-list
		   (let ((random-job-tag (nth (random (length level-up-available-list)) level-up-available-list)))
		     (incf (getf job-level-list (car random-job-tag)))
		     (decf exp-point (cadr random-job-tag)))
		   (return))))))

;;初期特技セット
(defun set-init-random-skill (unit)
  (with-slots (passive-skill action-skill declare-skill) unit
    (let* ((skill-tag (nth (random (length *init-skill-tag-list*)) *init-skill-tag-list*))
	   (skill-type (getf (getf *all-skill-tag-and-data* skill-tag) :skill-type)))
      (case skill-type
	(:passive (push skill-tag passive-skill))
	(:action (push skill-tag action-skill))
	(:declare (push skill-tag declare-skill))))))

;;名前セット
(defun set-random-unit-name (unit)
  (setf (name unit) (nth (random (length *name-list*)) *name-list*)))

;;movecostセット
(defun set-move-cost (unit race-data)
  (setf (movecost unit) (getf race-data :movecost)))

;;ランダムユニット作成
(defun create-random-unit ()
  (let* ((unit (make-instance 'unit :w *battle-obj-w* :h *battle-obj-h* :team :player))
	 (race-tag (get-random-race-tag))
	 (race-data (getf *all-race-tag-and-data-list* race-tag))
	 (init-race-job-tag (get-random-init-job-tag race-data))
	 (init-job-data (getf *init-job-data-list* init-race-job-tag))
	 (init-job-tag (getf init-job-data :tag)))
    (set-race-name unit race-data)
    (set-move-cost unit race-data)
    (init-job-level-up unit init-job-tag)
    (set-init-exp-point unit init-job-data)
    (set-init-ability-value unit init-job-data)
    (set-job-img unit init-job-data)
    (set-random-ability-value unit)
    (init-random-job-level-up unit)
    (set-init-random-skill unit)
    (set-random-unit-name unit)
    (unit-status-adjust unit)
    ;;debug
    (push :energy-bolt (magics unit))
    (push :fire (magics unit))
    (push :me-number (declare-skill unit))
    (push :me-area (declare-skill unit))
    (push :me-distance (declare-skill unit))
    ;;debug
    unit))

;;--------------------------------------------------------------------------------
;;装備アイテムボタンつくる
(defun create-item-btn ()
  (with-slots (item btn-list item-page) *game*
    (setf btn-list nil)
    (loop :for i :from (* item-page *item-show-max*) :below (length item)
	  :repeat *item-show-max*
	  :for posx = 60
	  :for posy :from 750 :downto 0 :by 32
	  :do (let* ((itemun (nth i item))
		     (btn (make-instance 'equip-item-btn
					 :pos (gk:vec2 posx posy)
					 :w 250 :h 30
					 :new (new itemun)
					 :equiped-unit (equiped itemun)
					 :item itemun
					 :font *font32*
					 :color (gk:vec4 1 1 1 1)
					 :string (name itemun))))
		(push btn btn-list)))))

;;装備メニュー画面の次と前ボタンと終了ボタン
(Defun create-next-prev-btn ()
  (with-slots (btn-list) *game*
    (let ((next-btn (make-instance 'next-item-page
				   :pos (gk:vec2 265 85) :box? t
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "次→"))
	  (prev-btn (make-instance 'prev-item-page
				   :pos (gk:vec2 25 85) :box? t
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "←前"))
	  (end-btn (make-instance 'end-equip-menu-btn
				  :pos (gk:vec2 130 40) :box? t
				  :w 70 :h 40 :font *font48*
				  :color (gk:vec4 1 1 0 1)
				  :string "終了")))
      (push next-btn btn-list)
      (push prev-btn btn-list)
      (push end-btn btn-list))))


;;装備メニュー開く
(defun open-equip-menu ()
  (create-item-btn)
  (create-next-prev-btn))
;;------------------------------------------------------------------------------------
;;店売ってるアイテムのボタン作る
(defun create-sale-item-btn ()
  (with-slots (selected-town btn-list) *game*
    (with-slots (sale-item sale-item-page) selected-town
	(setf btn-list nil)
	(loop :for i :from (* sale-item-page *sale-item-show-max*) :below (length sale-item)
	      :repeat *sale-item-show-max*
	      :for posx = 160
	      :for posy :from 680 :downto 0 :by 40
	      :do (let* ((itemun (nth i sale-item)))
		    (setf (new itemun) t)
		    (push (make-instance 'sale-item-btn
					 :pos (gk:vec2 posx posy)
					 :w 250 :h 38
					 :price (price itemun)
					 :item itemun
					 :font *font48*
					 :color (gk:vec4 1 1 1 1)
					 :string (name itemun))
			  btn-list))))))

;;店の次へ前へボタン
(Defun create-shop-next-prev-end-btn ()
  (with-slots (btn-list) *game*
    (let ((next-btn (make-instance 'shop-next-item-page
				   :pos (gk:vec2 570 60) :box? t
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "次→"))
	  (prev-btn (make-instance 'shop-prev-item-page
				   :pos (gk:vec2 140 60) :box? t
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "←前"))
	  (end-btn (make-instance 'end-shop-btn :pos (gk:vec2 30 40) :box? t
						:w 70 :h 40 :font *font48*
						:color (gk:vec4 1 1 1 1)
						:string "戻る")))
      (push next-btn btn-list)
      (push prev-btn btn-list)
      (push end-btn btn-list))))
;;------------------------------------------------------------------------------------
;;初期パーティ作成用ボタン生成
(defun create-init-job-btn ()
  (with-slots (btn-list) *game*
    (loop :for job-name :in *jon-name-list*
	  :for posx = 150
	  :for posy :from 600 :downto 0 :by 64
	  :do (push (make-instance 'init-jon-btn :string job-name :w 200 :h 50
						 :pos (gk:vec2 posx posy)
						 :font *font64*
						 :color *white*)
		    btn-list))
    (push (make-instance 'create-init-party-end-btn
			 :string "はじめる" :w 200 :h 50 :box? t
			 :pos (gk:vec2 930 220) :font *font64* :color *white*)
	  btn-list)))

;;初期パーティボタン ポジション再設定
(defun adjust-init-party-btn ()
  (with-slots (temp-init-party) *game*
    (loop :for btn :in temp-init-party
	  :for posy :from 600 :downto 0 :by 64
	  :do (with-slots (pos) btn
		(setf (gk:y pos) posy)))))

;;------------------------------------------------------------------------------------
;;種族選択ボタン
(defun create-race-btn ()
  (with-slots (btn-list) *game*
    (loop :for posx = 220
	  :for posy :from 550 :downto 0 :by 80
	  :for race-tag :in *all-race-tag-list*
	  :do (let ((race-data (getf *all-race-tag-and-data-list* race-tag)))
		(push (make-instance 'race-btn :pos (gk:vec2 posx posy)
					     :tag race-tag
					     :string (getf race-data :name)
					     :w 400 :h 60
					     :font *font64* :color *white*)
		    btn-list)))))

;;------------------------------------------------------------------------------------
;;初期技能選択ボタン
(defun create-job-btn (race-job-list)
  (with-slots (btn-list) *game*
    (loop :for posx = 150
	  :for posy :from 570 :downto 0 :by 76
	  :for job :in race-job-list
	  :do (let* ((data (getf *init-job-data-list* job))
		     (name (getf data :name)))
		(push (make-instance 'job-btn :pos (gk:vec2 posx posy)
						:string name :job job
						:w 500 :h 60
						:font *font64* :color *white*)
		      btn-list)))
    (push (make-instance 'back-select-race-btn :pos (gk:vec2 20 30) :w 100 :h 50
					       :font *font64* :color *white*
					       :string "戻る")
	  btn-list)))
;;------------------------------------------------------------------------------------
;;初期能力値ダイスボタン
(defun create-ability-dice-btn ()
  (with-slots (btn-list) *game*
    (push (make-instance 'ability-dice-btn :pos (gk:vec2 200 50) :box? t
					   :font *font64* :color (gk:vec4 0 0.5 1 1)
					   :string "サイコロを振る" :w 370 :h 50)
	  btn-list)
    (push (make-instance 'ability-dice-end-btn :pos (gk:vec2 960 50) :box? t
					   :font *font64* :color (gk:vec4 1 0.5 1 1)
					   :string "次へ" :w 100 :h 50)
	  btn-list)
    (push (make-instance 'back-select-job-btn :pos (gk:vec2 20 30) :w 100 :h 50
					       :font *font64* :color *white*
					      :string "戻る")
	  btn-list)))


;;------------------------------------------------------------------------------------
;;初期技能レベルアップボタン
(defun create-job-level-up-btn ()
  (with-slots (btn-list) *game*
    (loop :for tag :in *all-job-list*
	  :for posx = 140
	  :for posy :from 610 :downto 0 :by 59
	  :do (let* ((job-data (getf *all-job-tag-and-data-list* tag))
		     (name (getf job-data :name)))
		(push (make-instance 'job-level-up-btn :pos (gk:vec2 posx posy)
						       :job tag :string name :w 220 :h 44
						       :font *font48* :color *white*)
		      btn-list)))
    (push (make-instance 'back-ability-dice-btn :pos (gk:vec2 20 30) :w 100 :h 50
					    :font *font64* :color *white*
					    :string "戻る")
	  btn-list)
    (push (make-instance 'job-level-up-end-btn :pos (gk:vec2 960 30) :w 100 :h 50
					       :font *font64* :color *white*
					       :string "次へ")
	  btn-list)))
;;------------------------------------------------------------------------------------
;;初期特技ボタン
(defun create-init-skill-btn ()
  (with-slots (btn-list) *game*
    (let ((posx 80)
	  (posy 700))
      (loop :for skill-tag :in *init-skill-tag-list*
	    :for i :from 1 :to 50
	    :do (let* ((skill-data (getf *all-skill-tag-and-data* skill-tag))
		       (name (getf skill-data :name))
		       (skill-type (getf skill-data :skill-type)))
		  (push (make-instance 'init-skill-btn :string name
						       :skill-type skill-type :tag skill-tag
						       :pos (gk:vec2 posx posy)
						       :w 370 :h 38
						       :description (getf skill-data :description)
						       :font *font40* :color *white*)
			btn-list)
		  (incf posx 380)
		  (when (zerop (mod i 3))
		    (setf posx 80
			  posy (- posy 42)))))
      (push (make-instance 'back-to-select-job-level-up-btn :pos (gk:vec2 20 30) :w 100 :h 50
					    :font *font64* :color *white*
					    :string "戻る")
	    btn-list))))
;;------------------------------------------------------------------------------------
;;初期作成ユニットステータス表示
(defun create-show-init-player-unit-status-btn ()
  (with-slots (btn-list) *game*
    (push (make-instance 'back-to-select-init-skill-btn :pos (gk:vec2 20 30) :w 100 :h 50
					    :font *font64* :color *white*
					    :string "戻る")
	  btn-list)
    (push (make-instance 'create-init-player-unit-end-btn :pos  (gk:vec2 960 30) :w 100 :h 50
							  :font *font64* :color *white*
							  :string "開始")
	  btn-list)))
;;------------------------------------------------------------------------------------
;;仲間募集
;;ランダムユニットボタン
(defun create-recruit-random-unit-btn ()
  (with-slots (btn-list) *game*
    (loop :repeat 5
	  :for posx = 50
	  :for posy :from 600 :downto 0 :by 50
	  :do (let ((unit (create-random-unit)))
		(push (make-instance 'recruit-random-unit-btn :pos (gk:vec2 posx posy)
							      :string (name unit)
							      :w 200 :h 40 :unit unit
							      :font *font40* :color *white*)
		      btn-list)))
    (push (make-instance 'end-recruit-btn :pos (gk:vec2 20 30) :w 100 :h 50
					    :font *font64* :color *white*
					  :string "戻る")
	  btn-list)))

;;;;------------------------------------------------------------------------------------
;;パーティユニットステータス表示＆外す
(defun create-show-unit-status-btn ()
  (with-slots (btn-list party) *game*
    (loop :for p :in party
	  :for posx = 50
	  :for posy :from 600 :downto 0 :by 50
	  :do (push (make-instance 'show-unit-status-btn :pos (gk:vec2 posx posy)
							 :string (name p)
							 :w 200 :h 40 :unit p
							 :font *font40* :color *white*)
		    btn-list))
    (push (make-instance 'end-recruit-btn :pos (gk:vec2 20 30) :w 100 :h 50
					  :font *font64* :color *white*
					  :string "戻る")
	  btn-list)))
;;------------------------------------------------------------------------------------
(defmethod equip-item ((item weapondesc) unit)
  (with-slots (weapon name shield) unit
    (unless (and (eq (hand item) :2h) ;;盾持ってる場合2hは装備できない
		 shield)
      (when weapon
	(setf (equiped weapon) nil))
      (setf weapon item
	    (equiped weapon) name))))

(defmethod equip-item ((item armordesc) unit)
  (with-slots (armor name) unit
    (when armor
      (setf (equiped armor) nil))
    (setf armor item
	  (equiped armor) name)))

;;盾装備
(defun equip-shield (item unit)
  (with-slots (shield name weapon) unit
    (when shield
      (setf (equiped shield) nil))
    (setf shield item
	  (equiped shield) name)))
;;盾装備
(defmethod equip-item ((item shielddesc) unit)
  (with-slots (shield name weapon) unit
    (if weapon
	(unless (eq (hand weapon) :2h)
	  (equip-shield item unit))
	(equip-shield item unit))))

;;アイテム装備
(defmethod equip-item ((item use-item) unit)
  (with-slots (use-item name) unit
    (when (> 3 (length use-item))
      (setf (equiped item) name)
      (push item use-item))))

;; btn event ----------------------------------------------------------------


(defmethod btn-click-event ((btn town-exit-btn))
  (with-slots (state btn-list) *game*
    (setf state :world-map
	  btn-list nil)))

(defparameter *town-description*
  '("町を出る" "敵が落としたアイテムを売る" "HP・MPを回復する" "ステータス確認" "仲間を雇う" "買い物"))

;;町メニューボタン
(defun create-town-menu-button ()
  (with-slots (btn-list) *game*
    (setf btn-list nil)
    (loop :for posx = 80
	  :for posy :from 300 :to 1000 :by 70
	  :for des :in *town-description*
	  :for kind :in (list 'town-exit-btn 'cash-exchange-btn 'inn-btn 'show-party-status-btn 'recruit-btn 'shop-btn)
	  :for str :in '("町を出る" "換金アイテムを売る ""宿屋" "ステータス確認 ""仲間募集" "買い物")
	  :do (let* ((len (length str))
		     (btn (make-instance kind :pos (gk:vec2 posx posy)
					     :w (* len 52) :h 50
					      :font *font64*
					      :description des
					     :color (gk:vec4 1 1 1 1)
					     :string str)))
		(push btn btn-list)))))



(defun debug-create-random-unit ()
  (with-slots (party) *game*
    (loop :repeat 5
	  :do (push (create-random-unit) party))))



;;ゲーム開始ボタン キャラクリへ行く TODO
(defmethod btn-click-event ((btn game-start-btn))
  (with-slots (state action-state btn-list selected-unit item) *game*
    (setf state :battle-ready ;;:select-race  ;;debug
	  btn-list nil
	  action-state :player-turn)
	  ;; selected-unit (make-instance 'unit :w *battle-obj-w* :h *battle-obj-h*
	  ;; 				     :lvup-exp 100 ;;test
	  ;; 				     :state :inaction :img-id :job-img
	  ;; 				     :name (nth (random (length *name-list*)) *name-list*)))
    ;; (let ((weapon (item-make +w_knife+)))
    ;;   (setf (equiped weapon) (name selected-unit)
    ;; 	    (weapon selected-unit) weapon)
    ;;   (push weapon item))
    ;; (create-race-btn)))
    ;;(create-init-job-btn)))
    (debug-create-random-unit)
    (set-battle-field)
    (set-party-battle-ready-pos)
    (get-show-cell-coord)))


;;タイトル画面のゲーム終了ボタン
(defmethod btn-click-event ((btn game-end-btn))
  (gk:stop))


;;種族選択ボタン
(defmethod btn-click-event ((btn race-btn))
  (with-slots (selected-unit state btn-list) *game*
    (with-slots (string tag) btn
      (let ((race-data (getf *all-race-tag-and-data-list* tag)))
	(setf (race selected-unit) string
	      (movecost selected-unit) (getf race-data :movecost)
	      state :select-job
	      btn-list nil)
	(cond
	  ((string= string "人間") (create-job-btn *init-human-job-list*))
	  ((string= string "エルフ") (create-job-btn *init-elf-job-list*))
	  ((string= string "ドワーフ") (create-job-btn *init-dwarf-job-list*))
	  ((string= string "タビット") (create-job-btn *init-tabbit-job-list*))
	  ((string= string "ルーンフォーク") (create-job-btn *init-runefolk-job-list*))
	  ((string= string "ナイトメア") (create-job-btn *init-nightmare-job-list*))
	  ((string= string "リカント") (create-job-btn *init-lycant-job-list*)))))))

;;種族選択画面へ戻るボタン
(defmethod btn-click-event ((btn back-select-race-btn))
  (with-slots (btn-list state) *game*
    (with-slots (str-dice dex-dice vit-dice int-dice agi-dice res-dice num) *ability-dice*
      (setf btn-list nil
	    state :select-race
	    str-dice 0 dex-dice 0 vit-dice 0 int-dice 0 agi-dice 0 res-dice 0 num 3)
      (create-race-btn))))

;;技能選択ボタン
(defmethod btn-click-event ((btn job-btn))
  (with-slots (selected-unit state btn-list) *game*
    (with-slots (job-level-list con tec mnd exp-point origin translate-y) selected-unit
      (with-slots (job) btn
	(let* ((data (getf *init-job-data-list* job))
	       (img (getf data :img))
	       (con1 (getf data :con))
	       (mnd1 (getf data :mnd))
	       (tec1 (getf data :tec))
	       (exp-1 (getf data :exp-point))
	       (tag (getf data :tag)))
	  (cond ((eq tag :none)
		 nil)
		((listp tag) ;;初期技能複数の場合あり
		 (dolist (n tag)
		   (incf (getf job-level-list n))))
		(t
		 (incf (getf job-level-list tag))))
	  (setf state :dice-init-ability
		con con1 mnd mnd1 tec tec1
		exp-point exp-1
		btn-list nil
		origin (gk:vec2 0 (* img *origin-obj-h*))
		translate-y (- (* img *battle-obj-h*)))
	  (create-ability-dice-btn))))))

;;技能選択画面へ戻るボタン
(defmethod btn-click-event ((btn back-select-job-btn))
  (with-slots (selected-unit btn-list state) *game*
    (with-slots (job-level-list race) selected-unit
      (setf job-level-list (copy-tree *job-level-list*)
	    btn-list nil
	    state :select-job)
      (cond
	((string= race "人間") (create-job-btn *init-human-job-list*))
	((string= race "エルフ") (create-job-btn *init-elf-job-list*))
	((string= race "ドワーフ") (create-job-btn *init-dwarf-job-list*))
	((string= race "タビット") (create-job-btn *init-tabbit-job-list*))
	((string= race "ルーンフォーク") (create-job-btn *init-runefolk-job-list*))
	((string= race "ナイトメア") (create-job-btn *init-nightmare-job-list*))
	((string= race "リカント") (create-job-btn *init-lycant-job-list*))))))


;;初期能力値ダイスボタン
(defmethod btn-click-event ((btn ability-dice-btn))
  (with-slots (selected-unit) *game*
    (with-slots (race) selected-unit
      (with-slots (str-dice agi-dice vit-dice res-dice int-dice dex-dice num) *ability-dice*
	(when (> num 0)
	  (decf num)
	  (multiple-value-bind (dex agi str vit int res) (get-ability-dice-by-race race)
	    (setf str-dice str dex-dice dex vit-dice vit res-dice res int-dice int agi-dice agi)))))))

;;初期能力値決定　終了ボタン
(defmethod btn-click-event ((btn ability-dice-end-btn))
  (with-slots (selected-unit state btn-list) *game*
    (with-slots (str dex agi vit int res con mnd tec move) selected-unit
      (with-slots (str-dice agi-dice vit-dice res-dice int-dice dex-dice) *ability-dice*
	(setf str (+ con str-dice) dex (+ tec dex-dice) vit (+ con vit-dice) res (+ mnd res-dice)
	      agi (+ tec agi-dice)  int (+ mnd int-dice)
	      btn-list nil
	      move (max 4 (floor agi 3)) ;;移動力 敏捷度/2
	      state :init-job-level-up)
	(create-job-level-up-btn)))))

;;初期能力値決定画面へ戻る
(defmethod btn-click-event ((btn back-ability-dice-btn))
  (with-slots (selected-unit btn-list state) *game*
    (with-slots (job-level-list race) selected-unit
      (setf btn-list nil
	    state :dice-init-ability)
      (create-ability-dice-btn))))

;;初期技能レベルアップボタン
(defmethod btn-click-event ((btn job-level-up-btn))
  (with-slots (selected-unit) *game*
    (with-slots (exp-point job-level-list) selected-unit
      (with-slots (job) btn
	(let* ((now-level (getf job-level-list job))
	       (job-data (getf *all-job-tag-and-data-list* job))
	       (exp-table (getf job-data :exp-point-table))
	       (required-point (getf exp-table (1+ now-level))))
	  (when (and (>= 2 now-level)
		     (>= exp-point required-point))
	    (incf (getf job-level-list job))
	    (decf exp-point required-point)))))))

;;初期技能レベルアップ終了
(defmethod btn-click-event ((btn job-level-up-end-btn))
  (with-slots (state btn-list) *game*
    (setf state :select-init-skill
	  btn-list nil)
    (create-init-skill-btn)))

;;初期技能レベルアップ画面へ戻る
(defmethod btn-click-event ((btn back-to-select-job-level-up-btn))
  (with-slots (state btn-list) *game*
      (setf state :init-job-level-up
	    btn-list nil)
      (create-job-level-up-btn)))

;;初期特技ボタン 選んだらステータス表示画面へ
(defmethod btn-click-event ((btn init-skill-btn))
  (with-slots (state btn-list selected-unit) *game*
    (with-slots (passive-skill declare-skill action-skill) selected-unit
      (with-slots (tag skill-type) btn
	(case skill-type
	  (:passive (push tag passive-skill))
	  (:declare (push tag declare-skill))
	  (:action (push tag action-skill)))
	(unit-status-adjust selected-unit)
	(setf state :show-init-player-unit-status
	      btn-list nil)
	(create-show-init-player-unit-status-btn)))))


;;初期スキル選択画面へ戻る
(defmethod btn-click-event ((btn back-to-select-init-skill-btn))
  (with-slots (state btn-list selected-unit) *game*
    (with-slots (passive-skill declare-skill action-skill) selected-unit
      (setf state :select-init-skill
	    btn-list nil
	    passive-skill nil declare-skill nil action-skill nil)
      (create-init-skill-btn))))

;;ゲーム開始　キャラクリｵﾜﾘ
(defmethod btn-click-event ((btn create-init-player-unit-end-btn))
  (with-slots (state party btn-list selected-unit selected-town action-state) *game*
    (push selected-unit party)
    (setf state :town
	  btn-list nil
	  selected-unit nil
	  action-state :town-menu
	  selected-town (getf *town-list* 5))
    (create-town-menu-button)))


;;次へボタン
(defmethod btn-click-event ((btn next-item-page))
  (with-slots (item item-page btn-list) *game*
    (let ((max-item-page (floor (length item) *item-show-max*)))
      (cond
	((> max-item-page item-page)
	 (incf item-page))
	((= max-item-page item-page)
	 (setf item-page 0)))
      (create-item-btn)
      (create-next-prev-btn))))

;;前へボタン
(defmethod btn-click-event ((btn prev-item-page))
  (with-slots (item item-page btn-list) *game*
    (let ((max-item-page (floor (length item) *item-show-max*)))
      (cond
	((= item-page 0)
	 (setf item-page max-item-page))
	((> item-page 0)
	 (decf item-page)))
      (create-item-btn)
      (create-next-prev-btn))))

;;装備メニュー終了ボタン
(defmethod btn-click-event ((btn end-equip-menu-btn))
  (with-slots (state btn-list selected-unit item item-page) *game*
    (setf state :battle
	  btn-list nil
	  itme-page 0)
    (create-action-command-btn selected-unit)
    (loop :for i :in item
	  :do (setf (new i) nil))))

;;アイテムボタン
(defmethod btn-click-event ((btn equip-item-btn))
  (with-slots (item equiped-unit) btn
    (with-slots (selected-unit) *game*
      (with-slots (name str shield) selected-unit
	(with-slots (category equiped required-str hand new) item
	  (print category)
	  (when (and (>= str required-str)
		     (null equiped-unit))
	    (equip-item item selected-unit)
	    (setf new nil)
	    (create-item-btn)
	    (create-next-prev-btn)))))))

;;町メニュー
;;買い物ボタン
(defmethod btn-click-event ((btn shop-btn))
  (with-slots (btn-list action-state) *game*
    (gk:play-sound :button)
    (setf btn-list nil
	  action-state :shop)
    (create-sale-item-btn)
    (create-shop-next-prev-end-btn)))

;;換金アイテム売却
(defmethod btn-click-event ((btn cash-exchange-btn))
  (with-slots (money cash-exchange-item) *game*
    (when cash-exchange-item
      (gk:play-sound :cash-exchange)
      (loop :for item :in cash-exchange-item
	    :do (let ((item-money (price (nth item *cash-exchange-item-list*))))
		  (incf money item-money)))
      (setf cash-exchange-item nil))))

;;宿屋
(defmethod btn-click-event ((btn inn-btn))
  (with-slots (party) *game*
    (gk:play-sound :heal)
    (loop :for p :in party
	  :do (with-slots (hp maxhp mp maxmp) p
		(setf hp maxhp
		      mp maxmp)))))

;;仲間募集
(defmethod btn-click-event ((btn recruit-btn))
  (with-slots (btn-list action-state selected-town) *game*
    (setf btn-list nil
	  action-state :recruit)
    (create-recruit-random-unit-btn)))

;;仲間ステータス確認
(defmethod btn-click-event ((btn show-party-status-btn))
   (with-slots (btn-list action-state selected-town) *game*
    (setf btn-list nil
	  action-state :show-status)
     (create-show-unit-status-btn)))

;;仲間ステータス確認画面　右クリックで仲間を外す
(defmethod btn-click-event ((btn show-unit-status-btn))
  (with-slots (party btn-list) *game*
    (with-slots (unit) btn
      (when (> (length party) 1)
	(setf party (remove unit party :test #'equal)
	      btn-list (remove btn btn-list :test #'equal))))))

;;仲間ゲット
(defmethod btn-click-event ((btn recruit-random-unit-btn))
  (with-slots (party btn-list) *game*
    (with-slots (unit) btn
      (when (> 5 (length party))
	(push unit party)
	(setf btn-list (remove btn btn-list :test #'equal))))))

;;仲間募集から戻る
(defmethod btn-click-event ((btn end-recruit-btn))
  (with-slots (btn-list action-state selected-town) *game*
    (setf btn-list nil
	  action-state :town-menu)
    (create-town-menu-button)))

;;店から戻るボタン
(defmethod btn-click-event ((btn end-shop-btn))
  (with-slots (btn-list action-state selected-town) *game*
    (setf btn-list nil
	  action-state :town-menu
	  (sale-item-page selected-town) 0)
    (gk:play-sound :cancel)
    (create-town-menu-button)))

;;店次へボタン
(defmethod btn-click-event ((btn shop-next-item-page))
  (with-slots (selected-town) *game*
    (with-slots (sale-item-page sale-item) selected-town
      (let ((max-item-page (floor (length sale-item) *sale-item-show-max*)))
	(cond
	  ((> max-item-page sale-item-page)
	   (incf sale-item-page))
	  ((= max-item-page sale-item-page)
	   (setf sale-item-page 0)))
	(create-sale-item-btn)
	(create-shop-next-prev-end-btn)
	(gk:play-sound :button)))))

;;店前へボタン
(defmethod btn-click-event ((btn shop-prev-item-page))
  (with-slots (selected-town) *game*
    (with-slots (sale-item-page sale-item) selected-town
      (let ((max-item-page (floor (length sale-item) *sale-item-show-max*)))
	(cond
	  ((= sale-item-page 0)
	   (setf sale-item-page max-item-page))
	  ((> sale-item-page 0)
	   (decf sale-item-page)))
	(create-sale-item-btn)
	(create-shop-next-prev-end-btn)
	(gk:play-sound :button)))))

;;店の商品ボタン
(defmethod btn-click-event ((btn sale-item-btn))
  (with-slots (money) *game*
    (with-slots (item) btn
      (when (>= money (price item))
	(gk:play-sound :buy)
	(push (shallow-copy-object item) (game/item *game*))
	(decf money (price item))))))

;;レベルアップボタン
(defmethod btn-click-event ((btn status-up-btn))
  (with-slots (selected-unit action-state btn-list state) *game*
    (with-slots (str dex agi vit res int level) selected-unit
      (with-slots (string) btn
	(cond
	  ((string= string "筋力") (incf str))
	  ((string= string "器用度") (incf dex))
	  ((string= string "生命力") (incf vit))
	  ((string= string "知力") (incf int))
	  ((string= string "敏捷度") (incf agi))
	  ((string= string "精神力") (incf res)))
	(incf level)
	(unit-status-adjust selected-unit)
	(setf action-state :player-turn
	      state :battle
	      btn-list nil)))))


;;初期ジョブボタン
(defmethod btn-click-event ((btn init-jon-btn))
  (with-slots (btn-list temp-init-party) *game*
    (with-slots (string pos font color w h) btn
      (let ((party-num (length temp-init-party)))
	(when (> 5 party-num)
	  (gk:play-sound :button)
	  (let ((posy (- 600 (* party-num 64))))
	    (setf temp-init-party (append temp-init-party
					  (list (make-instance 'temp-init-party-btn :string string :w w :h h
						      :pos (gk:vec2 (+ (gk:x pos) 400) posy)
						      :font font :color color))))))))))

;;暫定初期パーティボタン
(defmethod btn-click-event ((btn temp-init-party-btn))
  (with-slots (btn-list temp-init-party) *game*
    (gk:play-sound :cancel)
    (setf temp-init-party (remove btn temp-init-party :test #'equalp))
    (adjust-init-party-btn)))

;;初期パーティ作成終了 ゲーム開始ボタン
(defmethod btn-click-event ((btn create-init-party-end-btn))
  (with-slots (btn-list temp-init-party state) *game*
    (when (> (length temp-init-party) 0)
      (gk:play-sound :button)
      (create-init-party-data)
      (setf btn-list nil
	    temp-init-party nil
	    state :world-map))))
      ;;(go-battle-mode))))

;;------------------------------------------------------------------------------------
;;敵の攻撃を受けるか判定をnilに戻す
(defun init-unit-atked-pos (units)
    (loop :for e :in units
	  :do (setf (atked-pos e) nil)))


;;移動する準備
(defun unit-ready-move (unit move)
  (with-slots (movearea) unit
    (init-unit-atked-pos (game/party *game*))
    (init-unit-atked-pos (enemies *battle-field*))
    (setf movearea (get-area unit move nil nil))
    ;;動く前に攻撃できる敵がいるか見る
    ;;(get-attack-can-reach unit targets)
    ))

;;現在位置での攻撃範囲に敵がいるか
(defun get-attack-can-reach (atk-unit)
  (with-slots (weapon x y movearea atk-dir team) atk-unit
    (let ((rangemax (rangemax weapon))
	  (rangemin (rangemin weapon))
	  ;;(can-atk? nil)
	  (enemies (if (eq team :player) (enemies *battle-field*) (game/party *game*))))
      (loop :for e :in enemies
	    :do (with-slots ((e-x x) (e-y y) atked-pos) e
		  (let* ((diffx (- x e-x))
			 (diffy (- y e-y))
			 (manhatan-dist (+ (abs diffx) (abs diffy))))
		    (cond
		      ;;その場で攻撃届く敵
		      ((>= rangemax manhatan-dist rangemin)
		       (setf atked-pos (list x y)
			     atk-dir (get-atk-dir diffx diffy))))))))))
		      ;;移動可能範囲全部から攻撃届く敵を見つける
		      ;; (t (loop :for area :in movearea
		      ;; 	       :do (let* ((movex (car area))
		      ;; 			  (movey (cadr area))
		      ;; 			  (diffx (- movex e-x))
		      ;; 			  (diffy (- movey e-y))
		      ;; 			  (manhatan-dist (+ (abs diffx) (abs diffy))))
		      ;; 		     (when (>= rangemax manhatan-dist rangemin)
		      ;; 		       (setf atked-pos (list movex movey)
		      ;; 			     atk-dir (get-atk-dir diffx diffy)
		      ;; 			     can-atk? t)
			;;	       (return)))))
      ;; 		      ))))
      ;; can-atk?)))

;;現在地でこうげきできる敵がいるか？
(defun attack-p (unit)
  (with-slots (weapon x y team) unit
    (let* ((new-weapon (if weapon weapon (aref *weapondescs* +w_punch+)))
	   (rangemax (rangemax new-weapon))
	   (rangemin (rangemin new-weapon))
	   (enemies (if (eq team :player) (enemies *battle-field*) (game/party *game*))))
      (loop :for e :in enemies
	    :when (with-slots ((e-x x) (e-y y) atked-pos) e
		    (let* ((diffx (- x e-x))
			   (diffy (- y e-y))
			   (manhatan-dist (+ (abs diffx) (abs diffy))))
		      (>= rangemax manhatan-dist rangemin)))
	      :return t))))
;;----------------------------------------------------------------------------------
;;magic btn
(defun create-magic-btn (unit skill)
  (with-slots (pos) unit
    (let* ((h 25)
	   (posy (gk:y pos))
	   (btn-y (cond ((>= posy (- *window-h* *battle-obj-h*))
			 (+ posy h))
			((< posy *battle-obj-h*)
			 -10)
			((<= posy *battle-obj-h*)
			 (- posy h))
			(t (+ posy (* h 2) h))))
	   (skill-list (if (> btn-y *battle-obj-h*)
			   skill (reverse skill)))
	   (func (if (> btn-y *battle-obj-h*)
		     #'- #'+)))
      (loop :for sk :in skill-list
	    :do (let* ((name (name (getf *all-magic-list* sk)))
		      (w (max 70 (* (length name) 21)))
		      (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
				 (- (gk:x pos) w)
				 (+ (gk:x pos) 50))))
		  (push (make-instance 'magic-btn :pos (gk:vec2 btn-x (setf btn-y (funcall func btn-y (+ h 1))))
						  :tag sk :w w :h h :color (gk:vec4 1 1 1 1) :font *font28*
						  :description (getf *btn-description* sk)
						  :string name)
			(game/btn-list *game*))))
      )))
;;----------------------------------------------------------------------------------
;; declare skill btn
(defun create-declare-skill-btn (unit)
  (with-slots (pos declare-skill active-declare-skill) unit
    (let* ((h 25)
	   (posy (gk:y pos))
	   (btn-y (cond ((>= posy (- *window-h* *battle-obj-h*))
			 *window-h*)
			((< posy *battle-obj-h*)
			 -10)
			((<= posy *battle-obj-h*)
			 (- posy h))
			(t (+ posy (* h 2) h))))
	   (declare-skill-list (if (> btn-y *battle-obj-h*)
				   declare-skill (reverse declare-skill)))
	   (func (if (> btn-y *battle-obj-h*)
		     #'- #'+)))
      (loop :for tag :in declare-skill-list
	    :do (let* ((skill-data (getf *all-skill-tag-and-data* tag))
		       (name (getf skill-data :name))
		       (description (getf skill-data :description))
		       (w (max 70 (* (length name) 21)))
		       (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
				  (- (gk:x pos) w)
				  (+ (gk:x pos) 50)))
		       (color (gk:vec4 1 1 1 1)))
		  (push (make-instance 'declare-skill-btn :pos (gk:vec2 btn-x (setf btn-y (funcall func btn-y (+ h 1))))
							  :tag tag :w w :h h :color color :font *font28*
							  :description description
							  :string name)
			(game/btn-list *game*))))
      )))
;;----------------------------------------------------------------------------------
;; item btn
(defun create-item-cmd-btn (unit use-item)
  (with-slots (pos) unit
    (let* ((w 100) (h 25)
	   (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
		      (- (gk:x pos) w)
		      (+ (gk:x pos) 50)))
	   (posy (gk:y pos))
	   (btn-y (cond ((>= posy (- *window-h* *battle-obj-h*))
			 *window-h*)
			((< posy 32)
			 -10)
			((<= posy 32)
			 (- posy h))
			(t (+ posy 70))))
	   (item-list (if (> btn-y 32)
			   use-item (reverse use-item)))
	   (func (if (> btn-y 32)
		     #'- #'+)))
      (loop :for item :in item-list
	    :do (push (make-instance 'use-item-btn :pos (gk:vec2 btn-x (setf btn-y (funcall func btn-y (+ h 1))))
						   :w w :h h :color (gk:vec4 1 1 1 1) :font *font28*
						   :description (getf *btn-description* (tag item))
						   :item item
						   :string (name item))
		      (game/btn-list *game*))))
    ))


;;----------------------------------------------------------------------------------
;; move btn
(defparameter *move-btn-list* '(("通常移動" normal-move-cmd-btn :normal-move) ("全力移動" fast-move-cmd-btn :fast-move)))

(defun create-all-move-btn (unit)
  (with-slots (btn-list) *game*
    (with-slots (pos) unit
      (let* ((w 100) (h 25)
	     (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
			(- (gk:x pos) w)
			(+ (gk:x pos) 50)))
	     (posy (gk:y pos))
	     (btn-y (cond ((>= posy (- *window-h* *battle-obj-h*))
			   *window-h*)
			  ((< posy 32)
			   -10)
			  ((<= posy 32)
			   (- posy h))
			  (t (+ posy 70))))
	     (move-list (if (> btn-y 32)
			    *move-btn-list* (reverse *move-btn-list*)))
	     (func (if (> btn-y 32)
		       #'- #'+)))
	(loop :for move :in move-list
	      :do (let ((btn-class (cadr move))
			(string (car move))
			(tag (caddr move)))
		    (push (make-instance btn-class :pos (gk:vec2 btn-x (setf btn-y (funcall func btn-y (+ h 1))))
						    :w w :h h :color (gk:vec4 1 1 1 1) :font *font28*
						    :description (getf *btn-description* tag)
						    :string string)
			  btn-list)))
	))))

;;----------------------------------------------------------------------------------

;;画面上側or普通
(defun create-action-command-btn-normal (btn-x btn-y w h unit)
  (with-slots (btn-list) *game*
    (with-slots (state use-item declare-skill magics) unit
      (let ((font *font28*)
	    (line-width (+ h 1)))
	(when (eq state :inaction)
	  (push (make-instance 'move-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
					     :description (getf *btn-description* :normal-move)
					     :string "移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	;; (push (make-instance 'normal-move-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
	;; 					  :description (getf *btn-description* :normal-move)
	;; 					  :string "通常移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	;;       btn-list)
	;; (push (make-instance 'fast-move-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
	;; 					:description (getf *btn-description* :fast-move)
	;; 					:string "全力移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	;;       btn-list))
	(when (attack-p unit)
	  (push (make-instance 'attack-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
					       :description (getf *btn-description* :normal-attack)
					       :string "通常攻撃" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when magics
	  (push (make-instance 'magic-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
					      :description (getf *btn-description* :skill)
					      :string "魔法" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when declare-skill
	  (push (make-instance 'declare-skill-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
						      :description (getf *btn-description* :declare-skill)
						      :string "宣言" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when use-item
	  (push (make-instance 'use-item-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
					     :description (getf *btn-description* :use-item)
					     :string "アイテム" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	;; (push (make-instance 'change-equip-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
	;; 					     :description (getf *btn-description* :change-equip)
	;; 					 :string "装備変更" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	;; 	    btn-list)
	(push (make-instance 'wait-cmd-btn :pos (gk:vec2 btn-x (decf btn-y line-width))
					   :description (getf *btn-description* :wait)
					   :string "待機" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	      btn-list)))))

;;画面下側にいた場合
(defun create-action-command-btn-bottom-side (btn-x btn-y w h unit)
  (with-slots (btn-list) *game*
    (with-slots (state magics use-item declare-skill) unit
      (let ((font *font28*)
	    (line-width (+ h 1)))
	(push (make-instance 'wait-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width)) :description (getf *btn-description* :wait)
					   :string "待機" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	      btn-list)
	;; (push (make-instance 'change-equip-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
	;; 				       :description (getf *btn-description* :change-equip)
	;; 				       :string "装備変更" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	;;       btn-list)
	(when use-item
	  (push (make-instance 'use-item-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
					     :description (getf *btn-description* :use-item)
					     :string "アイテム" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when declare-skill
	  (push (make-instance 'declare-skill-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
					     :description (getf *btn-description* :declare-skill)
					     :string "宣言" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when magics
	  (push (make-instance 'magic-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
					      :description (getf *btn-description* :skill)
					      :string "魔法" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(when (attack-p unit)
	  (push (make-instance 'attack-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
					       :description (getf *btn-description* :normal-attack)
					       :string "通常攻撃" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
		btn-list))
	(push (make-instance 'move-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
						  :description (getf *btn-description* :normal-move)
						  :string "移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
	      btn-list)))))

      ;; (when (eq state :inaction)
      ;; 	(push (make-instance 'fast-move-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
      ;; 						:description (getf *btn-description* :fast-move)
      ;; 						:string "全力移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
      ;; 	      btn-list))
      ;; (push (make-instance 'normal-move-cmd-btn :pos (gk:vec2 btn-x (incf btn-y line-width))
      ;; 						:description (getf *btn-description* :normal-move)
      ;; 						:string "通常移動" :w w :h h :color (gk:vec4 1 1 1 1) :font font)
      ;; 	    btn-list)))))

;; action command btn
(defun create-action-command-btn (unit)
  (with-slots (btn-list) *game*
    (with-slots (pos) unit
      (let* ((w 100) (h 25)
	     (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
			(- (gk:x pos) w)
			(+ (gk:x pos) 50)))
	     (posy (gk:y pos)))
	(cond ((>= posy (- *window-h* *battle-obj-h*))
	       (create-action-command-btn-normal btn-x *window-h* w h unit))
	      ((< posy *battle-obj-h*)
	       (create-action-command-btn-bottom-side btn-x -10 w h unit))
	      ((<= posy *battle-obj-h*)
	       (create-action-command-btn-bottom-side btn-x (- posy h)  w h unit))
	      (t (create-action-command-btn-normal btn-x (+ posy h h h) w h unit)))))))

;;コマンドボタンイベント
;;移動ボタン
(defmethod click-command-btn ((btn move-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf  action-state :select-move-mode
	   btn-list nil)
    (create-all-move-btn unit)))

;;通常移動
(defmethod click-command-btn ((btn normal-move-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf  action-state :move-mode
	   btn-list nil
	  (selected-cmd unit) :normal-move)
    (unit-ready-move unit (move unit))))

;;全力移動
(defmethod click-command-btn ((btn fast-move-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :move-mode
	  btn-list nil
	(selected-cmd unit) :fast-move)
    (unit-ready-move unit (* (move unit) 2))))

;;通常攻撃
(defmethod click-command-btn ((btn attack-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :attack-mode
	  btn-list nil)
    (get-attack-can-reach unit)))

;;魔法
(defmethod click-command-btn ((btn magic-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :select-skill-mode
	  btn-list nil)
    (create-magic-btn unit (magics unit))))

;;宣言スキル
(defmethod click-command-btn ((btn declare-skill-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :select-declare-skill-mode
	  btn-list nil)
    (create-declare-skill-btn unit)))

;;アイテム
(defmethod click-command-btn ((btn use-item-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :select-item-mode
	  btn-list nil)
    (create-item-cmd-btn unit (use-item unit))))

;;装備変更ボタン
(defmethod click-command-btn ((btn change-equip-btn) unit)
  (with-slots (state btn-list) *game*
    (setf state :equip-menu
	  btn-list nil)
    (open-equip-menu)))

;;待機
(defmethod click-command-btn ((btn wait-cmd-btn) unit)
  (with-slots (btn-list action-state) *game*
    (gk:play-sound :button)
    (setf btn-list nil
	  action-state :player-turn)
    (unit-action-end unit)))
;;----------------------------------------------------------------------------------
;;ユニット同氏のマンハッタン距離
(defun manhatan-unit (u1 u2)
  (let ((diffx (- (x u1) (x u2)))
	(diffy (- (y u1) (y u2))))
    (+ (abs diffx) (abs diffy))))


(defun reverse-dir (v)
  (cond ((equal '(0 1) v) '(0 -1))
	((equal '(0 -1) v) '(0 1))
	((equal '(1 0) v) '(-1 0))
	((equal '(-1 0) v) '(1 0))))

;;ユニットの移動可能範囲と視界取得
(defun get-can-move-cell (unit x y move movecost enemies allies next sight-mode magic-mode)
  (with-slots (tate yoko field-array p-sight-coord) *battle-field*
    (when (and (>= (1- yoko) x 0) (>= (1- tate) y 0))
      (let* ((cell (aref field-array y x))
             (cost (aref movecost cell))
	     (enemy (if sight-mode
			nil
			(find-if #'(lambda (p) (and (= x (x p)) (= y (y p)))) enemies)))
	     (ally (if sight-mode
		       nil
		       (find-if #'(lambda (p) (and (= x (x p)) (= y (y p)))) allies)))
	     (next-paths (remove (reverse-dir next) '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal))
	     (xy (list x y)))
	(when (and (>= move cost) (>= cost 0)
		   (null enemy)
		   (or (null magic-mode)
		       (and magic-mode (find xy p-sight-coord :test #'equal))))
	  ;;   (eq (state enemy) :dead))) ;;死んでる敵の上は移動できる
	  (let ((temp-xy (find xy (temparea unit) :test #'equal :key #'car)))
	    (cond  ((and (not temp-xy))
		    ;; (eq (state ally) :dead)))
		    (when (null ally)
		      (push (list xy move) (temparea unit))) ;;残り移動力を保持する
		    (loop for v in next-paths
			  :do
			     (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
						(- move cost) movecost  enemies allies v sight-mode magic-mode)))
		   ((and temp-xy (> move (cadr temp-xy)))
		    (setf (cadr (find xy (temparea unit) :test #'equal :key #'car)) move)
		    (loop for v in next-paths
			  do
			     (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
						(- move cost) movecost  enemies allies v sight-mode magic-mode))))))))))

;;視界範囲か移動範囲をゲット
(defmethod get-area ((unit unit) range sight-mode magic-mode)
  (with-slots (enemies) *battle-field*
    (with-slots (party) *game*
      (with-slots (x y movecost temparea) unit
	(setf temparea nil)
	(when  (> range 0)
	  (loop for v in '((0 1) (0 -1) (1 0) (-1 0))
		do (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
				      range movecost enemies party nil sight-mode magic-mode))
	  (setf temparea (loop :for area :in temparea ;;探索で使用したのを消す
			       :collect (car area))))
	(push (list x y)  temparea) ;;自分自身
	temparea))))

(defmethod get-area ((unit e-unit) range sight-mode magic-mode)
  (with-slots (enemies) *battle-field*
    (with-slots (party) *game*
      (with-slots (x y movecost temparea) unit
	(setf temparea nil)
	(loop for v in '((0 1) (0 -1) (1 0) (-1 0))
	      do
		 (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
				    range movecost party enemies nil sight-mode magic-mode))
	(setf temparea (loop :for area :in temparea ;;探索で使用したのを消す
			     :collect (car area)))
	temparea))))
;;----------------------------------------------------------------------------------
;;レベルアップ
(defun unit-level-up (unit)
  )

;;----------------------------------------------------------------------------------
;; skillの効果範囲ゲット
(defun get-skill-scope (r skill)
  (with-slots (temparea) skill
    (multiple-value-bind (x y) (get-mouse-coord)
      (setf temparea nil)
      (cond
	((= r 0)
	 (push (list x y) temparea)
	 temparea)
	(t
	 (loop for v in '((0 1) (0 -1) (1 0) (-1 0))
	       do (get-can-move-cell skill (+ x (car v)) (+ y (cadr v))
				     r #(1 1 1 1 1 1 1 1 1) nil nil nil t nil))
	 (setf temparea (loop :for area :in temparea
			      :collect (car area)))
	 (push (list x y) temparea)
	 temparea)))))

;;--------------ダメージ計算------------------------------------------------

;;物理ダメージ計算
(defun %damage-calc (critical dmg-table)
  (let* ((dmg-num 0))
    (loop :for dice1 = (dice 1 6)
	  :for dice2 = (dice 1 6)
	  :when (and (= dice1 1) (= dice2 1))
	    :do (return dmg-num)
	  :do (incf dmg-num (nth (+ dice1 dice2) dmg-table))
	  :when (< (+ dice1 dice2) critical)
	    :do (return  dmg-num ))))

;;魔法ダメージダイス
(defun magic-damage-dice-calc (selected-skill selected-unit)
  (with-slots (critical dmg-table (skill-mp mp)) selected-skill
    (with-slots ((unit-mp mp) active-declare-skill) selected-unit
      (let* ((dmg-num 0)
	     (me-power-certainty (if (find :me-power-certainty active-declare-skill) t nil)))
	(loop :for dice1 = (dice 1 6)
	      :for dice2 = (dice 1 6)
	      :when (and me-power-certainty ;;魔法拡大/威力確実化が有効な場合
			 (>= 4 (+ dice1 dice2))
			 (>= unit-mp skill-mp)) ;;mpが足りない
		:do (let ((temp1 (dice 1 6))
			  (temp2 (dice 1 6)))
		      (when (> (+ temp1 temp2) (+ dice1 dice2))
			(setf dice1 temp1
			      dice2 temp2))
		      (setf me-power-certainty nil)
		      (decf unit-mp skill-mp)) ;;追加でMP消費
	      :when (and (= dice1 1) (= dice2 1))
		:do (return dmg-num)
	      :do (incf dmg-num (nth (+ dice1 dice2) dmg-table))
	      :when (< (+ dice1 dice2) critical)
		:do (return  dmg-num ))))))


;;プレイヤーユニットの攻撃と敵人型ユニットの防御
;; (defmethod physical-damage-calc ((atker unit) (defender e-unit))
;;   (with-slots (weapon str int) atker
;;     (with-slots (armor shield) defender
;;       (with-slots (dmg-table critical) weapon
;; 	(let* ((def-num 0))
;; 	  (when armor
;; 	    (incf def-num (def armor)))
;; 	  (when shield
;; 	    (incf def-num (def shield)))
;; 	  (max (- (%damage-calc critical dmg-table) def-num) 0))))))

;;プレイヤーユニットの攻撃とモンスター型ユニットの防御
(defmethod physical-damage-calc ((atker unit) (defender monster))
  (with-slots (weapon add-damage shield) atker
    (with-slots (def) defender
      (with-slots (damage critical hand category) weapon
	(let* ((shin-dmg (if (and (null shield)
				  (eq hand :1hor2h))
			    (2h-damage-by-category damage category)
			    damage))
	       (dmg-table (nth shin-dmg *default-damage-table-list*)))
	(max (- (+ (%damage-calc critical dmg-table) add-damage) def) 0))))))

;;モンスター型の攻撃とプレイヤーユニットの防御
(defmethod physical-damage-calc ((atker monster) (defender unit))
  (with-slots (atk-point) atker
    (with-slots (armor shield) defender
      (let* ((def-num 0)
	     (dice1 (dice 1 6))
	     (dice2 (dice 1 6)))
	(when armor
	  (incf def-num (def armor)))
	(when shield
	  (incf def-num (def shield)))
	(max (- (+ dice1 dice2 atk-point) def-num) 0)))))

;;----------------------------------------------------------------------------------------
;;魔法ダメージ計算
(defun magic-damage-calc ()
  (with-slots (selected-skill selected-unit) *game*
    (magic-damage-dice-calc selected-skill selected-unit)))
;;----------------------------------------------------------------------------------------
;;被ダメージ処理 TODO 死亡判定
(defun take-damage (defender dmg-num target-type)
  (with-slots (maxhp hp) defender
    (when (numberp dmg-num)
      (if (eq target-type :ally)
	  (setf hp  (min maxhp (+ hp dmg-num)))
	  (setf hp  (- hp dmg-num))))))

;;----------------------------------------------------------------------------------------
;;ダメージフォント
(defun create-damage-font (atker defender dmg-num target-type)
  (with-slots (pos) defender
    (let* ((dmg-x (+ (gk:x pos) 10))
	   (dmg-y (+ (gk:y pos) 5))
	   (x-dir (if (eq (atk-dir atker) :left) :left :right))
	   (dmg (make-instance 'dmg-font :pos (gk:vec2 dmg-x dmg-y)
			       :dmg-num  dmg-num :y-dir :up :x-dir x-dir
					 :miny dmg-y :maxy (+ dmg-y 15)
					 :font (if (numberp dmg-num)
						   *font48*
						   *font28*))))
      (if (eq target-type :ally)
	  (setf (color dmg) (gk:vec4 0 1 0.3 1))
	  (setf (color dmg) (gk:vec4 1 1 1 1)))
      dmg)))

;;精神抵抗値
(defmethod get-mnd-resist ((unit monster))
  (res-bonus unit))

(defmethod get-mnd-resist ((unit unit))
  (+ (level unit) (res-bonus unit)))

;;魔法攻撃ダメージ判定
(defun magic-damage-hit (atker defender)
  (with-slots (int-bonus magic-power active-declare-skill) atker
    (with-slots (res-bonus) defender
      (let* ((m1 (dice 1 6)) ;;命中ダイス
	     (m2 (dice 1 6))
	     (mnd-res (get-mnd-resist defender))
	     (res1 (dice 1 6)) ;;抵抗ダイス
	     (res2 (dice 1 6)))
	;;魔法拡大/確実化が有効な場合
	(when (find :me-certainty active-declare-skill)
	  (let ((temp1 (dice 1 6))
		(temp2 (dice 1 6)))
	    (when (> (+ temp1 temp2) (+ m1 m2))
	      (setf m1 temp1
		    m2 temp2))))
	(cond
	  ((and (= m1 1) (= m2 1))
	   "ミス")
	  ((> (+ magic-power m1 m2) (+ mnd-res res1 res2))
	    (+ magic-power (magic-damage-calc)))
	  ((<= (+ magic-power m1 m2) (+ mnd-res res1 res2))
	   (floor (+ magic-power (magic-damage-calc)) 2)))))))

;;魔法の回復の場合のボーナス
(defmethod get-heal-bonus ((skill skill) atker)
  (magic-power atker))
;;アイテムの回復の場合のボーナス 本来はレンジャーレベル
(defmethod get-heal-bonus ((skill use-item) atker)
  (+ (level atker) (int-bonus atker)))

;;魔法回復ダメージ判定
(defun magic-heal-hit (skill atker)
  (let* ((m1 (dice 1 6))
	 (m2 (dice 1 6))
	 (num-bonus (get-heal-bonus skill atker)))
    (cond
      ((and (= m1 1) (= m2 1))
       "ミス")
      (t
       (+ num-bonus (magic-damage-calc))))))


;;物理ダメージ命中判定 攻撃側プレイヤーユニット
(defmethod physical-damage-hit ((atker unit) (defender monster))
  (with-slots (dex-bonus hit-value weapon armor shield) atker
    (with-slots (agi-bonus avoid-value) defender
      (let* ((hit1 (dice 1 6))
	     (hit2 (dice 1 6))
	     (avoid1 (dice 1 6))
	     (avoid2 (dice 1 6))
	     (weapon-hit (if (and weapon
				  (null shield)
				  (eq (hand weapon) :1hor2h))
			     (2h-hit-by-category (hit weapon) (category weapon))
			     (hit weapon))))
	(format t "hit1:~a hit2:~a avoid1:~a avoid2:~a~%" hit1 hit2 avoid1 avoid2)
	(cond
	  ;;命中判定1ゾロはミス
	  ((and (= hit1 1) (= hit2 1))
	   "ミス")
	  ;;回避判定6ゾロはミス
	  ((= avoid1 6) (= avoid2 6)
	   "ミス")
	  ;;回避判定1ゾロは命中
	  ((and (= avoid1 1) (= avoid2 1))
	   (physical-damage-calc atker defender))
	  ;;命中判定が6ゾロで回避判定が6ゾロ以外は命中
	  ((and (= hit1 6) (= hit2 6))
	   (physical-damage-calc atker defender))
	  ;;以下命中判定と回避判定の比べあい
	  ((> (+ hit-value hit1 hit2 weapon-hit) (+ avoid-value avoid1 avoid2))
	   (physical-damage-calc atker defender))
	  ((<= (+ hit-value hit1 hit2 weapon-hit) (+ avoid-value avoid1 avoid2))
	   "ミス"))))))

;;物理ダメージ命中判定 攻撃側モンスターユニット
(defmethod physical-damage-hit ((atker monster) (defender unit))
  (with-slots (dex-bonus hit-value) atker
    (with-slots (agi-bonus avoid-value armor shield) defender
      (let* ((hit1 (dice 1 6))
	     (hit2 (dice 1 6))
	     (avoid1 (dice 1 6))
	     (avoid2 (dice 1 6))
	     (equip-avoid 0))
        (when armor
	  (incf equip-avoid (avoid armor)))
	(when shield
	  (incf equip-avoid (avoid shield)))
	(cond
	  ;;命中判定1ゾロはミス
	  ((and (= hit1 1) (= hit2 1))
	   "ミス")
	  ;;回避判定6ゾロはミス
	  ((= avoid1 6) (= avoid2 6)
	   "ミス")
	  ;;回避判定1ゾロは命中
	  ((and (= avoid1 1) (= avoid2 1))
	   (physical-damage-calc atker defender))
	  ;;命中判定が6ゾロで回避判定が6ゾロ以外は命中
	  ((and (= hit1 6) (= hit2 6))
	   (physical-damage-calc atker defender))
	  ;;以下命中判定と回避判定の比べあい
	  ((> (+ hit-value hit1 hit2) (+ avoid-value avoid1 avoid2 equip-avoid))
	   (physical-damage-calc atker defender))
	  ((<= (+ hit-value hit1 hit2) (+ avoid-value avoid1 avoid2 equip-avoid))
	   "ミス"))))))

;;ユニット死亡　消す
(defun unit-dead (unit)
  (with-slots (party) *game*
    (with-slots (weapon armor shield use-item) unit
      (when weapon
	(setf (equiped weapon) nil))
      (when armor
	(setf (equiped armor) nil))
      (when shield
	(setf (equiped shield) nil))
      (when use-item
	(dolist (i use-item)
	  (setf (equiped i) nil)))
      (setf party (remove unit party :test #'equal)))))

;;生死判定
(defun life-or-death (unit)
  (with-slots (state level vit-bonus hp translate-x) unit
    (let ((value (abs hp))
	  (dice1 (dice 1 6))
	  (dice2 (dice 1 6)))
      (cond
	((and (= dice1 1) (= dice2 1)) ;;1ゾロは死亡　消える
	 (unit-dead unit))
	((and (= dice1 6) (= dice2 6) ;;6ゾロならHP1に戻し生きる
	      (not (eq state :swoon)))
	 (setf hp 1))
	((>= (+ level vit-bonus dice1 dice2) value) ;;判定成功で気絶状態
	 (setf state :swoon translate-x (- (* +swoon+ *battle-obj-w*))))
	((< (+ level vit-bonus dice1 dice2) value) ;;判定失敗で死亡　消える
	 (unit-dead unit))))))

;;ドロップしたアイテムのオブジェクト作る
(defun create-drop-item-font (defender drop-item)
  (with-slots (dmg-font) *game*
    (with-slots (pos) defender
      (loop :for item :in drop-item
	    :for y :from 0 :to 200 :by 30
	    :do (let* ((drop-item-name (name (nth item *cash-exchange-item-list*)))
		       (name-len (length drop-item-name))
		       (posy (+ (gk:y pos) 30 y))
		       (posx (- (gk:x pos) (* name-len 4))))
		  (push (make-instance 'drop-item-font :dmg-num drop-item-name :pos (gk:vec2 posx posy)
						       :maxy (+ posy 40) :color *white* :font *font18*)
			dmg-font))))))


;;アイテムドロップ判定
(defun item-drop-proc (defender)
  (with-slots (cash-exchange-item) *game*
    (with-slots (drop) defender
      (let* ((drop-dice (dice 2 6)) ;;アイテムドロップ
	     (drop-item (funcall drop drop-dice)))
	(cond
	  ((null drop-item) nil)
	  ((listp drop-item)
	   (setf cash-exchange-item (append cash-exchange-item drop-item))
	   (create-drop-item-font defender drop-item)))))))

;;死亡判定 モンスターの場合消す
(defmethod death-verdict ((atker unit) (defender monster))
  (with-slots (enemies) *battle-field*
    (with-slots (hp level drop) defender
      (when (>= 0 hp)
	(item-drop-proc defender)
	(setf enemies (remove defender enemies :test #'equal))
	(with-slots (expe lvup-exp) atker
	  ;;経験値を増やす
	  (incf expe (* level 10)))))))

;;死亡判定 プレイヤーユニットの場合 state:deadにしておく
(defmethod death-verdict ((atker monster) (defender unit))
  (with-slots (state hp) defender
    (when (>= 0 hp)
      (life-or-death defender))))

;;ダメージ計算して表示する位置とか設定 ダメージフォントオブジェクトを帰す  TODO
(defun damage-proc (atker defender atking-type)
  (with-slots (dex-bonus) atker
    (with-slots (x y pos obj-type hp atk-spd state agi-bonus) defender
      (let ((dmg-num (cond
		       ((or (eq atking-type :short)
			    (eq atking-type :long))
			(physical-damage-hit atker defender))
		       ((eq atking-type :magic)
			(magic-damage-hit atker defender)))))
	(take-damage defender dmg-num :enemy) ;; :ally以外ならマイナスダメージ
	(death-verdict atker defender)
	(create-damage-font atker defender dmg-num :enemy)))))

;;-------------------------------------------------------------------------
;; skillの処理
(defmethod skill-proc ((skill heal) atker defender)
  (with-slots (target) skill
    (let ((dmg-num (magic-heal-hit skill atker)))
      (take-damage defender dmg-num target)
      (create-damage-font atker defender dmg-num target))))

;;魔香水
(defmethod skill-proc ((skill magic-perfume) atker defender)
  (with-slots (level int-bonus id) atker
    (with-slots (mp maxmp) defender
      (let* ((level-bonus (if (eq id :ranger) level 0))
	     (num (+ level-bonus int-bonus)))
	(setf mp (min (+ mp num) maxmp))
	(create-damage-font atker defender num :ally)))))

;;ヒーリングぽーちょん
(defmethod skill-proc ((skill healing-potion) atker defender)
  (with-slots (level int-bonus id) atker
    (with-slots (hp maxhp) defender
      (with-slots (critical power) skill
	(let* ((level-bonus (if (eq id :ranger) level 0))
	       (num (+ level-bonus int-bonus (%damage-calc critical (nth power *default-damage-table-list*)))))
	  (setf hp (min (+ hp num) maxhp))
	  (create-damage-font atker defender num :ally))))))

;;応急手当
(defmethod skill-proc ((skill first-aid) atker defender)
  (with-slots (level dex-bonus id) atker
    (with-slots (hp translate-x state) defender
      (let ((dice1 (dice 1 6))
	    (dice2 (dice 1 6))
	    (value (abs hp))
	    (level-bonus (if (eq id :ranger) level 0))
	    (string "ミス"))
	(when (eq state :swoon)
	  (cond
	    ((and (= dice1 6) (= dice2 6))
	     (setf hp 1 translate-x 0 state :inaction string "気絶"))
	    ((and (= dice1 1) (= dice2 1))
	     (life-or-death defender))
	    ((>= (+ level-bonus dex-bonus dice1 dice2) value)
	     (setf hp 1 translate-x 0 state :inaction string "気絶"))
	    (t (life-or-death defender))))
	(create-damage-font atker defender string :ally)))))

;;普通のスキル
(defmethod skill-proc ((skill skill) atker defender)
  (with-slots (power depend mp target atking-type) skill
    (let ((dmg-num (cond
		     ((or (eq atking-type :short)
			  (eq atking-type :long))
		      (physical-damage-hit atker defender))
		     ((eq atking-type :magic-atk)
		      (magic-damage-hit atker defender))
		     ((eq atking-type :magic-heal)
		      (magic-heal-hit skill atker)))))
      (take-damage defender dmg-num target)
      (death-verdict atker defender)
      (create-damage-font atker defender dmg-num target))))
;;-------------------------------------------------------------------------

;;ドロップアイテムのフォント更新
(defmethod update-font ((item drop-item-font))
  (with-slots (dmg-font) *game*
    (with-slots (pos maxy) item
      (incf (gk:y pos))
      (when (>= (gk:y pos) maxy)
	(setf dmg-font (remove item dmg-font :test #'equal))))))

;;ダメージフォントの位置更新
(defmethod update-font ((dmg dmg-font))
  (with-slots (dmg-font) *game*
    (with-slots (y-dir x-dir pos miny maxy) dmg
      (cond
	((eq :down y-dir)
	 (if (eq x-dir :right)
	     (incf (gk:x pos))
	     (decf (gk:x pos)))
	 (decf (gk:y pos) 1)
	 (when (<= (gk:y pos) miny)
	   (setf dmg-font (remove dmg dmg-font :test #'equal))))
	((eq :up y-dir)
	 (if (eq x-dir :right)
	     (incf (gk:x pos))
	     (decf (gk:x pos)))
	 (incf (gk:y pos) 1)
	 (when (>= (gk:y pos) maxy)
	   (setf y-dir :down)))))))

(defun update-fonts ()
  (with-slots (dmg-font) *game*
    (loop :for dmg :in dmg-font
	  :do (update-font dmg))))

;;----------------------------------------------------------------------------------
;;攻撃アニメ

;;攻撃アニメが終わった後の処理
(defun after-anime-end-proc ()
  (with-slots (action-state selected-unit state) *game*
    (with-slots (team expe lvup-exp) selected-unit
      (cond ((eq team :player)
	     (cond ((>= expe lvup-exp) ;;レベルアップ
		    (decf expe lvup-exp) ;;debug test
		    (incf lvup-exp 100) ;;debug test
		    (create-status-up-btn)
		    (setf state :lvup-mode))
		   (t
		    (setf action-state :player-turn
			  selected-unit nil))))
	    (t
	     (setf action-state :enemy-turn
		   selected-unit nil))))))

;;攻撃した時の移動アニメーション計算
(defun update-atk-img-pos (i)
  (cond
    ((>= 11 i)
     2)
    ((>= i 12)
     -2)))

;;攻撃移動更新
(defun update-atk-img (p i)
  (with-slots (pos) p
    (case (atk-dir p)
      (:right (incf (gk:x (pos p)) (update-atk-img-pos i)))
      (:left  (decf (gk:x (pos p)) (update-atk-img-pos i)))
      (:down  (decf (gk:y (pos p)) (update-atk-img-pos i)))
      (:up    (incf (gk:y (pos p)) (update-atk-img-pos i)))
      (:rightup
       (incf (gk:x pos) (update-atk-img-pos i))
       (incf (gk:y (pos p)) (update-atk-img-pos i)))
      (:rightdown
       (decf (gk:y (pos p)) (update-atk-img-pos i))
       (incf (gk:x (pos p)) (update-atk-img-pos i)))
      (:leftup
       (decf (gk:x (pos p)) (update-atk-img-pos i))
       (incf (gk:y (pos p)) (update-atk-img-pos i)))
      (:leftdown
       (decf (gk:x (pos p)) (update-atk-img-pos i))
       (decf (gk:y (pos p)) (update-atk-img-pos i))))))

;;攻撃アニメ終わるまでループ TODO プレイヤーのターンか敵のターンで違う
(defun update-atk-anime (atk-unit)
  (with-slots (action-state selected-unit temp-dmg dmg-font) *game*
    (with-slots (atk-frame atking-enemy temp-pos pos (unit-state state) team translate-x expe lvup-exp) atk-unit
      (when (= 7 atk-frame)
	(push temp-dmg dmg-font)
	(if (numberp (dmg-num temp-dmg))
	    (gk:play-sound :normal-attack)
	    (gk:play-sound :miss)))
      (update-atk-img atk-unit atk-frame)
      (if (and (= (gk:x temp-pos) (gk:x pos))
	       (= (gk:y temp-pos) (gk:y pos)))
	  (progn (setf atk-frame 0
		       atking-enemy nil
		       temp-pos nil
		       unit-state :end
		       temp-dmg nil
		       translate-x (- (* +action-end+ *battle-obj-w*)))
		 (after-anime-end-proc))
	  (incf atk-frame)))))
;;---------------------------------------------------------------------------------
;;攻撃方向
(defun get-atk-dir (diffx diffy)
  (cond
    ((and (>= diffx 0)
	  (= diffy 0))
     :left)
    ((and (< diffx 0)
	  (= diffy 0))
     :right)
    ((and (= diffx 0)
	  (>= diffy 0))
     :down)
    ((and (= diffx 0)
	  (< diffy 0))
     :up)
    ((and (> diffx 0)
	  (> diffy 0))
     :leftdown)
    ((and (> diffx 0)
	  (< diffy 0))
     :leftup)
    ((and (< diffx 0)
	  (> diffy 0))
     :rightdown)
    ((and (< diffx 0)
	  (< diffy 0))
     :rightup)))

;;xy座標リストとユニットから攻撃方向ゲット
(defun get-atk-dir-xy (unit last-move-pos)
  (let ((diffx (- (car last-move-pos) (x unit)))
	(diffy (- (cadr last-move-pos) (y unit))))
    (get-atk-dir diffx diffy)))

;;unit1 から unit2への攻撃方向
(defun get-atk-dir-unit (unit1 unit2)
  (let ((diffx (- (x unit1) (x unit2)))
	(diffy (- (y unit1) (y unit2))))
    (get-atk-dir diffx diffy)))




;;-------------------移動----------------------------------------------------------
;;相手ユニットがいて移動できないcellリストを返す
(defun get-enemy-cell-list (enemies)
  (mapcar #'(lambda (u) (list (x u) (y u)))
              enemies))



;;goalまでの道順ゲット
(defmethod get-move-paths ((start-unit unit) goal-cell-xy)
  (let* ((enemies (enemies *battle-field*))
	 (block-cell (get-enemy-cell-list enemies))
	 (cost) (paths)
	 (start (list (x start-unit) (y start-unit))))
    (setf (values cost paths)
	  (astar start goal-cell-xy (field-array *battle-field*) (movecost start-unit) block-cell *4-dir*))
    paths))

(defmethod get-move-paths ((start-unit e-unit) goal-cell-xy)
  (with-slots (party) *game*
    (let* ((block-cell (get-enemy-cell-list party))
	   (cost) (paths)
	   (start (list (x start-unit) (y start-unit))))
      (setf (values cost paths)
	    (astar start goal-cell-xy (field-array *battle-field*) (movecost start-unit) block-cell *4-DIR*))
      paths)))


;;pos更新
(defun update-pos (diffx diffy unit)
  (with-slots (pos) unit
    (let ((spd 6))
      (cond
	((> diffx 0)
	 (incf (gk:x pos) spd))
	((< diffx 0)
	 (decf (gk:x pos) spd)))
      (cond
	((> diffy 0)
	 (incf (gk:y pos) spd))
	((< diffy 0)
	 (decf (gk:y pos) spd))))))

;;ユニットの行動終了処理
(defun unit-action-end (unit)
  (with-slots (action-state selected-unit) *game*
    (with-slots (state selected-cmd translate-x) unit
      (setf action-state :player-turn
	    state :end
	    selected-cmd nil
            translate-x (- (* +action-end+ *battle-obj-w*))
	    selected-unit nil))))

;;プレイヤーユニットの移動終わった後の処理
(defun player-move-end-proc (unit)
  (with-slots (enemies) *battle-field*
    (with-slots (party action-state selected-unit) *game*
      (with-slots ((unit-state state) weapon temp-pos pos origin selected-cmd magics) unit
	(cond
	  ;;移動のみ
	  ((eq unit-state :fast-move)
	   (unit-action-end unit))
	  ((eq unit-state :normal-move)
	   ;;(let ((attackable (get-attack-can-reach unit)))
	   (if (or (attack-p unit)
		   magics)
	       ;;通常攻撃可能かスキルを持ってる場合
	       (progn (setf action-state :after-move)
		      (create-action-command-btn unit))
	       ;;行動終了
	       (unit-action-end unit))))))))
	  ;;移動後に攻撃
	  ;; ((eq unit-state :move-atk)
	  ;;  (setf temp-pos  (math:copy-vec2 pos)
	  ;; 	 action-state :atk-anime)))))))

;;敵の移動終わった後の処理
(defun enemy-move-end-proc (unit)
  (with-slots (action-state selected-unit) *game*
    (with-slots ((unit-state state) temp-pos pos) unit
      (cond
	((eq unit-state :move)
	 (setf action-state :enemy-turn
	       unit-state :end
	       selected-unit nil))
	((eq unit-state :move-atk)
	 (setf temp-pos  (math:copy-vec2 pos)
	       action-state :atk-anime))))))

;;移動が終わった処理
(defun move-end-proc (unit)
  (with-slots (movearea team) unit
    (setf movearea nil)
	  ;;posx2 (+ posx 32)
	  ;;posy2 (+ posy 32))
    (if (eq team :player)
	(player-move-end-proc unit)
	;;敵の場合
	(enemy-move-end-proc unit))))


;;移動アニメーション ;;TODO 移動後に攻撃あるかも
(defun move-anime ()
  (with-slots (action-state selected-unit) *game*
    (with-slots (pos x y w h move-paths) selected-unit
      (let* ((path (car move-paths))
	     (goalposx (* (car path) *battle-obj-w*))
	     (goalposy (* (cadr path) *battle-obj-h*))
	     (diffx (- goalposx (gk:x pos)))
	     (diffy (- goalposy (gk:y pos))))
	(update-pos diffx diffy selected-unit)
	(when (and  (= (gk:x pos) goalposx)
		    (= (gk:y pos) goalposy))
	  (gk:play-sound :walk)
	  (setf move-paths (cdr move-paths)
		x (floor (gk:x pos) *battle-obj-w*)
		y (floor (gk:y pos) *battle-obj-h*))
	  (get-show-cell-coord) ;;視界アップデート
	  (unless move-paths	  ;;移動終わり
	    (move-end-proc selected-unit)
	    ))))))

(defun update-unit-move ()
  (move-anime))

;;----------battle-ready-----------------------------------------------------------
(defun set-party-battle-ready-pos ()
  (with-slots (party) *game*
    (with-slots (player-init-pos) *battle-field*
      (let ((init-pos  player-init-pos))
	(loop :for p :in party
	      :do (with-slots (pos x y) p
		    (let ((cell (nth (random (length init-pos)) init-pos)))
		      (setf pos (math:copy-vec2 (pos cell)) ;;(gk:vec2 posx posy)
			    x (x cell)
			    y (y cell)
			    init-pos (remove cell init-pos :test #'equal)))))))))

;;バトル準備画面でユニット選択したとき
(defun unit-select-battle-ready ()
  (with-slots (party selected-unit) *game*
    (loop :for p :in party
          :do (with-slots (movearea move) p
                (when (collide-p *mouse* p)
                  (setf selected-unit p
                        movearea (get-area p move nil nil))
                  (return))))))

;;バトル準備画面でユニットの位置を変える
(defun change-battle-ready-pos ()
  (with-slots (selected-unit party) *game*
    (with-slots ((s-posx posx) (s-posy posy) (s-pos pos) (s-posx2 posx2) (s-posy2 posy2) (s-x x) (s-y y)) selected-unit
      (with-slots (player-init-pos) *battle-field*
	(let ((pos-hoge 0)
	      (x-hoge 0)
              (y-hoge 0))
	  (loop :for cell :in player-init-pos
		:do (with-slots (x y pos) cell
                      (when (collide-p *mouse* cell)
			(let ((preexisting-unit (find-if (lambda (p) (math:vec= (pos p) pos)) party)))
		       	  (if preexisting-unit
			      ;;指定した位置にユニットがいた場合
			      (with-slots ((pre-pos pos) (pre-x x) (pre-y y)) preexisting-unit
                                (setf pos-hoge (math:copy-vec2 pre-pos)
		       		      x-hoge pre-x
				      y-hoge pre-y
				      pre-x s-x
				      pre-y s-y
                                      pre-pos (math:copy-vec2 s-pos)
				      s-x x-hoge
				      s-y y-hoge
				      s-pos pos-hoge))
			      ;;指定した位置にユニットがいなかった場合
			      (setf s-x x
				    s-y y
				    s-pos (math:copy-vec2 pos)))
			  (setf selected-unit nil)
			  (get-show-cell-coord) ;;視界
			  (return))))))))))

;;カーソルの位置の地形
(defun get-click-cell ()
  (with-slots (field) *battle-field*
    (loop :for cell :in field
          :do (when (collide-p *mouse* cell)
                (return cell)))))

;;出撃初期配置設定
(defun battle-ready-event ()
  (with-slots (left) *mouse*
    (with-slots (selected-unit action-state state flash-flag frame) *game*
      (with-slots (player-init-pos) *battle-field*
	(when (zerop (mod frame 80))
	  (setf flash-flag (null flash-flag)))
        (cond
	  (left
	   (if selected-unit
	       (let* ((click-cell (get-click-cell))
                      (cell-xy (list (x click-cell) (y click-cell))))
		 (cond
		   ((find click-cell player-init-pos :test #'equal)
		    (change-battle-ready-pos))
		   ))
	       (unit-select-battle-ready)))
	  ((space1 *keystate*)
	   (setf state :battle
		 action-state :player-turn
		 selected-unit nil)))))))


;;-----------------battle-------------------------------------------------------------------------------


;;ターン終了ボタン押したとき
(defun select-player-turn-end ()
  (with-slots (party btn-list) *game*
    (print "nnnnndd")
    (setf btn-list nil)
    (loop :for p :in party
	  :do (setf (state p) :end
		    (atked-pos p) nil))))



;;クリックしたユニット選択ユニットにセット&コマンドボタンを出す
(defun set-selected-unit (allies)
  (with-slots (selected-unit) *game*
    (loop :for p :in allies
     	  :do (with-slots (movearea move state weapon) p
		(setf movearea nil) ;; todo
		(when (and (collide-p *mouse* p)
			   (eq state :inaction))
		  (if (eq (team p) :player) ;;プレイヤーユニットならコマンド出す
		      (create-action-command-btn p)
		      ;;敵ユニットの場合 通常移動範囲を表示 TODO
		      (unit-ready-move p  (move p)))
		  (setf selected-unit p)
		  (return t))))))


;;バトル中 味方or敵 ユニットをクリック
(defun unit-click-in-battle () ;;TODO
  (with-slots (party action-state) *game*
    (with-slots (enemies) *battle-field*
      ;;プレイヤーキャラを選択していなかった場合敵キャラを選択したか調べる
      (cond
	((set-selected-unit party)
	 (setf action-state :select-cmd-mode))
	(t
	 (set-selected-unit enemies))))))


;;クリックしたユニットに何かする
(defun atk-or-heal-to-clicked-unit (selected-unit clicked-unit targets)
  (with-slots (temp-pos atk-dir atking-enemy state weapon pos move-paths) selected-unit
    (with-slots (action-state) *game*
      (let* ((diffx (- (x selected-unit) (x clicked-unit)))
	     (diffy (- (y selected-unit) (y clicked-unit)))
	     (manhatan-dist (+ (abs diffx) (abs diffy))))
	(if (>= (rangemax weapon) manhatan-dist (rangemin weapon))
	    ;;動かずに攻撃
	    (progn (setf action-state :atk-anime
			 temp-pos (math:copy-vec2 pos)
			 atk-dir (get-atk-dir diffx diffy)
			 atking-enemy clicked-unit))
	    (progn (setf state :move-atk
			 atking-enemy clicked-unit
			 move-paths (get-move-paths selected-unit (atked-pos clicked-unit))
			 atk-dir  (get-atk-dir-xy clicked-unit (car (last move-paths)))
			 action-state :move-anime)
		   (print  (car (last move-paths)))))
	(init-unit-atked-pos targets)))))


;;味方ユニットを選択した状態で他の味方をクリック
(defun click-ally-unit-with-selected-unit (selected-unit ally-unit)
  (with-slots (weapon) selected-unit
    (with-slots (party selected-unit btn-list) *game*
      (with-slots (enemies) *battle-field*
	(cond
	  ;;クリックした味方ユニットが回復できる距離にいる
	  ;; ((and (eq (atktype weapon) :heal)
	  ;; 	(atked-pos ally-unit))
	  ;;  (atk-or-heal-to-clicked-unit selected-unit ally-unit party)
	  ;;  (init-unit-atked-pos party))
	  ((eq (state ally-unit) :inaction)
	   ;; (let ((new-targets (if (eq (atktype (weapon ally-unit)) :heal)
	   ;; 			  party
	   ;; 			  enemies)))
	   ;;   (unit-ready-move ally-unit new-targets)
	   (setf selected-unit ally-unit
		 btn-list nil)
	   (create-action-command-btn selected-unit)))))))

;;プレイヤーのユニットを選択した状態でクリックイヴェント
(defun left-click-with-player-unit-selected (selected-unit)
  (with-slots (party action-state btn-list) *game*
    (with-slots (enemies) *battle-field*
      (with-slots (weapon move-paths atk-dir pos) selected-unit
	(let* ((click-enemy (find-if #'(lambda (unit) (collide-p *mouse* unit)) enemies))
	       (click-other-ally (find-if #'(lambda (unit) (collide-p *mouse* unit)) party))
	       (select-command-btn (find-if (lambda (btn) (collide-p *mouse* btn)) btn-list)))
	  (cond
	    ;;コマンド選択
	    (select-command-btn
	     (click-command-btn select-command-btn selected-unit))
	     ;;(setf btn-list nil))
	    ;;他の味方をクリックした
	    (click-other-ally
	     (click-ally-unit-with-selected-unit selected-unit click-other-ally))
	    ;;敵をクリックした
	    (click-enemy
	     (setf btn-list nil)
	     (unit-ready-move click-enemy  (move click-enemy))
	     (setf (game/selected-unit *game*) click-enemy
		   action-state :player-turn))
	    ))))))


;;バトル中のクリックイベント TODO test temp-pos
(defun left-click-event-in-battle ()
  (with-slots (selected-unit) *game*
    (cond
      ;;プレイヤーキャラを選択している状態
      ;; ((and selected-unit
      ;;       (eq (team selected-unit) :player))
      ;;  (left-click-with-player-unit-selected selected-unit))
      (t
       (unit-click-in-battle)))))

;;右クリック イベント
(defun right-click-event-in-battle ()
  (with-slots (selected-unit btn-list) *game*
    (cond
      (selected-unit
       (setf selected-unit nil
	     btn-list nil)))))

;;-------------------------------------------------------------------------------------
;;コマンド選択モード

(defun select-cmd-mode-left-click-event ()
  (with-slots (selected-unit) *game*
    (left-click-with-player-unit-selected selected-unit)))

(defun select-cmd-mode-right-click-event ()
  (with-slots (action-state selected-unit btn-list) *game*
    (cond
      (selected-unit
       (setf selected-unit nil
	     action-state :player-turn
	     btn-list nil)))))

(defun select-cmd-mode-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-cmd-mode-left-click-event))
      (right (select-cmd-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;; select move mode
;;クリック
(defun select-move-mode-left-click-event ()
  (with-slots (btn-list selected-skill selected-unit action-state) *game*
    (loop :for btn :in btn-list
	  :do (when (collide-p *mouse* btn)
	        (click-command-btn btn selected-unit)))))

;;左クリック
(defun select-move-mode-right-click-event ()
  (with-slots (btn-list action-state selected-unit) *game*
    (setf action-state :select-cmd-mode
	  btn-list nil)
    (create-action-command-btn selected-unit)))


(defun select-move-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-move-mode-left-click-event))
      (right (select-move-mode-right-click-event)))))

;;------------------------------------------------------------------------------------
;; 移動モード クリックイベント
(defun move-mode-left-click-event ()
  (with-slots (selected-unit action-state) *game*
    (with-slots (movearea move-paths state selected-cmd) selected-unit
      (let* ((click-cell (get-click-cell))
	     (cell-xy (list (x click-cell) (y click-cell))))
	(when (find cell-xy movearea :test #'equal)
	  (setf move-paths (get-move-paths selected-unit cell-xy)
		state selected-cmd ;;todo?
		action-state :move-anime)
	  (init-unit-atked-pos (enemies *battle-field*)))))))

;;移動モード  右クリックイベント
(defun move-mode-right-click-event ()
  (with-slots (selected-unit action-state) *game*
    (setf action-state :select-move-mode
	  (movearea selected-unit) nil)
    (create-all-move-btn selected-unit)))

;;移動先選択中
(defun move-mode-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (move-mode-left-click-event))
      (right (move-mode-right-click-event)))))

;;------------------------------------------------------------------------------------
;;  after move
;;コマンド選ぶ状態
(defun after-move-left-click-event ()
  (with-slots (btn-list selected-unit) *game*
    (loop :for btn :in btn-list
	  :do (when (collide-p *mouse* btn)
		(click-command-btn btn selected-unit)))))

;;右クリック
(defun after-move-right-click-event ()
  (with-slots (btn-list selected-unit) *game*
    (unit-action-end selected-unit)
    (setf btn-list nil)))

;; 移動後のイベント
(defun after-move-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (after-move-left-click-event))
      (right (after-move-right-click-event)))))
;;------------------------------------------------------------------------------------
;; attack mode
 ;;移動後に攻撃できる敵がいる場合のクリックイベント
(defun attack-mode-left-click-event ()
  (with-slots (selected-unit action-state temp-dmg) *game*
    (with-slots (pos atking-enemy weapon origin) selected-unit
      (with-slots (enemies) *battle-field*
	(loop :for e :in enemies
	      :do (with-slots (atked-pos) e
		    (when (and (collide-p *mouse* e)
			       atked-pos)
		      (let ((diffx (- (x selected-unit) (x e)))
			    (diffy (- (y selected-unit) (y e))))
			(print "nnnnn")
			(setf (temp-pos selected-unit) (math:copy-vec2 pos)
			      (atk-dir selected-unit) (get-atk-dir diffx diffy)
			      action-state :atk-anime
			      atking-enemy e
			      temp-dmg (damage-proc selected-unit e (atktype weapon)))
			(init-unit-atked-pos enemies)
			(return)))))))))

;;右クリック
(defun attack-mode-right-click-event ()
  (with-slots (selected-unit action-state) *game*
    (with-slots (state) selected-unit
      (print "ng")
      (init-unit-atked-pos (enemies *battle-field*))
      (cond
	((eq state :inaction)
	 (setf action-state :select-cmd-mode))
	((eq state :normal-move)
	 (setf action-state :after-move)))
      (create-action-command-btn selected-unit))))


(defun attack-mode-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (attack-mode-left-click-event))
      (right (attack-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;; select item mode
;;クリック
(defun select-item-mode-left-click-event ()
  (with-slots (btn-list selected-skill selected-unit action-state) *game*
    (loop :for btn :in btn-list
	  :do (when (collide-p *mouse* btn)
		(let ((item (item btn)))
		  (with-slots (range r rangemax) item
		    (setf action-state :skill-mode
			  range (get-area selected-unit rangemax t t)
			  selected-skill item))
		  (setf btn-list nil)
		  (return))))))

;;左クリック
(defun select-item-mode-right-click-event ()
  (with-slots (btn-list action-state selected-unit) *game*
    (setf action-state :select-cmd-mode
	  btn-list nil)
    (create-action-command-btn selected-unit)))


(defun select-item-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-item-mode-left-click-event))
      (right (select-item-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------
;; select declare skill mode
;;クリック
(defun select-declare-skill-mode-left-click-event ()
  (with-slots (btn-list selected-unit action-state) *game*
    (with-slots (active-declare-skill) selected-unit
      (loop :for btn :in btn-list
	    :do (when (collide-p *mouse* btn)
		  (with-slots (tag) btn
		    (if (find tag active-declare-skill)
			(setf active-declare-skill (remove tag active-declare-skill))
			(push tag active-declare-skill))
		    (setf btn-list nil)
		    (create-declare-skill-btn selected-unit)
		    (return)))))))

;;左クリック
(defun select-declare-skill-mode-right-click-event ()
  (with-slots (btn-list action-state selected-unit) *game*
    (setf action-state :select-cmd-mode
	  btn-list nil)
    (create-action-command-btn selected-unit)))


(defun select-declare-skill-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-declare-skill-mode-left-click-event))
      (right (select-declare-skill-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;; select skill mode
;;クリック
(defun select-skill-mode-left-click-event ()
  (with-slots (btn-list selected-skill selected-unit action-state) *game*
    (loop :for btn :in btn-list
	  :do (when (collide-p *mouse* btn)
		(with-slots (tag) btn
		  (let ((skill (getf *all-magic-list* tag)))
		    (with-slots (range r rangemax) skill
		      (setf action-state :skill-mode
			    range (get-area selected-unit rangemax t t)
			    selected-skill skill))
		    (setf btn-list nil)
		    (return)))))))

;;左クリック
(defun select-skill-mode-right-click-event ()
  (with-slots (btn-list action-state selected-unit) *game*
    (setf action-state :select-cmd-mode
	  btn-list nil)
    (create-action-command-btn selected-unit)))


(defun select-skill-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-skill-mode-left-click-event))
      (right (select-skill-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;;スキル別のターゲット条件
;;特別条件なければt
(defmethod get-skill-target ((skill skill) target-unit)
  t)
;;ヒール
(defmethod get-skill-target ((skill heal) target-unit)
  (not (eq (state target-unit) :swoon)))
;;応急手当
(defmethod get-skill-target ((skill first-aid) target-unit)
  (eq (state target-unit) :swoon))

;;------------------------------------------------------------------------------------
(defmethod used-item? ((item use-item))
  t)

(defmethod used-item? ((skill skill))
  nil)

;;ｍｐ消費の計算
(defun mp-consumption-calc ()
  (with-slots (selected-skill selected-unit expand-magic-dist expand-magic-area skill-target-units) *game*
    (with-slots (active-declare-skill (unit-mp mp)) selected-unit
      (with-slots ((skill-mp mp) target) selected-skill
	(let ((num 1))
	  (when (and (find :me-number active-declare-skill)
		     (eq target :one)) ;;todo
	    (setf num (* (length skill-target-units) num)))
	  (when (find :me-certainty active-declare-skill)
	    (setf num (* num 2)))
	  (when (find :me-distance active-declare-skill)
	    (setf num (* num expand-magic-dist)))
	  (when (find :me-area active-declare-skill)
	    (setf num (* num (1+ expand-magic-area))))
	  (* num skill-mp))))))

;;ターゲット決定
(defun skill-target-determination (target-units)
  (with-slots (selected-skill action-state selected-unit temp-dmg item) *game*
    (with-slots (active-declare-skill mp) selected-unit
      (with-slots (pos team) selected-skill
	(multiple-value-bind (mouse-x mouse-y) (get-mouse-coord)
	  ;;MP消費
	  (decf mp (mp-consumption-calc))
	  ;;ダメージフォントオブジェクト
	  (loop :for target-enemy :in target-units
		:do (push (skill-proc selected-skill selected-unit target-enemy) temp-dmg))
	  (setf action-state :skill-anime
		pos (gk:vec2 (* mouse-x *battle-obj-w*) (* mouse-y *battle-obj-h*))
		team :player)
	  (when (used-item? selected-skill) ;;もしアイテムを使用したら
	    (with-slots (use-item) selected-unit
	      (setf use-item (remove selected-skill use-item :test #'equal)
		    item (remove selected-skill item :test #'equal)))))))))

;;  skill mode スキルでターゲットを選択する場面
;;クリック
(defun skill-mode-left-click-event ()
  (with-slots (selected-skill party action-state selected-unit temp-dmg item skill-target-units) *game*
    (with-slots (active-declare-skill (unit-mp mp)) selected-unit
      (with-slots (range target pos team scope atking-type tag (skill-mp mp)) selected-skill
	(multiple-value-bind (mouse-x mouse-y) (get-mouse-coord)
	  (let* ((targets (append  party (enemies *battle-field*)))
		 (mouse-xy (list mouse-x mouse-y))
		 (target-units nil)
		 (within-range? (find mouse-xy range :test #'equal)))
	    (loop :for xy :in scope
		  :do (let* ((x (car xy))
			     (y (cadr xy))
			     (tar (find-if #'(lambda (unit) (and (= x (x unit)) (= y (y unit))
								 (get-skill-target selected-skill unit))) targets)))
			(when tar
			  (push tar target-units))))
	    (cond
	      ;;魔法拡大/数が有効になっていて選択したユニットが既に選択されていた場合選択解除
	      ((and (find :me-number active-declare-skill)
		    (eq target :one) ;;todo
		    target-units
		    (find (Car target-units) skill-target-units :test #'equal))
	       (setf skill-target-units (remove (car target-units) skill-target-units :test #'equal))
	       (print skill-target-units))
	      ;;魔法拡大/数が有効になっている場合
	      ((and (find :me-number active-declare-skill)
		    (eq target :one) ;;todo
		    target-units
		    within-range?
		    (>= unit-mp (+ (mp-consumption-calc) (* skill-mp 2))))
	       (setf skill-target-units (append skill-target-units target-units))
	       (print skill-target-units))
	      ;;魔法拡大/数が有効になっていて MPが足りない
	      ((and (find :me-number active-declare-skill)
		    (eq target :one) ;;todo
		    target-units
		    within-range?
		    (< unit-mp (+ (mp-consumption-calc) (* skill-mp 2))))
	       nil)
	      ((and (>= (mp selected-unit) skill-mp)
		    target-units
		    within-range?)
	       (setf skill-target-units (append skill-target-units target-units))
	       (skill-target-determination target-units)
	       ))))))))


;;左クリック
(defun skill-mode-right-click-event ()
  (with-slots (selected-unit selected-skill action-state btn-list skill-target-units) *game*
    (with-slots (use-item magics active-declare-skill) selected-unit
      (setf btn-list nil)
      (cond
	;;魔法拡大/数が有効な場合、右クリックを魔法発動とする
	((and (find :me-number active-declare-skill)
	      skill-target-units)
	 (skill-target-determination skill-target-units))
	((used-item? selected-skill)
	 (setf action-state :select-item-mode)
	 (create-item-cmd-btn selected-unit use-item)
	 (setf selected-skill nil))
	(t
	 (create-magic-btn selected-unit magics)
	 (setf action-state :select-skill-mode)
	 (setf selected-skill nil))))))

;;スキルの効果範囲を更新
(defun update-skill-scope ()
  (with-slots (selected-skill expand-magic-area selected-unit) *game*
    (with-slots (e r) *keystate*
      (with-slots (active-declare-skill (unit-mp mp)) selected-unit
	(with-slots (scope range (skill-r r) target (skill-mp mp)) selected-skill
	  (multiple-value-bind (mouse-x mouse-y) (get-mouse-coord)
	    (let* ((mouse-xy (list mouse-x mouse-y)))
	      (cond
		;;エリア拡大
		((and e (eq target :area) (find :me-area active-declare-skill)
		      (>= unit-mp (+ (mp-consumption-calc) (* skill-mp 2))))
		 (incf expand-magic-area))
		;;エリア縮小
		((and r (> expand-magic-area 0) (eq target :area) (find :me-area active-declare-skill))
		 (decf expand-magic-area)))
	      (when (find mouse-xy range :test #'equal)
		(setf scope (get-skill-scope (+ skill-r expand-magic-area) selected-skill))))))))))

;;スキルの射程距離を伸ばす
(defun extend-skill-range ()
  (with-slots (selected-skill expand-magic-dist selected-unit) *game*
    (with-slots (active-declare-skill (unit-mp mp)) selected-unit
      (with-slots (scope range (skill-r r) target (skill-mp mp) rangemax) selected-skill
	(when (and (> rangemax 1)
		   (find :me-distance active-declare-skill)
		   (>= unit-mp (+ (mp-consumption-calc) (* skill-mp 2))))
	  (incf expand-magic-dist)
	  (setf range (get-area selected-unit (* rangemax  expand-magic-dist) t t)))))))

;;スキルの射程距離を短くする
(defun shorten-skill-range ()
  (with-slots (selected-skill expand-magic-dist selected-unit) *game*
    (with-slots (active-declare-skill (unit-mp mp)) selected-unit
      (with-slots (scope range (skill-r r) target (skill-mp mp) rangemax) selected-skill
	(when (and (> rangemax 1)
		   (find :me-distance active-declare-skill)
		   (> expand-magic-dist 1))
	  (decf expand-magic-dist)
	  (setf range (get-area selected-unit (* rangemax  expand-magic-dist) t t)))))))

	
(defun skill-mode-event ()
  (with-slots (left right) *mouse*
    (with-slots (f g) *keystate*
      (update-skill-scope)
      (cond
	(left (skill-mode-left-click-event))
	(right (skill-mode-right-click-event))
	(f (extend-skill-range))
	(g (shorten-skill-range))
	))))

;;------------------------------------------------------------------------------------
(defmethod play-skill-sound ((skill heal))
  (gk:play-sound :heal))
(defmethod play-skill-sound ((skill fire))
  (gk:play-sound :fire))
(defmethod play-skill-sound ((skill first-aid))
  (gk:play-sound :heal))
;; skill anime
(defun update-skill-anime ()
  (with-slots (selected-skill action-state selected-unit temp-dmg dmg-font skill-target-units) *game*
    (with-slots (interval frame max-frame translate-x pos img sound) selected-skill
      (incf frame)
      (when (= frame 7)
	(setf dmg-font (copy-list temp-dmg))
	(gk:play-sound sound))
      (when (zerop (mod frame interval))
	(setf translate-x (- *battle-obj-w* translate-x))
	(when (>= frame max-frame)
	  (setf frame 0
		selected-skill nil
		(state selected-unit) :end
		(selected-cmd selected-unit) nil
		(translate-x selected-unit) (- (* +action-end+ *battle-obj-w*))
		temp-dmg nil
		skill-target-units nil)
	  (after-anime-end-proc))))))

;;------------------------------------------------------------------------------------
;;装備メニュー画面
(defmethod remove-equip-item ((item weapondesc))
  )
;;鎧外す
(defmethod remove-equip-item ((item armordesc))
  (with-slots (selected-unit) *game*
    (with-slots (armor) selected-unit
      (setf (equiped armor) nil
	    armor nil))))
;;盾外す
(defmethod remove-equip-item ((item shielddesc))
  (with-slots (selected-unit) *game*
    (with-slots (shield) selected-unit
      (setf (equiped shield) nil
	    shield nil))))
;;アイテム外す
(defmethod remove-equip-item ((item use-item))
  (with-slots (selected-unit) *game*
    (with-slots (use-item) selected-unit
      (setf (equiped item) nil
	    use-item (remove item use-item :test #'equal)))))

;;装備を外す
(defun remove-equip (btn)
  (with-slots (item equiped-unit) btn
    (with-slots (selected-unit) *game*
      (with-slots (weapon armor shield name) selected-unit
	(when (equal equiped-unit name)
	  (remove-equip-item item)
	  (create-item-btn)
	  (create-next-prev-btn))))))

;;装備変更画面でのイベント
(Defun equip-menu-event ()
  (with-slots (c) *keystate*
    (with-slots (left right) *mouse*
      (with-slots (state btn-list) *game*
	(cond
	  (c (setf state :battle
		   btn-list nil))
	  (left
	   (loop :for btn :in btn-list
		 :do (when (collide-p *mouse* btn)
		       (btn-click-event btn))))
	  (right
	   (loop :for btn :in btn-list
		 :do (when (collide-p *mouse* btn)
		       (remove-equip btn)))))))))

;;-----------------------------------------------------------------------
;;バトルエベント
(defun battle-event ()
  (with-slots (space1 c) *keystate*
    (with-slots (left right) *mouse*
      (with-slots (selected-unit) *game*
	(cond
	  ((and c selected-unit)
	   (open-equip-menu))
	  (space1
	   (select-player-turn-end))
	  (right
	   (right-click-event-in-battle))
	  (left
	   (left-click-event-in-battle)))))))

;;ターンエンド処理
(defun turn-end? ()
  (with-slots (action-state party state dmg-font) *game*
    (with-slots (enemies) *battle-field*
      (cond
	;;敵全滅　ワールドマップへ
	((null enemies)
	 (setf state :world-map
	       dmg-font nil)
	 (dolist (p party)
	   (with-slots (hp translate-x state) p
	     (when (eq state :swoon) ;;気絶してたらHP1で復活
	       (setf hp 1))
	     (setf state :inaction
		   translate-x 0))))
	;;プレイヤーターンから敵ターンへ
	((and (eq action-state :player-turn)
	      (every #'(lambda (unit) (or (eq (state unit) :end)
					  (eq (state unit) :swoon))) party))
	 (setf action-state :enemy-turn)
	 (dolist (p party)
	   (with-slots (translate-x state) p
	     (unless (eq state :swoon) ;;気絶してるモノ以外行動可に
	       (setf state :inaction
		     translate-x 0)))))
	;;敵ターンからプレイヤーターンへ
	((and (eq action-state :enemy-turn)
	      (every #'(lambda (unit) (eq (state unit) :end)) (enemies *battle-field*)))
	 (setf action-state :player-turn)
	 (dolist (e (enemies *battle-field*))
	   (with-slots (state translate-x) e
	     (setf state :inaction
		   translate-x 0))))))))






;;------------------------------敵の行動-------------------------------------------------------
;;---------------------------------------------------------------------------------------------
;;一番近いプレイヤーキャラ見つけ、攻撃か移動のみか判定
(defun %get-nearest-target-and-self-action (x y rangemin rangemax targets)
  (let ((nearest nil)
	(a 1000))
    (loop :for target :in targets
	  :do  (let ((manh (manhatan-x-y x (x target) y (y target))))
		 (when (>= a manh)
		   (setf a manh
			 nearest target))))
    (if (>= rangemax a rangemin)
	(values :atk nearest)
	(values :move nearest))))

;;モンスターの場合
(defmethod get-nearest-target-and-self-action ((monster monster) targets)
  (with-slots (x y rangemin rangemax) monster
    (%get-nearest-target-and-self-action x y rangemin rangemax targets)))

;;人型の場合
(defmethod get-nearest-target-and-self-action ((unit e-unit) targets)
  (with-slots (x y weapon) unit
    (%get-nearest-target-and-self-action x y (rangemin weapon) (rangemax weapon) targets)))
;;---------------------------------------------------------------------------------------------

;;移動可能範囲からターゲットに一番近い場所を選ぶ
(defun %get-goal-from-movearea (self target rangemin rangemax)
  (with-slots (movearea weapon state) self
    (with-slots (enemies) *battle-field*
      (with-slots (x y) target
	(let ((goal nil)
	      (a 1000))
	  (loop :for area :in movearea
		:do (let ((manh (manhatan-x-y (car area) x (cadr area) y)))
		      (when (and (>= a manh)
				 (>= manh rangemin)
				 (not (find-if #'(lambda (unit) (and (= (car area) (x unit))
								     (= (cadr area) (y unit)))) enemies)))
			(setf a manh
			      goal area))))
	  (if (>= rangemax a rangemin)
	      (setf state :move-atk)
	      (setf state :move))
	  goal)))))

;;モンスターの場合
(defmethod get-goal-from-movearea ((monster monster) target)
  (with-slots (rangemax rangemin) monster
    (%get-goal-from-movearea monster target rangemin rangemax)))

;;人型の場合
(defmethod get-goal-from-movearea ((unit e-unit) target)
  (with-slots (weapon) unit
    (%get-goal-from-movearea unit target (rangemin weapon) (rangemax weapon))))
;;---------------------------------------------------------------------------------------------
;;モンスター型の攻撃タイプ
(defmethod get-enemy-atking-type ((monster monster))
  (atking-type monster))
;;人型の攻撃タイプ
(defmethod get-enemy-atking-type ((unit e-unit))
  (with-slots (weapon) unit
    (atking-type weapon)))
;;---------------------------------------------------------------------------------------------


;;未行動の敵を順番に選ぶ
(defun select-action-enemy ()
  (with-slots (selected-unit) *game*
    (let ((action-enemy (find-if #'(lambda (unit) (eq (state unit) :inaction)) (enemies *battle-field*))))
      (if action-enemy
	  (setf selected-unit action-enemy)
	  (setf selected-unit nil)))))

;;移動だけか攻撃かその場で攻撃か
(Defun select-move-or-attack ()
  (with-slots (selected-unit action-state party temp-dmg) *game*
    (with-slots (move-paths atk-dir temp-pos pos atking-enemy state translate-x) selected-unit
      ;;TODO target選び 近い敵 HPの低い敵 etc
      (multiple-value-bind (action target) (get-nearest-target-and-self-action selected-unit party)
	(case action
	  (:move
	   (let ((goal (get-goal-from-movearea selected-unit target)))
	     (if goal
		 (progn (setf move-paths (get-move-paths selected-unit goal)
			      action-state :move-anime)
			(when (eq state :move-atk)
			  (setf atk-dir (get-atk-dir-xy target (car (last move-paths)))
				temp-dmg (damage-proc selected-unit target (get-enemy-atking-type selected-unit))
				atking-enemy target)))
		 (setf state :end
                       translate-x (- (* +action-end+ *battle-obj-w*))))))
	  (:atk
	   (setf action-state :atk-anime
		 temp-pos (math:copy-vec2 pos)
		 temp-dmg (damage-proc selected-unit target (get-enemy-atking-type selected-unit))
		 atk-dir (get-atk-dir-unit selected-unit target)
		 atking-enemy target)))
	))))

;;TODO 敵の行動
(defun get-enemy-move-area ()
  (with-slots (selected-unit action-state party) *game*
    (with-slots (movearea move state) selected-unit
      (when selected-unit
	(setf movearea (get-area selected-unit move nil nil))
	(if  movearea
	     (select-move-or-attack)
	     (setf state :end))))))

;; TODO　敵もスキル使いたい
(Defun update-enemies-action ()
  (select-action-enemy)
  (get-enemy-move-area))

;;--------------------------------------------------------------------------------------

(defun title-click-event ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
     	  :do (when (collide-p *mouse* btn)
         	(btn-click-event btn)))))

(defun title-event ()
  (with-slots (left) *mouse*
    (when left
      (title-click-event))))



(defun field-test ()
  (with-slots (left) *mouse*
    (when left
      (set-battle-field )
      (set-party-battle-ready-pos))))

;;----------------------------------------------------------------------------------------------------
;;マップスクロール
(defun world-map-scroll ()
  (with-slots (w a s d) *keystate*
    (with-slots (scroll world-pos) *game*
      (let* ((speed 6)
	    (w-max (- 3200 speed)))
	(cond
	  (d (when (>= w-max (+ (gk:x scroll) *window-w*))
	       (incf (gk:x scroll) speed)))
	  (a (when (>= (gk:x scroll) speed)
	       (decf (gk:x scroll) speed))))
	(cond
	  (w (when (>= w-max (+ (gk:y scroll) *window-h*))
	       (incf (gk:y scroll) speed)))
	  (s (when (>= (gk:y scroll) speed)
	       (decf (gk:y scroll) speed)))))
	;;(setf world-pos (gk:subt world-pos scroll))
	)))

;;マップデータ作成
(defun set-world-map-data ()
  (with-slots (x y) *mouse*
    (with-slots (key1 key2 key3 key4 key5 key6 key7 key8 key9 key0 c z v b) *keystate*
      (with-slots (scroll) *game*
	(let ((arr-x (floor (+ x (gk:x scroll)) 32))
	      (arr-y (floor (+ y (gk:y scroll)) 32)))
	  ;;(format t  "x:~d y:~d~%" arr-x arr-y)
	  ;;(print key1)
	  (world-map-scroll)
	  (cond
	    (key0 (setf (aref *world-map-data* arr-y arr-x) 0))
	    (key1 (setf (aref *world-map-data* arr-y arr-x) 1))
	    (key2 (setf (aref *world-map-data* arr-y arr-x) 2))
	    (key3 (setf (aref *world-map-data* arr-y arr-x) 3))
	    (key4 (setf (aref *world-map-data* arr-y arr-x) 4))
	    (key5 (setf (aref *world-map-data* arr-y arr-x) 5))
	    (key6 (setf (aref *world-map-data* arr-y arr-x) 6))
	    (key7 (setf (aref *world-map-data* arr-y arr-x) 7))
	    (key8 (setf (aref *world-map-data* arr-y arr-x) 8))
	    (key9 (setf (aref *world-map-data* arr-y arr-x) 9))
	    (c    (setf (aref *world-map-data* arr-y arr-x) 'c))
	    (v    (setf (aref *world-map-data* arr-y arr-x) 'v))
	    (b    (setf (aref *world-map-data* arr-y arr-x) 'b))
	    (z    (setf (aref *world-map-data* arr-y arr-x) 'z)))
	  )))))

;;----------------------------------------------------------------------------------------------------
;;ワールドマップ上のイベント
(defun world-map-event ()
  (with-slots (scroll move-goal move-paths world-pos frame flash-flag) *game*
    (with-slots (x y left) *mouse*
      (when (zerop (mod frame 80))
	  (setf flash-flag (null flash-flag)))
      (when left
	;;(setf move-goal (gk:vec2 (+ x (gk:x scroll)) (+ y (gk:y scroll))))
	(let* ((arr-x (floor (+ x (gk:x scroll)) 32))
	       (arr-y (floor (+ y (gk:y scroll)) 32)))
	  (when (not (eq (aref *world-map-data* arr-y arr-x) 0)) ;;0:海
	    (let* ((goal (list arr-x arr-y))
		   (startx (floor (gk:x world-pos) 32))
		   (starty (floor (gk:y world-pos) 32))
		   (start (list startx starty))
		   (path ) (value ))
	      (setf (values value path) (world-astar start goal *world-map-data* #(-1 1 1 1 1 1 1 1) nil *8-DIR* 100 100))
	      (when (listp path)
		(print path)
		(setf move-paths path))
	      (setf move-goal (gk:vec2 (+ x (gk:x scroll)) (+ y (gk:y scroll))))
	      (print move-goal)
	      (print world-pos)
	      )))))))



;;ワールド上の移動スピード
(Defun get-world-move-speed (pos)
  (let ((cell (get-world-cell pos)))
    (case cell
      (0 0.2)
      (1 2)
      (2 1)
      (3 1)
      (4 1)
      ((5 6 7 8 9 z c v b) 2))))

;;ワールドユニット移動
(defun update-world-unit-pos ()
  (with-slots (world-pos move-goal move scroll) *game*
    (when (and move-goal
	       ;;(not (math:vec= move-goal world-pos))
	       )
      (let ((speed (get-world-move-speed world-pos)))
	(cond
	  ((> (gk:x world-pos) (gk:x move-goal))
	   (decf (gk:x world-pos) speed))
	  ((< (gk:x world-pos) (gk:x move-goal))
	   (incf (gk:x world-pos) speed)))
	(cond
	  ((> (gk:y world-pos) (gk:y move-goal))
	   (decf (gk:y world-pos) speed))
	  ((< (gk:y world-pos) (gk:y move-goal))
	   (incf (gk:y world-pos) speed)))))))

;;TODO ワールドマップ上の移動
(defun update-world-move-path ()
  (with-slots (move-paths world-pos move-goal) *game*
    (let* ((path (car move-paths))
	   (goalposx (+ 16 (* (car path) *origin-obj-w*)))
	   (goalposy (+ 16 (* (cadr path) *origin-obj-h*)))
	   (diffx (- goalposx (gk:x world-pos)))
	   (diffy (- goalposy (gk:y world-pos)))
	   (speed (get-world-move-speed world-pos)))
      (cond
	((> diffx 0)
	 (incf (gk:x world-pos) speed))
	((< diffx 0)
	 (decf (gk:x world-pos) speed)))
      (cond
	((> diffy 0)
	 (incf (gk:y world-pos) speed))
	((< diffy 0)
	 (decf (gk:y world-pos) speed)))
;;      (format t "posy ~d goaly ~d~%" (gk:y world-pos) goalposy)
      (when (and  (>= speed (abs (- (gk:x world-pos) goalposx)))
		  (>= speed (abs (- (gk:y world-pos) goalposy))))
	(setf move-paths (cdr move-paths)))
      (when (and (>= 32 (abs (- (gk:x world-pos) (gk:x move-goal))))
		 (>= 32 (abs (- (gk:y world-pos) (gk:y move-goal)))))
	(setf move-paths nil)
	(print 2)
	))))

;;モンスタ－シンボルの移動
(defun update-monster-symbol ()
  (with-slots (monster-symbol frame) *game*
    (loop :for monster :in monster-symbol
	  :do (with-slots (pos v change-dir-timing alive-time) monster
		(let ((spd (get-world-move-speed pos)))
		  (setf pos (gk:add pos (gk:mult v (gk:vec2 spd spd))))
		  (decf alive-time)
		  (when (zerop (mod frame change-dir-timing))
		    (setf (gk:x v) (- (random 3) 1)
			  (gk:y v) (- (random 3) 1)))
		  (when (or (<= alive-time 0)
			    (>= (gk:x pos) 3200)
			    (< (gk:x pos) 0)
			    (>= (gk:y pos) 3200)
			    (< (gk:y pos) 0)
			    (eq 0 (get-world-cell pos)))
		    (setf monster-symbol (remove monster monster-symbol :test #'equal))))))))

;;モンスターシンボル生成 5体
(defun create-monster-symbol ()
  (with-slots (monster-symbol) *game*
    (loop :repeat 5
	  :do
	     (let* ((start-x 0)
		    (start-y 0))
	       (loop :while (eq (aref *world-map-data* start-y start-x) 0)
		     :do (setf start-x (random-minmax 3 96)  start-y (random-minmax 3 96)))
	       (push  (make-instance 'monster-symbol :x start-x :y start-y :pos (gk:vec2 (* start-x 32) (* start-y 32))
						     :img-id :monster-img :v (gk:vec2 (1- (random 3)) (1- (random 3)))
						     :change-dir-timing (random-minmax 70 130)
						     :alive-time (random-minmax 300 500)
						     :origin (gk:vec2 0 (* (random +img-monster-max+) 32)))
		      monster-symbol)))))

;;ワールドマップでのプレイヤーとモンスターシンボルの当たり判定
(defun collide-player-monster-in-world ()
  (with-slots (world-pos monster-symbol) *game*
    (loop :for monster :in monster-symbol
	  :do (with-slots (pos) monster
		(let* ((r 16)
		       (p-px (gk:x world-pos))
		       (p-py (gk:y world-pos))
		       (m-px (gk:x pos))
		       (m-py (gk:y pos)))
		  (when (>= (+ r r)  (sqrt (+ (expt (abs (- p-px m-px)) 2) (expt (abs (- p-py m-py)) 2))))
		    (setf monster-symbol (remove monster monster-symbol :test #'equal))
		    (return t)))))))

;;バトルモードへ行く準備
(defun go-battle-mode ()
  (with-slots (state action-state btn-list) *game*
    (setf state :battle-ready
	  action-state :player-turn ;;todo 先制判定
	  btn-list nil)
    (set-battle-field)
    (set-party-battle-ready-pos)
    (get-show-cell-coord)))

;;ワールドマップ更新
(defun world-map-update ()
  (with-slots (world-pos move-goal move scroll move-paths state frame selected-town action-state) *game*
    (world-map-scroll)
    (let ((temp-pos (math:copy-vec2 world-pos)))
      (when (zerop (mod frame 100))
	(create-monster-symbol))
      (update-monster-symbol)
      (cond
	(move-paths
	 (update-world-move-path))
	(move-goal
	 (update-world-unit-pos)
	 (when move-goal
	   (let ((diff-pos (gk:subt move-goal world-pos)))
	     (when (and (>= move (gk:x diff-pos) (- move))
			(>= move (gk:y diff-pos) (- move)))
	       (setf move-goal nil))))))
      (let ((cell (get-world-cell world-pos)))
	(case cell
	  ((5 6 7 8 9 z c v b) ;; +town+
	   (let* ((diff-posx (- (gk:x world-pos) (gk:x temp-pos)))
		  (diff-posy (- (gk:y world-pos) (gk:y temp-pos))))
	     (cond
	       ((> diff-posx 0)
		(decf (gk:x world-pos) 16))
	       ((< diff-posx 0)
		(incf (gk:x world-pos) 16)))
	     (cond
	       ((> diff-posy 0)
		(decf (gk:y world-pos) 16))
	       ((< diff-posy 0)
		(incf (gk:y world-pos) 16)))
	     (setf state :town
		   move-goal nil
		   move-paths nil
		   action-state :town-menu
		   selected-town (getf *town-list* cell))
	     (create-town-menu-button)))
	  (t
	   (when (collide-player-monster-in-world)
	     (gk:play-sound :encount)
	     (go-battle-mode))))))
    ))
;;----------------------------------------------------------------------------------------------------
;;町イベント
(defun town-event ()
  (with-slots (btn-list) *game*
    (with-slots (left) *mouse*
      (when left
	(loop :for btn :in btn-list
	      :do
		 (when (collide-p *mouse* btn)
		   (btn-click-event btn)))))))

;;店イベント
(defun town-shop-event ()
  (town-event))
;;----------------------------------------------------------------------------------------------------
;;レベルアップイベント
(defun lvup-event ()
  (with-slots (btn-list) *game*
    (with-slots (left) *mouse*
      (when left
	(loop :for btn :in btn-list
	      :do
		 (when (collide-p *mouse* btn)
		   (btn-click-event btn)))))))
;;----------------------------------------------------------------------------------------------------
;;初期パーティ作り
;;とりあえずジョブ選ぶだけ

(defun create-init-party-event ()
  (with-slots (btn-list temp-init-party) *game*
    (with-slots (left) *mouse*
      (when left
	(loop :for btn :in (append btn-list temp-init-party)
	      :do
		 (when (collide-p *mouse* btn)
		   (btn-click-event btn)
		   (return)))))))
;;----------------------------------------------------------------------------------------------------
;;ボタンリストを押すだけのイベント
(defun only-btn-list-click-event ()
  (with-slots (btn-list) *game*
    (with-slots (left) *mouse*
      (when left
	(loop :for btn :in btn-list
	      :do
		 (when (collide-p *mouse* btn)
		   (btn-click-event btn)))))))

;;----------------------------------------------------------------------------------------------------
;;仲間ステータス確認画面 右クリックで仲間を外す
(defun show-status-event ()
  (with-slots (btn-list) *game*
    (with-slots (right left) *mouse*
      (when (or left right)
	(loop :for btn :in btn-list
	      :do
		 (cond ((and (collide-p *mouse* btn)
			     (eq (type-of btn) 'show-unit-status-btn)
			     right)
			(btn-click-event btn))
		       ((and (collide-p *mouse* btn)
			     (eq (type-of btn) 'end-recruit-btn)
			     left)
			(btn-click-event btn))))))))
;;----------------------------------------------------------------------------------------------------
(defmethod gk:act ((app lowmogecage))
  (with-slots (state selected-unit action-state frame) *game*
    (case state
      (:town
       (case action-state
	 (:town-menu (town-event))
	 (:shop (town-shop-event))
	 (:recruit (only-btn-list-click-event))
	 (:show-status (show-status-event))))
      (:select-race
       (only-btn-list-click-event))
      (:select-job
       (only-btn-list-click-event))
      (:dice-init-ability
       (only-btn-list-click-event))
      (:init-job-level-up
       (only-btn-list-click-event))
      (:select-init-skill
       (only-btn-list-click-event))
      (:show-init-player-unit-status
       (only-btn-list-click-event))
      (:world-map-test
       (set-world-map-data))
      (:create-init-party
       (create-init-party-event))
      (:world-map
       (world-map-event)
       (world-map-update))
      (:title
       (title-event))
      (:battle-ready
       (battle-ready-event))
      (:equip-menu
       (equip-menu-event))
      (:lvup-mode
       (lvup-event))
      (:battle
       (case action-state
	 (:player-turn
	  (battle-event)
	  (turn-end?))
	 (:select-move-mode
	  (select-move-event))
	 (:select-cmd-mode
	  (select-cmd-mode-event))
	 (:move-mode
	  (move-mode-event))
	 (:after-move
	  (after-move-event))
	 (:attack-mode
	  (attack-mode-event))
	 (:select-item-mode
	  (select-item-event))
	 (:select-skill-mode
	  (select-skill-event))
	 (:select-declare-skill-mode
	  (select-declare-skill-event))
	 (:skill-mode
	  (skill-mode-event))
	 (:skill-anime
	  (update-skill-anime))
	 (:move-anime (update-unit-move))
	 (:atk-anime
	  (update-atk-anime selected-unit))
	 (:enemy-turn
	  (update-enemies-action)
	  (turn-end?)))
       (update-fonts)))
    (init-mouse)
    (init-key)
    (incf frame)
    (when (>= frame 100000)
      (setf frame 0))))


(defmethod gk:draw ((app lowmogecage))
  (with-slots (state action-state) *game*
    (gk:scale-canvas *scale-w* *scale-h*)
    (gk:draw-rect (gk:vec2 0 0) *scale-window-w* *scale-window-h* :fill-paint (gk:vec4 0 0 0 1))
    (draw-mouse-test )
    (case state
      (:select-race
       (draw-select-race))
      (:select-job
       (draw-select-job))
      (:dice-init-ability
       (draw-init-ability))
      (:init-job-level-up
       (draw-init-job-level-up))
      (:select-init-skill
       (draw-select-init-skill))
      (:show-init-player-unit-status
       (draw-init-player-unit-status))
      (:town
       (draw-town))
      (:create-init-party
       (draw-create-init-party))
      (:lvup-mode
       (draw-lvup))
      (:world-map-test
       (draw-world-map-data))
      (:world-map
       (draw-world))
      (:title
       ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
       (draw-title))
      (:equip-menu
       (draw-equip-menu))
      (:battle-ready
       (draw-battle-ready))
      (:battle
       (draw-battle)))))


(defun run ()
  (gk:start 'lowmogecage :viewport-resizable t))
