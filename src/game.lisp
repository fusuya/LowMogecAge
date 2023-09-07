(in-package :lowmogecage)
;;TODO アイテム装備 (ワールドマップ スクロール)　クエスト　買い物　町
;; スキル　ヒール 複数ターゲット ダメージ計算 魔法ダメージ アイテムデータ修正

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
  (with-slots (space1 a key1 key2 key3 key4 key5 key0 w s d) *keystate*
    (setf space1 nil a nil
	  ;;key1 nil key2 nil key3 nil key4 nil key5 nil key0 nil
	  ;;w nil  d nil s nil
	  )))

(defun bind-mouse-event ()
  (with-slots ((mouse-x x) (mouse-y y) left right x-for-obj y-for-obj) *mouse*
    (gamekit:bind-cursor (lambda (x y)
			   "Save cursor position"
			   (setf mouse-x (/ x *scale-w*)
				 mouse-y (/ y *scale-h*)
				 x-for-obj (/ mouse-x *scale-obj-w*)
				 y-for-obj (/ mouse-y *scale-obj-h*))))
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
  (with-slots (space1 a key1 key2 key3 key4 key5 key0 w s d) *keystate*
    (gk:bind-button :space :pressed
		    (lambda ()
		      (setf space1 t)))
    (gk:bind-button :space :released
		    (lambda ()
		      (setf space1 nil)))
    (gk:bind-button :a :pressed
		    (lambda ()
		      (setf a t)))
    (gk:bind-button :a :released
		    (lambda ()
		      (setf a nil)))
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
    (gk:bind-button :0 :pressed
		    (lambda ()
		      (setf key0 t)))
    (gk:bind-button :0 :released
		    (lambda ()
		      (setf key0 nil)))
))



;;キャラクターを追加 debug
(defun push-chara-init-party (num unit &optional skill1)
  (with-slots (party item) *game*
    (when (> 5 (length party))
      (let* ((weapon (job-init-weapon unit))
	     (armor (item-make  +a_cloth_armor+))
	     (chara (make-instance unit :job num :hp 30 :maxhp 30
				   ;;:weapon weapon
					:vit 3 :str 7 :agi 2 :res 3 :int 3
					;;:armor armor
					:state :inaction
					:name (nth (random (length *name-list*)) *name-list*))))
	(setf (equiped weapon) (name chara)
	      (equiped armor)  (name chara)
	      (weapon chara) weapon
	      (armor chara) armor
	      party (append party (list chara)))
	(when skill1
	  (setf (skill chara) skill1))
	(push weapon item)
	(push armor item)))))
;; debug
(defun test-create-party-chara ()
  (push-chara-init-party +job_warrior+ 'p-warrior)
  (push-chara-init-party +job_sorcerer+ 'p-sorcerer (list :fire))
  (push-chara-init-party +job_priest+ 'p-priest (list :heal))
  (push-chara-init-party +job_s_knight+ 'p-s-knight)
  (push-chara-init-party +job_archer+ 'p-archer))


;;開始前の初期化
(defmethod gk:post-initialize ((app lowmogecage))
  (set-font)
  (bind-key-event)
  (setf *game* (make-instance 'game :state :title
			      :item (create-item-n +w_max+)
			      :world-pos (gk:vec2 200 400)
				    :scroll (gk:vec2 0 0)
				    :enemy-rate (copy-tree *appear-enemy-rate-monster*) ;; todo
                      		    :btn-list (list (make-instance 'game-start-btn :pos (gk:vec2 550 200)
                                                				   :string "はじめる" :color (gk:vec4 1 0.5 0.5 1)
										   :w 200 :h 64
                                                				   :font *font64*
                                                				   :font-size 64)
                                    		    (make-instance 'game-end-btn :pos (gk:vec2 570 120)
                                                  				 :font *font64*
                                                  				 :string "おわる" :color (gk:vec4 1 0.5 0.4 1)
										 :w 150 :h 64
                                                  				 :font-size 64)))
	*mouse* (make-instance 'mouse))
  (test-create-party-chara)
  (bind-mouse-event))






;;視界 表示できる地形座標ゲット
(defun get-show-cell-coord ()
  (with-slots (party) *game*
    (with-slots (tate yoko p-sight-coord) *battle-field*
      (setf p-sight-coord nil
	    p-sight-coord (loop :for p :in party
				:append (with-slots (sight) p
					  (get-area p sight t)))))))


(defun set-battle-field ()
  (setf *battle-field* (make-instance 'donjon))
  (create-battle-field))


;;------------------------------------------------------------------------------
;;当たり判定
(defmethod collide-p ((mouse mouse) obj)
  (with-slots (x-for-obj y-for-obj) mouse
    (with-slots (pos w h) obj
      (let* ((x1 (gk:x pos))
             (y1 (gk:y pos))
             (x2 (+ x1 w))
             (y2 (+ y1 h)))
        (and (>= x2 x-for-obj x1)
             (>= y2 y-for-obj y1))))))

(defmethod collide-p ((mouse mouse) (btn command-btn))
  (with-slots (x-for-obj y-for-obj) mouse
    (with-slots (pos w h) btn
      (let* ((x1 (gk:x pos))
             (y1 (gk:y pos))
             (x2 (+ x1 w))
             (y2 (+ y1 h)))
        (and (>= x2 x-for-obj x1)
             (>= y2 y-for-obj y1))))))

(defmethod collide-p ((mouse mouse) (btn button))
  (with-slots (x y) mouse
    (with-slots (pos w h) btn
      (let* ((x1 (-  (gk:x pos) 3))
             (y1 (-  (gk:y pos) 7))
             (x2 (+ x1 w))
             (y2 (+ y1 h)))
        (and (>= x2 x x1)
             (>= y2 y y1))))))

;; area 座標リスト
(defmethod collide-p ((mouse mouse) (area list))
  (with-slots (x-for-obj y-for-obj) mouse
    (let* ((mouse-x (floor x-for-obj 32))
	   (mouse-y (floor y-for-obj 32))
	   (mouse-xy (list mouse-x mouse-y)))
      (find mouse-xy area :test #'equal))))
;;--------------------------------------------------------------------------------
;; skillの処理
(defun skill-proc (skill caster targets)
  (with-slots (power depend mp target) skill
    (with-slots (int str) caster
      (let ((fluc (if (eq target :ally) #'+ #'-))
	    (dmg (+ power (if (eq depend :int) int str))))
	(loop :for tar :in targets
	      :do (with-slots (hp) tar
		    (setf hp (funcall fluc hp dmg))))))))


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

(defmethod equip-item ((item shielddesc) unit)
  (with-slots (shield name weapon) unit
    (if weapon
	(unless (eq (hand weapon) :2h)
	  (equip-shield item unit))
	(equip-shield item unit))))

;; btn event ----------------------------------------------------------------


(defmethod btn-click-event ((btn town-exit-btn))
  (with-slots (state btn-list) *game*
    (setf state :world-map
	  btn-list nil)))

(defun create-town-menu-button ()
  (with-slots (btn-list) *game*
    (setf btn-list nil)
    (loop :for posx = 80
	  :for posy :from 400 :to 1000 :by 70
	  :for kind :in (list 'town-exit-btn 'quest-btn 'recruit-btn 'shop-btn)
	  :for str :in '("町を出る" "クエスト" "仲間募集" "買い物")
	  :do (let ((btn (make-instance kind :pos (gk:vec2 posx posy)
					     :w 210 :h 50
					     :font *font64*
					     :color (gk:vec4 1 1 1 1)
					     :string str)))
		(push btn btn-list)))))

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

;;装備メニュー画面の次と前ボタン
(Defun create-next-prev-btn ()
  (with-slots (btn-list) *game*
    (let ((next-btn (make-instance 'next-item-page
				   :pos (gk:vec2 265 90)
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "次→"))
	  (prev-btn (make-instance 'prev-item-page
				   :pos (gk:vec2 25 90)
				   :w 60 :h 40 :font *font48*
				   :color (gk:vec4 1 1 1 1)
				   :string "←前")))
      (push next-btn btn-list)
      (push prev-btn btn-list))))

;;装備メニュー画面でユニット選択に使う
(defun create-unit-btn ()
  (with-slots (btn-list party) *game*
    (loop :for p :in party
	  :for posx :from 500 :by 40
	  :for posy = 30
	  :do (with-slots (origin origin-h origin-w img-id) p
		(let ((unit-btn (make-instance 'unit-btn :origin origin
							 :origin-h origin-h :origin-w origin-w
							 :img-id img-id
							 :pos (gk:vec2 posx posy))))
		  (push unit-btn btn-list))))))



;;ゲーム開始ボタン TODO
(defmethod btn-click-event ((btn game-start-btn))
  (with-slots (state action-state btn-list) *game*
    (setf state :battle-ready
          action-state :ready
	  btn-list nil)
    (set-battle-field)
    (set-party-battle-ready-pos)
    (get-show-cell-coord)))



(defmethod btn-click-event ((btn game-end-btn))
  (gk:stop))

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

;;アイテムボタン
(defmethod btn-click-event ((btn equip-item-btn))
  (with-slots (item equiped-unit) btn
    (with-slots (selected-unit) *game*
      (with-slots (canequip name str shield) selected-unit
	(with-slots (category equiped required-str hand) item
	  (when (and (find category canequip)
		     (>= str required-str)
		     (null equiped-unit))
	    (equip-item item selected-unit)
	    (create-item-btn)
	    (create-next-prev-btn)))))))
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
    (setf movearea (get-area unit move nil))
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
    (let ((rangemax (rangemax weapon))
	  (rangemin (rangemin weapon))
	  (enemies (if (eq team :player) (enemies *battle-field*) (game/party *game*))))
      (loop :for e :in enemies
	    :when (with-slots ((e-x x) (e-y y) atked-pos) e
		    (let* ((diffx (- x e-x))
			   (diffy (- y e-y))
			   (manhatan-dist (+ (abs diffx) (abs diffy))))
		      (>= rangemax manhatan-dist rangemin)))
	      :return t))))
;;----------------------------------------------------------------------------------
;; skill btn
(defun create-skill-btn (unit)
  (with-slots (skill pos) unit
    (let* ((w 60) (h 17)
	   (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
		      (- (gk:x pos) w)
		      (+ (gk:x pos) 35)))
	   (posy (gk:y pos))
	   (btn-y (cond ((>= posy 512)
			 (+ 526 h))
			((< posy 32)
			 -10)
			((<= posy 32)
			 (- posy h))
			(t (+ posy (* h 2) h))))
	   (skill-list (if (> btn-y 32)
			   skill (reverse skill)))
	   (func (if (> btn-y 32)
		     #'- #'+)))
      (loop :for sk :in skill-list
	    :do (let ((name (name (getf *skill-list* sk))))
		  (push (make-instance 'skill-btn :pos (gk:vec2 btn-x (setf btn-y (funcall func btn-y h)))
						  :tag sk :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*
						  :string name)
			(game/btn-list *game*))))
      ;;(print (button/pos (car (game/btn-list *game*))))
      )))


;;----------------------------------------------------------------------------------

;;画面上側or普通
(defun create-action-command-btn-normal (btn-x btn-y w h unit)
  (with-slots (btn-list) *game*
    (with-slots (state skill) unit
      (when (eq state :inaction)
	(push (make-instance 'normal-move-cmd-btn :pos (gk:vec2 btn-x (decf btn-y h))
						  :string "通常移動" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list)
	(push (make-instance 'fast-move-cmd-btn :pos (gk:vec2 btn-x (decf btn-y h))
						:string "全力移動" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (when (attack-p unit)
	(push (make-instance 'attack-cmd-btn :pos (gk:vec2 btn-x (decf btn-y h))
					     :string "通常攻撃" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (when skill
	(push (make-instance 'skill-cmd-btn :pos (gk:vec2 btn-x (decf btn-y h))
					    :string "スキル" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (push (make-instance 'wait-cmd-btn :pos (gk:vec2 btn-x (decf btn-y h))
					 :string "待機" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	    btn-list))))

;;画面下側にいた場合
(defun create-action-command-btn-bottom-side (btn-x btn-y w h unit)
  (with-slots (btn-list) *game*
    (with-slots (state skill) unit
      (push (make-instance 'wait-cmd-btn :pos (gk:vec2 btn-x (incf btn-y h))
					 :string "待機" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	    btn-list)
      (when skill
	(push (make-instance 'skill-cmd-btn :pos (gk:vec2 btn-x (incf btn-y h))
					    :string "スキル" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (when (attack-p unit)
	(push (make-instance 'attack-cmd-btn :pos (gk:vec2 btn-x (incf btn-y h))
					     :string "通常攻撃" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (when (eq state :inaction)
	(push (make-instance 'fast-move-cmd-btn :pos (gk:vec2 btn-x (incf btn-y h))
						:string "全力移動" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	      btn-list))
      (push (make-instance 'normal-move-cmd-btn :pos (gk:vec2 btn-x (incf btn-y h))
						:string "通常移動" :w w :h h :color (gk:vec4 1 1 1 1) :font *font18*)
	    btn-list))))

;; action command btn
(defun create-action-command-btn (unit)
  (with-slots (btn-list) *game*
    (with-slots (pos) unit
      (let* ((w 60) (h 17)
	     (btn-x (if (>= (gk:x pos) *origin-window-w/2*)
			(- (gk:x pos) w)
			(+ (gk:x pos) 35)))
	     (posy (gk:y pos)))
	(cond ((>= posy 512)
	       (create-action-command-btn-normal btn-x (+ 526 h) w h unit))
	      ((< posy 32)
	       (create-action-command-btn-bottom-side btn-x -10 w h unit))
	      ((<= posy 32)
	       (create-action-command-btn-bottom-side btn-x (- posy h)  w h unit))
	      (t (create-action-command-btn-normal btn-x (+ posy (* h 2) h) w h unit)))))))

;;コマンドボタンイベント
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

;;スキル
(defmethod click-command-btn ((btn skill-cmd-btn) unit)
  (with-slots (action-state btn-list) *game*
    (setf action-state :select-skill-mode
	  btn-list nil)
    (create-skill-btn unit)))

;;待機
(defmethod click-command-btn ((btn wait-cmd-btn) unit)
  (with-slots (btn-list) *game*
    (setf btn-list nil)
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
(defun get-can-move-cell (unit x y move movecost enemies allies next mode)
  (with-slots (tate yoko field-array) *battle-field*
    (when (and (>= (1- yoko) x 0) (>= (1- tate) y 0))
      (let* ((cell (aref field-array y x))
             (cost (aref movecost cell))
	     (enemy (if mode
			nil
			(find-if #'(lambda (p) (and (= x (x p)) (= y (y p)))) enemies)))
	     (ally (if mode
		       nil
		       (find-if #'(lambda (p) (and (= x (x p)) (= y (y p)))) allies)))
	     (next-paths (remove (reverse-dir next) '((0 1) (0 -1) (1 0) (-1 0)) :test #'equal))
	     (xy (list x y)))
	(when (and (>= move cost) (>= cost 0)
                   (or (null enemy)
		       (eq (state enemy) :dead))) ;;死んでる敵の上は移動できる
	  (when (and (not (find xy (temparea unit) :test #'equal))
		     (or (null ally)
			 (eq (state ally) :dead)))
	    (push xy (temparea unit)))
          (loop for v in next-paths
		do
		   (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
				      (- move cost) movecost  enemies allies v mode)))))))

;;視界範囲か移動範囲をゲット
(defmethod get-area ((unit unit) range sight?)
  (with-slots (enemies) *battle-field*
    (with-slots (party) *game*
      (with-slots (x y movecost temparea) unit
	(setf temparea nil)
	(loop for v in '((0 1) (0 -1) (1 0) (-1 0))
	      do
		 (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
				    range movecost enemies party nil sight?))
	temparea))))

(defmethod get-area ((unit e-unit) range sight?)
  (with-slots (enemies) *battle-field*
    (with-slots (party) *game*
      (with-slots (x y movecost temparea) unit
	(setf temparea nil)
	(loop for v in '((0 1) (0 -1) (1 0) (-1 0))
	      do
		 (get-can-move-cell unit (+ x (car v)) (+ y (cadr v))
				    range movecost party enemies nil sight?))
	temparea))))

;;----------------------------------------------------------------------------------
;; skillの効果範囲ゲット
(defun get-skill-scope (skill)
  (with-slots (x-for-obj y-for-obj) *mouse*
    (with-slots (r temparea) skill
      (let ((x (floor x-for-obj 32))
	    (y (floor y-for-obj 32)))
	(setf temparea nil)
	(cond
	  ((= r 1)
	   (push (list x y) temparea)
	   temparea)
	  (t
	   (push (list x y) temparea)
	   (loop for v in '((0 1) (0 -1) (1 0) (-1 0))
		 do (get-can-move-cell skill (+ x (car v)) (+ y (cadr v))
				       r #(1 1 1 1 1 1 1 1 1) nil nil nil t))
	   temparea))))))

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


;;プレイヤーユニットの攻撃と敵人型ユニットの防御
(defmethod physical-damage-calc ((atker unit) (defender e-unit))
  (with-slots (weapon str int) atker
    (with-slots (armor shield) defender
      (with-slots (dmg-table critical) weapon
	(let* ((def-num 0))
	  (when armor
	    (incf def-num (def armor)))
	  (when shield
	    (incf def-num (def shield)))
	  (max (- (%damage-calc critical dmg-table) def-num) 0))))))

;;プレイヤーユニットの攻撃とモンスター型ユニットの防御
(defmethod physical-damage-calc ((atker unit) (defender monster))
  (with-slots (weapon str int) atker
    (with-slots (def) defender
      (with-slots (dmg-table critical) weapon
	(max (- (%damage-calc critical dmg-table) def) 0)))))

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
  (with-slots (selected-skill) *game*
    (with-slots (critical dmg-table) selected-skill
      (%damage-calc critical dmg-table))))
;;----------------------------------------------------------------------------------------
;;----------------------------------------------------------------------------------------
;;ダメージフォント
(defun create-damage-font (atker defender dmg-num)
  (with-slots (pos) defender
    (let* ((dmg-x (+ (gk:x pos) 10))
	   (dmg-y (+ (gk:y pos) 5))
	   (x-dir (if (eq (atk-dir atker) :left) :left :right))
	   (dmg (make-instance 'dmg-font :pos (gk:vec2 dmg-x dmg-y)
			       :dmg-num  dmg-num :y-dir :up :x-dir x-dir
			       :miny dmg-y :maxy (+ dmg-y 15))))
      (if (eq (atktype (weapon atker)) :heal)
	  (setf (color dmg) (gk:vec4 0 1 0 1))
	  (setf (color dmg) (gk:vec4 1 1 1 1)))
      dmg)))

;;魔法ダメージ判定
(defun magic-damage-hit (atker defender)
  (with-slots (int-bonus magic-power) atker
    (with-slots (res-bonus) defender
      (let* ((m1 (dice 1 6))
	     (m2 (dice 1 6))
	     (mnd-res (+ (level defender) res-bonus))
	     (res1 (dice 1 6))
	     (res2 (dice 1 6)))
	(cond
	  ((and (= m1 1) (= m2 1))
	   "ミス")
	  ((> (+ magic-power m1 m2) (+ mnd-res res1 res2))
	    (magic-damage-calc))
	  ((<= (+ magic-power m1 m2) (+ mnd-res res1 res2))
	   (floor  (magic-damage-calc) 2)))))))


;;物理ダメージ判定
(defun physical-damage-hit (atker defender)
  (with-slots (dex-bonus hit-value) atker
    (with-slots (agi-bonus avoid-value) defender
      (let* ((hit1 (dice 1 6))
	     (hit2 (dice 1 6))
	     (avoid1 (dice 1 6))
	     (avoid2 (dice 1 6)))
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
	  ((> (+ hit-value hit1 hit2) (+ avoid-value avoid1 avoid2))
	   (physical-damage-calc atker defender))
	  ((<= (+ hit-value hit1 hit2) (+ avoid-value avoid1 avoid2))
	   "ミス"))))))

;;ダメージ計算して表示する位置とか設定 TODO
(defun damage-proc (atker defender atking-type)
  (with-slots (dmg-font) *game*
    (with-slots (dex-bonus) atker
      (with-slots (x y pos obj-type hp atk-spd state agi-bonus) defender
	(let ((dmg-num (cond
			 ((or (eq atking-type :short)
			      (eq atking-type :long))
			  (physical-damage-hit atker defender))
			 ((eq atking-type :magic)
			  (magic-damage-hit atker defender)))))
	  (push (create-damage-font atker defender dmg-num) dmg-font)
	  (when (numberp dmg-num)
	    ;;HPの増減
	    (decf (hp defender) dmg-num)) ;;hpを減らす
	  (cond
	    ((>= 0 hp) ;; hpが0以下になったら死亡
	     ;;(setf state :dead)))))))
	   )))))))
	   ;;(player-get-exp atker defender)
	   ;;(when (and (eq (team atker) :player)
	;;;	      (= (random 2) 1))
	   ;;  (get-item atker)))
	 ;; ((eq (atktype (weapon atker)) :heal)
	  ;; (player-get-exp atker defender))))))



;;ダメージフォントの位置更新
(defun update-damage-font ()
  (with-slots (dmg-font) *game*
    (loop :for dmg :in dmg-font
	  :do
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
		    (setf y-dir :down))))))))

;;----------------------------------------------------------------------------------
;;攻撃アニメ
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
  (with-slots (action-state selected-unit ) *game*
    (with-slots (atk-frame atking-enemy temp-pos pos (unit-state state) team origin) atk-unit
      (when (= 7 atk-frame)
	(damage-proc atk-unit atking-enemy))
      (update-atk-img atk-unit atk-frame)
      (if (and (= (gk:x temp-pos) (gk:x pos))
	       (= (gk:y temp-pos) (gk:y pos)))
	  (progn (setf atk-frame 0
		       selected-unit nil
		       atking-enemy nil
		       temp-pos nil
		       unit-state :end
		       (gk:x origin) (* +action-end+ *origin-obj-w*))
		 (if (eq team :player)
		     (setf action-state :player-turn)
		     (setf action-state :enemy-turn)))
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
    (cond
      ((> diffx 0)
       (incf (gk:x pos) 4))
      ((< diffx 0)
       (decf (gk:x pos) 4)))
    (cond
      ((> diffy 0)
       (incf (gk:y pos) 4))
      ((< diffy 0)
       (decf (gk:y pos) 4)))))

;;ユニットの行動終了処理
(defun unit-action-end (unit)
  (with-slots (action-state selected-unit) *game*
    (with-slots (state selected-cmd origin) unit
      (setf action-state :player-turn
	    state :end
	    selected-cmd nil
            (gk:x origin) (* +action-end+ *origin-obj-w*)
	    selected-unit nil))))

;;プレイヤーユニットの移動終わった後の処理
(defun player-move-end-proc (unit)
  (with-slots (enemies) *battle-field*
    (with-slots (party action-state selected-unit) *game*
      (with-slots ((unit-state state) weapon temp-pos pos origin selected-cmd) unit
	(cond
	  ;;移動のみ
	  ((eq unit-state :fast-move)
	   (unit-action-end unit))
	  ((eq unit-state :normal-move)
	   ;;(let ((attackable (get-attack-can-reach unit)))
	   (if (or (attack-p unit)
		   (skill unit))
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
	     (goalposx (* (car path) *origin-obj-w*))
	     (goalposy (* (cadr path) *origin-obj-h*))
	     (diffx (- goalposx (gk:x pos)))
	     (diffy (- goalposy (gk:y pos))))
	(update-pos diffx diffy selected-unit)
	(when (and  (= (gk:x pos) goalposx)
		    (= (gk:y pos) goalposy))
	  (setf move-paths (cdr move-paths)
		x (floor (gk:x pos) *origin-obj-w*)
		y (floor (gk:y pos) *origin-obj-h*))
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
	      :do (with-slots (origin-w origin-h pos x y) p
		    (let ((cell (nth (random (length init-pos)) init-pos)))
		      (setf ;;posx (posx cell)
			    ;;posy (posy cell)
			    ;;posx2 (+ posx origin-w)
			    ;;posy2 (+ posy origin-h)
			    pos (math:copy-vec2 (pos cell)) ;;(gk:vec2 posx posy)
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
                        movearea (get-area p move nil))
                  (return))))))

;;バトル準備画面でユニットの位置を変える
(defun change-battle-ready-pos ()
  (with-slots (selected-unit party) *game*
    (with-slots ((s-posx posx) (s-posy posy) (s-pos pos) (s-posx2 posx2) (s-posy2 posy2) (s-x x) (s-y y)) selected-unit
      (with-slots (x-for-obj y-for-obj ) *mouse*
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
			    (return)))))))))))

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
		   ;; ((find cell-xy (movearea selected-unit) :test #'equal)
		   ;;  (setf (move-paths selected-unit) (get-move-paths selected-unit cell-xy)
		   ;; 	 state :battle
		   ;; 	 (state selected-unit) :move
		   ;; 	 action-state :move-anime))
		   ))
	       ;;(update-unit-move selected-unit click-cell))))
	       (unit-select-battle-ready)))
	  ((space1 *keystate*)
	   (setf state :battle
		 action-state :player-turn
		 selected-unit nil)))))))


;;-----------------battle-------------------------------------------------------------------------------


;;ターン終了ボタン押したとき
(defun select-player-turn-end ()
  (with-slots (party) *game*
    (print "nnnnndd")
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
  (with-slots (party) *game*
    (with-slots (enemies) *battle-field*
      ;;プレイヤーキャラを選択していなかった場合敵キャラを選択したか調べる
      (unless (set-selected-unit party)
	(set-selected-unit enemies)))))


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
	(let* ((click-cell (get-click-cell))
	       (cell-xy (list (x click-cell) (y click-cell)))
	       (click-enemy (find-if #'(lambda (unit) (collide-p *mouse* unit)) enemies))
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
	     (setf (game/selected-unit *game*) click-enemy))
	     ;;(atk-or-heal-to-clicked-unit selected-unit click-enemy enemies))
	    ;;移動のみ ユニットがいない地形かつ移動範囲内をクリックした
	    ;; ((find cell-xy (movearea selected-unit) :test #'equal)
	    ;;  (setf move-paths (get-move-paths selected-unit cell-xy)
	    ;; 	   (state selected-unit) :move
	    ;; 	   action-state :move-anime)
	    ;;  (init-unit-atked-pos enemies))
	    ))))))


;;バトル中のクリックイベント TODO test temp-pos
(defun left-click-event-in-battle ()
  (with-slots (selected-unit) *game*
    (cond
      ;;プレイヤーキャラを選択している状態
      ((and selected-unit
            (eq (team selected-unit) :player))
       (left-click-with-player-unit-selected selected-unit))
      (t
       (unit-click-in-battle)))))

;;右クリック イベント
(defun right-click-event-in-battle ()
  (with-slots (selected-unit btn-list) *game*
    (cond
      (selected-unit
       (setf selected-unit nil
	     btn-list nil)))))

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
    (setf action-state :player-turn
	  (movearea selected-unit) nil)
    (create-action-command-btn selected-unit)))

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
    (with-slots (pos atking-enemy weapon origin job) selected-unit
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
			      temp-dmg (damage-proc selected-unit e (atking-type weapon)))
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
	 (setf action-state :player-turn))
	((eq state :normal-move)
	 (setf action-state :after-move)))
      (create-action-command-btn selected-unit))))


(defun attack-mode-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (attack-mode-left-click-event))
      (right (attack-mode-right-click-event)))))
;;------------------------------------------------------------------------------------
;; select skill mode
;;クリック
(defun select-skill-mode-left-click-event ()
  (with-slots (btn-list selected-skill selected-unit action-state) *game*
    (loop :for btn :in btn-list
	  :do (when (collide-p *mouse* btn)
		(print "mohge")
		(let ((skill (getf *skill-list* (tag btn))))
		  (with-slots (range r rangemax) skill
		    (setf action-state :skill-mode
			  range (get-area selected-unit rangemax t)
			  selected-skill skill))
		  (setf btn-list nil)
		  (return))))))

;;左クリック
(defun select-skill-mode-right-click-event ()
  (with-slots (btn-list action-state selected-unit) *game*
    (setf action-state :player-turn
	  btn-list nil)
    (create-action-command-btn selected-unit)))


(defun select-skill-event ()
  (with-slots (left right) *mouse*
    (cond
      (left (select-skill-mode-left-click-event))
      (right (select-skill-mode-right-click-event)))))
;;------------------------------------------------------------------------------------

;;  skill mode スキルでターゲットを選択する場面

;;クリック
(defun skill-mode-left-click-event ()
  (with-slots (x-for-obj y-for-obj) *mouse*
    (with-slots (selected-skill party action-state selected-unit) *game*
      (with-slots (range target pos team scope) selected-skill
	(let* ((targets (if (eq target :ally) party (enemies *battle-field*)))
	       (mouse-x (floor x-for-obj 32))
	       (mouse-y (floor y-for-obj 32))
	       (mouse-xy (list mouse-x mouse-y))
	       (target-units nil))
	  (loop :for xy :in scope
		:do (let* ((x (car xy))
			   (y (cadr xy))
			   (tar (find-if #'(lambda (unit) (and (= x (x unit)) (= y (y unit)))) targets)))
		      (when tar
			(push tar target-units))))
	  (when (and target-units
		     (find mouse-xy range :test #'equal))
	    (setf action-state :skill-anime
		  pos (gk:vec2 (* mouse-x 32) (* mouse-y 32))
		  team :player)
	    (skill-proc selected-skill selected-unit target-units)
	    ))))))


;;左クリック
(defun skill-mode-right-click-event ()
  (with-slots (selected-unit selected-skill action-state btn-list) *game*
    (setf selected-skill nil
	  btn-list nil
	  action-state :select-skill-mode)
    (create-skill-btn selected-unit)))

;;スキルの効果範囲を更新
(defun update-skill-scope ()
  (with-slots (x-for-obj y-for-obj) *mouse*
    (with-slots (selected-skill) *game*
      (with-slots (scope range) selected-skill
	(let* ((mouse-x (floor x-for-obj 32))
	       (mouse-y (floor y-for-obj 32))
	       (mouse-xy (list mouse-x mouse-y)))
	  (when (find mouse-xy range :test #'equal)
	    (setf scope (get-skill-scope selected-skill))))))))

(defun skill-mode-event ()
  (with-slots (left right) *mouse*
    (update-skill-scope)
    (cond
      (left (skill-mode-left-click-event))
      (right (skill-mode-right-click-event)))))

;;------------------------------------------------------------------------------------
;; skill anime
(defun update-skill-anime ()
  (with-slots (selected-skill action-state selected-unit) *game*
    (with-slots (interval frame max-frame origin team pos img) selected-skill
      (incf frame)
      (when (zerop (mod frame interval))
	(setf (gk:x origin) (- 32 (gk:x origin)))
	(when (>= frame max-frame)
	  (setf frame 0
		selected-skill nil
		(state selected-unit) :end
		(selected-cmd selected-unit) nil
		(gk:x (origin selected-unit)) (* +action-end+ *origin-obj-w*)
		selected-unit nil)
	  (if (eq team :player)
	      (setf action-state :player-turn)
	      (setf action-state :enemy-turn)))))))

;;------------------------------------------------------------------------------------
;;装備メニュー開く
(defun open-equip-menu ()
  (create-item-btn)
  (create-next-prev-btn)
  ;;(create-unit-btn)
  (setf (game/state *game*) :equip-menu))


;;装備を外す
(defun remove-equip (btn)
  (with-slots (item equiped-unit) btn
    (with-slots (selected-unit) *game*
      (with-slots (weapon armor shield name) selected-unit
	(when (equal equiped-unit name)
	  (cond
	    ;; ((and weapon
	    ;; 	  (eq (type-of item) 'weapondesc))
	    ;;  (setf (equiped weapon) nil
	    ;; 	   weapon nil))
	    ((and armor
		  (eq (type-of item) 'armordesc))
	     (setf (equiped armor) nil
		   armor nil))
	    ((and shield
		  (eq (type-of item) 'shielddesc))
	     (setf (equiped shield) nil
		   shield nil)))
	  (create-item-btn)
	  (create-next-prev-btn))))))

;;装備変更画面でのイベント
(Defun equip-menu-event ()
  (with-slots (a) *keystate*
    (with-slots (left right) *mouse*
      (with-slots (state btn-list) *game*
	(cond
	  (a (setf state :battle
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
  (with-slots (space1 a) *keystate*
    (with-slots (left right) *mouse*
      (with-slots (selected-unit) *game*
	(cond
	  ((and a selected-unit)
	   (open-equip-menu))
	  (space1
	   (select-player-turn-end))
	  (right
	   (right-click-event-in-battle))
	  (left
	   (left-click-event-in-battle)))))))

;;ターンエンド処理
(defun turn-end? ()
  (with-slots (action-state party) *game*
    (cond
      ((and (eq action-state :player-turn)
	    (every #'(lambda (unit) (eq (state unit) :end)) party))
       (print "owaaaa")
       (setf action-state :enemy-turn)
       (dolist (p party)
	 (with-slots (origin state) p
	   (setf state :inaction
		 (gk:x origin) 0))))
      ((and (eq action-state :enemy-turn)
	    (every #'(lambda (unit) (eq (state unit) :end)) (enemies *battle-field*)))
       (setf action-state :player-turn)
       (dolist (e (enemies *battle-field*))
	 (with-slots (state origin) e
	   (setf state :inaction
		 (gk:x origin) 0)))))))






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
    (with-slots (move-paths atk-dir temp-pos pos atking-enemy state origin) selected-unit
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
				atking-enemy target)))
		 (setf state :end
                       (gk:x  origin) (* +action-end+ *origin-obj-w*)))))
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
	(setf movearea (get-area selected-unit move nil))
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
    (with-slots (x-for-obj y-for-obj) *mouse*
      (loop :for btn :in btn-list
     	    :do (when (collide-p *mouse* btn)
         	  (btn-click-event btn))))))

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
    (with-slots (key1 key2 key3 key4 key5 key0) *keystate*
      (with-slots (scroll) *game*
	(let ((arr-x (floor (+ x (gk:x scroll)) 32))
	      (arr-y (floor (+ y (gk:y scroll)) 32)))
	  ;;(format t  "x:~d y:~d~%" arr-x arr-y)
	  ;;(print key1)
	  (world-map-scroll)
	  (cond
	    (key0 (setf (aref *world-map-test-arr* arr-y arr-x) 0))
	    (key1 (setf (aref *world-map-test-arr* arr-y arr-x) 1))
	    (key2 (setf (aref *world-map-test-arr* arr-y arr-x) 2))
	    (key3 (setf (aref *world-map-test-arr* arr-y arr-x) 3))
	    (key4 (setf (aref *world-map-test-arr* arr-y arr-x) 4))
	    (key5 (setf (aref *world-map-test-arr* arr-y arr-x) 5)))
	  )))))

;;----------------------------------------------------------------------------------------------------
;;ワールドマップ上のイベント
(defun world-map-event ()
  (with-slots (scroll move-goal move-paths world-pos) *game*
    (with-slots (x y left) *mouse*
      (when left
	;;(setf move-goal (gk:vec2 (+ x (gk:x scroll)) (+ y (gk:y scroll))))
	(let* ((arr-x (floor (+ x (gk:x scroll)) 32))
	      (arr-y (floor (+ y (gk:y scroll)) 32))
	       (goal (list arr-x arr-y))
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
	)))))

;;ワールド上の地形ゲット ;;TODO pos
(defun get-world-cell ()
  (with-slots (scroll world-pos) *game*
    (let* ((pos world-pos)
	   (arr-x (floor (gk:x pos) 32))
	   (arr-y (floor (gk:y pos) 32)))
      (aref *world-map-data* arr-y arr-x))))

;;ワールド上の移動スピード
(Defun get-world-move-speed ()
  (with-slots (move) *game*
    (let ((cell (get-world-cell)))
      (case cell
	(0 0.2)
	(1 2)
	(2 1)
	(3 1)
	(4 1)
	(5 2)))))

;;ワールドユニット移動
(defun update-world-unit-pos ()
  (with-slots (world-pos move-goal move scroll) *game*
    (when (and move-goal
	       ;;(not (math:vec= move-goal world-pos))
	       )
      (let ((speed (get-world-move-speed)))
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

;;TODO
(defun update-world-move-path ()
  (with-slots (move-paths world-pos move-goal move) *game*
    (let* ((path (car move-paths))
	   (goalposx (+ 16 (* (car path) *origin-obj-w*)))
	   (goalposy (+ 16 (* (cadr path) *origin-obj-h*)))
	   (diffx (- goalposx (gk:x world-pos)))
	   (diffy (- goalposy (gk:y world-pos)))
	   (speed (get-world-move-speed)))
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

;;ワールドマップ更新
(defun world-map-update ()
  (with-slots (world-pos move-goal move scroll move-paths state) *game*
    (world-map-scroll)
    (let ((temp-pos (math:copy-vec2 world-pos)))
      (cond
	(move-paths
	 ;;(print "moge")
	 (update-world-move-path))
	(move-goal
	 (update-world-unit-pos)
	 (when move-goal
	   (let ((diff-pos (gk:subt move-goal world-pos)))
	     (when (and (>= move (gk:x diff-pos) (- move))
			(>= move (gk:y diff-pos) (- move)))
	       (setf move-goal nil))))))
      (let ((cell (get-world-cell)))
	(when (= cell 5) ;; +town+
	  (format t "cell ~a~%" (get-world-cell))
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
		  move-paths nil)
	    (format t "cell ~a~%" (get-world-cell))
	    (create-town-menu-button)))))
    ))
;;----------------------------------------------------------------------------------------------------

(defun town-event ()
  (with-slots (btn-list) *game*
    (with-slots (left) *mouse*
      (when left
	(loop :for btn :in btn-list
	      :do
		 (when (collide-p *mouse* btn)
		   (btn-click-event btn)))))))

;;----------------------------------------------------------------------------------------------------
(defmethod gk:act ((app lowmogecage))
  (with-slots (state selected-unit action-state frame) *game*
    (case state
      (:town
       (town-event))
      (:world-map-test
       (set-world-map-data))
      (:world-map
       (world-map-event)
       (world-map-update))
      (:title
       (title-event))
      (:battle-ready
       (case action-state
	 (:ready (battle-ready-event))))
      ;;(:unit-move (update-unit-move))))
      ;;(update-selected-unit-pos))
      (:equip-menu
       (equip-menu-event))
      (:battle
       (case action-state
	 (:player-turn
	  (battle-event)
	  (turn-end?))
	 (:move-mode
	  (move-mode-event))
	 (:after-move
	  (after-move-event))
	 (:attack-mode
	  (attack-mode-event))
	 (:select-skill-mode
	  (select-skill-event))
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
       (update-damage-font)))
    (init-mouse)
    (init-key)
    (incf frame)))


(defmethod gk:draw ((app lowmogecage))
  (with-slots (state action-state) *game*
    (gk:scale-canvas *scale-w* *scale-h*)
    (gk:draw-rect (gk:vec2 0 0) *scale-window-w* *scale-window-h* :fill-paint (gk:vec4 0 0 0 1))
    (draw-mouse-test )
    (case state
      (:town
       (draw-town-menu))
      (:world-map-test
       (draw-world-map-data))
      (:world-map
       (draw-world))
      (:title
       ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
       (draw-title)
       (draw-btn-list))
      (:equip-menu
       (draw-equip-menu))
      (:battle-ready
       (draw-battle-ready))
      (:battle
       (draw-battle)))))


(defun run ()
  (gk:start 'lowmogecage :viewport-resizable t))
