(in-package :lowmogecage)

;;font64 w=45 h=38


(defun draw-mouse-test ()
  (with-slots (x y) *mouse*
    ;;(gk:draw-text "hoge" (gk:vec2 123 124))
    (gk:draw-text (format nil "x:~d y:~d" x y) (gk:vec2 1000 300)
		  :fill-color (gk:vec4 1 1 1 1) :font *font24* )))



;;カーソルがobjの範囲にあるか
(defun obj-on-cursor-p (obj)
  (with-slots (posx posy posx2 posy2 pos origin-w origin-h) obj
    (with-slots (x-for-obj y-for-obj) *mouse*
      (and (>= posx2 x-for-obj posx)
	   (>= posy2 y-for-obj posy)))))
	;;(gk:draw-rect pos origin-w origin-h :stroke-paint (gk:vec4 1 1 1 1) :thickness 1.5)))))

;;objの範囲にカーソルがあれば□描画
(defun draw-select-rect (obj stroke-paint fill-paint)
  (with-slots (pos origin-w origin-h) obj
    (gk:draw-rect pos origin-w origin-h :fill-paint fill-paint :stroke-paint stroke-paint :thickness 1.5)))

;;----------------------------------------------------------------------------------------------------
;; button
(defmethod draw-text-button-no-waku ((btn button))
  (with-slots (pos string font color w h) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect (gk:subt pos (gk:vec2 3 7)) w h :fill-paint (gk:vec4 1 1 1 1))
      	  (gk:draw-text string pos :font font :fill-color (gk:vec4 0 0 0 1)))
     	(gk:draw-text string pos :font font :fill-color color))))

;; 装備メニュー画面のアイテムリストボタン
(Defmethod draw-text-button-no-waku ((btn equip-item-btn))
  (with-slots (selected-unit) *game*
    (with-slots (pos string font color w h equiped-unit new) btn
      (if (collide-p *mouse* btn)
     	  (progn
	    (gk:draw-rect (gk:subt pos (gk:vec2 3 7)) w h :fill-paint (gk:vec4 1 1 1 1))
      	    (gk:draw-text string pos :font font :fill-color (gk:vec4 0 0 0 1)))
	  (if (eq (name selected-unit) equiped-unit)
	      (gk:draw-text string pos :font font :fill-color (gk:vec4 0 0.5 1 1))
     	      (gk:draw-text string pos :font font :fill-color color)))
      (when equiped-unit
	(gk:draw-text "E" (gk:subt pos (gk:vec2 20)) :fill-color (gk:vec4 1 0 1 1) :font font)))))

;;ボタンの説明
(defun draw-btn-description (btn)
  (with-slots (description) btn
    (gk:draw-rect (gk:vec2 100 7) 500 30 :fill-paint (gk:vec4 0 0 0.3 0.6) :thickness 2
		  :stroke-paint (gk:vec4 1 1 1 0.7) :rounding 3)
    (gk:draw-text description (gk:vec2 110 15) :fill-color (gk:vec4 0.7 1 1 1) :font *font24*)))

;;枠ありボタン bg=黒背景
(defmethod draw-text-btn-with-waku ((btn button) thickness &key (bg nil))
  (with-slots (pos string font color w h) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1))
      	  (gk:draw-text string (gk:add pos (gk:vec2 3 4)) :font font :fill-color (gk:vec4 0 0 0 1)))
	(progn
	  (when bg
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 0 0 0 0.6)))
	  (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
							:thickness thickness)
     	  (gk:draw-text string (gk:add pos (gk:vec2 3 4)) :font font :fill-color color)))))



;;枠ありボタン bg=黒背景
(defmethod draw-text-btn-with-waku ((btn command-btn) thickness &key (bg nil))
  (with-slots (pos string font color w h) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1))
      	  (gk:draw-text string (gk:add pos (gk:vec2 3 4)) :font font :fill-color (gk:vec4 0 0 0 1))
	  (draw-btn-description btn))
	(progn
	  (when bg
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 0 0 0 0.6)))
	  (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
							:thickness thickness)
     	  (gk:draw-text string (gk:add pos (gk:vec2 3 4)) :font font :fill-color color)))))


(defmethod draw-button ((btn button))
  )

(defmethod draw-button ((btn equip-item-btn))
  (draw-text-button-no-waku btn))

(defmethod draw-button ((btn unit-btn))
  ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
  (draw-obj-img btn))


(defun draw-btn-list ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-button-no-waku btn))))

;;バトル中のアクションコマンド
(defun draw-action-command-btn ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-btn-with-waku btn 1 :bg t)
	  )))
;;----------------------------------------------------------------------------------------------------
;;-----------------dmg------------------------------------------------

(Defun draw-stroke-text (string posx posy stroke-color text-color font)
  (gk:draw-text string (gk:vec2 (- posx 2) posy)
		:fill-color stroke-color :font font)
  (gk:draw-text string (gk:vec2 (+ posx 2) posy)
		:fill-color stroke-color :font font)
  (gk:draw-text string (gk:vec2 posx (- posy 2))
		:fill-color stroke-color :font font)
  (gk:draw-text string (gk:vec2 posx (+ posy 2))
		:fill-color stroke-color :font font)
  (gk:draw-text string (gk:vec2 posx posy)
		:fill-color text-color :font font))

(defun draw-dmg-font ()
  (with-slots (dmg-font) *game*
    (loop :for dmg :in dmg-font
	  :do (with-slots (pos dmg-num color font) dmg
		(draw-stroke-text (format nil "~a" dmg-num) (gk:x pos) (gk:y pos) (gk:vec4 0 0 0 1) color font)))))

;;---------------battle-field----------------------------------------------------
;;*objs-img*の描画
(defun draw-obj-img (obj)
  (with-slots (pos img-id origin-w origin-h origin) obj
    (gk:draw-image pos img-id :origin origin :width origin-w :height origin-h)))


;;TODO
(defun draw-cell-info (cell)
  (with-slots (name def heal avoid) cell
    (with-slots (x) *mouse*
      (let* ((rect-b-w 90)
	     (rect-b-h 110)
	     (rect-margin 5)
	     (rect-b-pos (if (>= *window-center* x)
			     (gk:vec2 (- (/ *window-w* 1.5) rect-b-w rect-margin) rect-margin)
			     (gk:vec2 rect-margin rect-margin)))
	     (text-x (if (>= *window-center* x)
			 (+ (- (/ *window-w* 1.5) rect-b-w) 6)
			 15))
	     (name-len (length name))
	     (name-text-x (cond ((= name-len 1) (+ text-x 25))
				((= name-len 2) (+ text-x 15)))))
	(gk:draw-rect rect-b-pos rect-b-w rect-b-h :fill-paint (gk:vec4 0 0 0 1)
						   :stroke-paint (gk:vec4 1 1 1 1) :thickness 5)
	(gk:draw-text (format nil "~a" name)  (gk:vec2 name-text-x 90) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "防御:~a%" def)   (gk:vec2 text-x 65) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "回避:~a%" avoid) (gk:vec2 text-x 40) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "回復:~a%" heal)  (gk:vec2 text-x 15) :font *font24* :fill-color (gk:vec4 1 1 1 1))))))

;;移動範囲表示
(defun draw-movearea ()
  (with-slots (selected-unit) *game*
    (when selected-unit
      (let ((stroke-paint (if (eq (team selected-unit) :player)
			      (gk:vec4 0 0 0.7 1)
			      (gk:vec4 0.7 0 0 1)))
	    (fill-paint (if (eq (team selected-unit) :player)
			    (gk:vec4 0 0 0.7 0.6)
			    (gk:vec4 0.7 0 0 0.6))))
	(loop :for xy :in (movearea selected-unit)
	      :do (let* ((w 32)
			 (h 32)
			 (pos (gk:vec2 (* (car xy) w) (* (cadr xy) h))))
		    (gk:draw-rect pos w h :stroke-paint stroke-paint :fill-paint fill-paint)))))))

;;スキル射程範囲
(defun draw-skill-area ()
  (with-slots (x-for-obj y-for-obj) *mouse*
    (with-slots (selected-skill) *game*
      (when selected-skill
	(with-slots (r range scope) selected-skill
	  (let ((stroke-paint (gk:vec4 0.7 0 0.7 1))
		(fill-paint (gk:vec4 0.8 0.2 0.8 0.6)))
	    (loop :for xy :in range
		  :do (let* ((w 32)
			     (h 32)
			     (pos (gk:vec2 (* (car xy) w) (* (cadr xy) h))))
			(gk:draw-rect pos w h :stroke-paint stroke-paint :fill-paint fill-paint)))
	    (loop :for xy :in scope
		  :do (let* ((w 32)
			     (h 32)
			     (pos (gk:vec2 (* (car xy) w) (* (cadr xy) h))))
			(gk:draw-rect pos w h :stroke-paint (gk:vec4 1 0.7 0 0.6) :fill-paint (gk:vec4 1 0.7 0 0.7))))
	    ))))))

;;TODO 表示する順番 情報は地形やモンスターの上に表示されるようにしたい
(defun draw-battle-field ()
  (with-slots (selected-skill) *game*
    (with-slots (field player-init-pos p-sight-coord) *battle-field*
      (let ((select-cell nil))
	(loop :for cell :in field
	      :do (with-slots (x y pos w h)  cell
		    (when (find (list x y) p-sight-coord :test #'equal)
		      (draw-obj-img cell)
		      (when (collide-p *mouse* cell)
			(setf select-cell cell))
		      )))
	(when select-cell
	  ;;(with-slots (x y) select-cell
	    ;;(if (and selected-skill ;;スキルエリア
	;;	     (find (list x y) (area selected-skill) :test #'equal))
	;;	(draw-select-rect select-cell (gk:vec4 0 1 1 1) (gk:vec4 1 0 1 0.7))
		(draw-select-rect select-cell (gk:vec4 1 1 1 1) (gk:vec4 0 0 0 0))
	    (draw-cell-info select-cell))))))

;;-----------------------------------------------------------------------------------
;;未行動可行動済みか文字列ゲット
(defun get-unit-state-string (unit)
  (with-slots (state) unit
    (cond ((eq state :inaction)
	   (values "未行動" (gk:vec4 0 1 0 1)))
	  ((eq state :end)
	   (values "行動済" (gk:vec4 1 1 0 1)))
	  ((eq state :swoon)
	   (values "気絶" (gk:vec4 1 0 0 1)))
	  ((eq state :atk)
	   (values "攻撃中" (gk:vec4 0 1 1 1))))))


;;ユニット情報
(defmethod draw-unit-data ((e unit) rect-pos-y text-x)
  (with-slots (hp maxhp str agi vit res int name state dex mp maxmp avoid-value level) e
      (let* ((font *font20*)
	     (bottom-y (+ rect-pos-y 10))
	     (line-width 16))
	(multiple-value-bind (act-str act-color) (get-unit-state-string e)
	  (gk:draw-text (format nil "~a" act-str) (gk:vec2 text-x bottom-y) :font font :fill-color act-color))
	(gk:draw-text (format nil "精神力: ~a" res) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 知力 : ~a" int)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "器用度: ~a" dex)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "敏捷度: ~a" agi)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "生命力: ~a" vit) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 筋力 : ~a" str)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "  MP  :~a/~a" mp maxmp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "  HP  :~a/~a" hp maxhp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "名前 :~a" name)  (gk:vec2 text-x (incf bottom-y 17)) :font *font18* :fill-color (gk:vec4 1 1 1 1))
	)))

;;モンスターのユニットデータ
(defmethod draw-unit-data ((e monster) rect-pos-y text-x)
  (with-slots (hp maxhp hit-value avoid-value vit-bonus res-bonus name state def mp maxmp atk-point) e
    (with-slots (x) *mouse*
      (let* ((font *font20*)
	     (bottom-y (+ rect-pos-y 10))
	     (line-width 16))
	(multiple-value-bind (act-str act-color) (get-unit-state-string e)
	  (gk:draw-text (format nil "~a" act-str) (gk:vec2 text-x bottom-y) :font font :fill-color act-color))
	(gk:draw-text (format nil " 防護点 : ~a" def) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 回避力 : ~a" avoid-value) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 命中力 : ~a" hit-value)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "精神抵抗: ~a" res-bonus)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "生命抵抗: ~a" vit-bonus)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 打撃点 : ~a" atk-point) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "   MP   : ~a/~a" mp maxmp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "   HP   : ~a/~a" hp maxhp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "名前 :~a" name)  (gk:vec2 text-x (incf bottom-y 17)) :font *font18* :fill-color (gk:vec4 1 1 1 1))
	))))


;;ユニット情報の枠
(defun draw-unit-info-box (rect-h rect-pos-y rect-margin rect-w name)
  (with-slots (x) *mouse*
    (let* ((rect-pos (if (>= *window-center* x)
			 (gk:vec2 (- (/ *window-w* 1.5) rect-w rect-margin) rect-pos-y)
			 (gk:vec2 rect-margin rect-pos-y))))
      (gk:draw-rect rect-pos rect-w rect-h :fill-paint (gk:vec4 0 0 0 1)
					   :stroke-paint (gk:vec4 1 1 1 1) :thickness 3))))

;;ユニット情報表示
(defun draw-unit-info (unit)
  (with-slots (x) *mouse*
    (let* ((rect-h 172)
	   (rect-margin 5)
	   (rect-pos-y (- (/ *window-h* 1.5) rect-h rect-margin))
	   (rect-w (max 135 (* (length (name unit)) 26)))
	   (text-x (if (>= *window-center* x)
		       (+ (- (/ *window-w* 1.5) rect-w) 1)
		       10)))
      (draw-unit-info-box rect-h rect-pos-y rect-margin rect-w (name unit))
      (draw-unit-data unit rect-pos-y text-x))))
;;---------------------enemy-----------------------------------------------------------


(defun draw-enemies ()
  (with-slots (enemies) *battle-field*
    (with-slots (selected-unit) *game*
	(let ((select-monster nil))
	  (loop :for e :in enemies
		:do (with-slots (pos w h atked-pos) e
		      (when atked-pos
			(gk:draw-rect pos w h :fill-paint (gk:vec4 0.7 0 0 0.7)))
		      (draw-obj-img e)
		      (when (collide-p *mouse* e)
			(setf select-monster e))))
	  (when select-monster
	    (draw-unit-info select-monster))))))
;; (gk:draw-rect pos 32 32 :fill-paint (gk:vec4 0.6 0 0 0.3)
;; 	      :stroke-paint (gk:vec4 1 0 0 1))))))
;;---------------------------------------------------------------------------------------------
;;----------------battle-ready------------------------------------------------------------------------
(defun draw-battle-start-text ()
  (with-slots (flash-flag) *game*
    (when flash-flag
      (gk:draw-text (format nil "戦闘開始") (gk:vec2 380 300) :fill-color (gk:vec4 1 0 1 1) :font *font64*)
      (gk:draw-text (format nil "spaceキー") (gk:vec2 380 250) :fill-color (gk:vec4 1 0 1 1) :font *font64*))))

(defun draw-battle-init-pos ()
  (with-slots (player-init-pos) *battle-field*
    (loop :for cell :in player-init-pos
	  :do (with-slots (pos) cell
		(gk:draw-rect pos 32 32 :fill-paint (gk:vec4 0 0 0.7 0.3)
					:stroke-paint (gk:vec4 0 0 1 1))))))

;;プレイヤーユニット表示
(Defun draw-party-chara ()
  (with-slots (party) *game*
    (let ((on-cursor nil))
      (loop :for p :in party
	    :do (with-slots (pos origin origin-w origin-h img-id atked-pos w h) p
		  (when atked-pos
		    (gk:draw-rect pos 32 32 :fill-paint (gk:vec4 0.9 0.9 0.0 0.9)))
		  (gk:draw-image pos img-id :origin origin :width origin-w :height origin-h)
		  (when (collide-p *mouse* p)
		    (draw-select-rect p (gk:vec4 1 1 1 1) (gk:vec4 0 0 0 0))
		    (setf on-cursor p))))
      (when on-cursor
	(draw-unit-info on-cursor)))))

;;-----------------------------------------------------------------------------------
;; skill
(defun draw-skill-anime ()
  (with-slots (selected-skill) *game*
    (with-slots (pos scope origin img) selected-skill
      (loop :for xy :in scope
	    :do (let ((posx (* (car xy) 32))
		      (posy (* (cadr xy) 32)))
		  (gk:draw-image (gk:vec2 posx posy) img :origin origin :width 32 :height 32))))))

;;-----------------------------------------------------------------------------------

(defun draw-battle-field-border ()
  (with-slots (battle-field-border-x) *battle-field*
    (gk:draw-line (gk:vec2 battle-field-border-x 0) (gk:vec2 battle-field-border-x *scale-window-h*)
		  (gk:vec4 1 1 1 1) :thickness 3)))




(defun draw-title ()
  (gk:draw-rect (gk:vec2 0 0) *scale-window-w* *scale-window-h* :fill-paint (gk:vec4 0 0 0 1))
  ;;(gk:draw-line  (gk:vec2 640 0) (gk:vec2 640 720) (gk:vec4 1 1 1 1))
  ;;(gk:draw-rect (gk:vec2 550 200) 45 38 :stroke-paint (gk:vec4 1 1 1 1))
  (gk:draw-text "Low Mogec Age" (gk:vec2 40 520) :font *font256* :fill-color (gk:vec4 1 0.5 0.5 1)))


;;バトルモード
(defun draw-battle ()
  (with-slots (action-state) *game*
    (gk:scale-canvas *scale-obj-w* *scale-obj-h*)
    (draw-battle-field )
    (draw-movearea)
    (when (eq action-state :skill-mode)
      (draw-skill-area))
    (draw-party-chara)
    (draw-enemies)
    (when (eq action-state :skill-anime)
      (draw-skill-anime))
    (draw-action-command-btn)
    (draw-dmg-font)))


(defun draw-battle-ready ()
  (gk:scale-canvas *scale-obj-w* *scale-obj-h*)
  (draw-battle-field)
  (draw-battle-field-border)
  (Draw-movearea)
  (draw-battle-init-pos)
  (draw-party-chara)
  (draw-enemies)
  (draw-battle-start-text))

;;-------------------------------------------------------------------------------
;; ;;武器変更画面
(defun draw-equip-menu-description ()
  (with-slots (item-page item selected-unit) *game*
    (let ((item-page-max (1+ (floor  (length item) *item-show-max*))))
      (gk:draw-text "持ち物" (gk:vec2 99 790) :fill-color (gk:vec4 1 1 1 1)  :font *font32*)
      (gk:draw-text (format nil "~d / ~d" (1+ item-page) item-page-max) (gk:vec2 230 790)
		    :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-rect (gk:vec2 20 130) 300 650 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
      (gk:draw-text "※外す:装備アイテムにカーソルを合わせて右クリック(武器は外せない)" (gk:vec2 320 50) :fill-color (gk:vec4 1 0 1 1)  :font *font32*)
      (gk:draw-text (format nil "選択中のユニット:~a" (name selected-unit)) (gk:vec2 340 793)
		    :fill-color (gk:vec4 1 1 0 1) :font *font32*)
      ;;(gk:draw-text "※装備中のアイテムは捨てることができません" (gk:vec2  320 20) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      )))

;;現在装備中のアイテム情報
(Defun draw-equiped-item-info ()
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (weapon armor shield) selected-unit
	(gk:draw-text "装備中の武器" (gk:vec2 430 755) :fill-color (gk:vec4 0.5 0.4 1 1) :font *font32*)
	(gk:draw-rect (gk:vec2 380 565) 280 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	(gk:draw-text "装備中の防具" (gk:vec2 730 755) :fill-color (gk:vec4 1 1 0.4 1) :font *font32*)
	(gk:draw-rect (gk:vec2 680 565) 280 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	(when weapon
	  (with-slots (name damage critical hit rangemin rangemax) weapon
	    (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 390 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "威力 : ~a" damage) (gk:vec2 390 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "命中 : ~a" hit) (gk:vec2 390 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "必殺 : ~a" critical) (gk:vec2 390 630) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "射程 : ~a～~a" rangemin rangemax) (gk:vec2 390 600) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    ))
	(when armor
	  (with-slots (name avoid def) armor
	    (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 690 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 690 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	    (gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 690 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))))))


;;装備中のアイテムとカーソル上のアイテムの比較数値
(defun draw-diff-item-ability (damage critical hit selected-unit)
  (with-slots (weapon) selected-unit
    (when weapon
      (with-slots ((w-damage damage) (w-critical critical) (w-hit hit)) weapon
	(let ((diff-damage (- damage w-damage))
	      (diff-cri (- (critical-rate critical) (critical-rate  w-critical)))
	      (diff-hit (- hit w-hit)))
	  (cond
	    ((> diff-damage 0)
	     (gk:draw-text (format nil "+~a" diff-damage) (gk:vec2 510 390) :fill-color (gk:vec4 0 1 0 1)
									    :font *font32*))
	    ((< diff-damage 0)
	     (gk:draw-text (format nil "~a" diff-damage) (gk:vec2 510 390) :fill-color (gk:vec4 1 0 0 1)
									   :font *font32*)))
	  (cond
	    ((> diff-cri 0)
	     (gk:draw-text (format nil "+~a" diff-cri) (gk:vec2 520 330) :fill-color (gk:vec4 0 1 0 1)
									 :font *font32*))
	    ((< diff-cri 0)
	     (gk:draw-text (format nil "~a" diff-cri) (gk:vec2 520 330) :fill-color (gk:vec4 1 0 0 1)
									:font *font32*)))
	  (cond
	    ((> diff-hit 0)
	     (gk:draw-text (format nil "+~a" diff-hit) (gk:vec2 510 360) :fill-color (gk:vec4 0 1 0 1)
									 :font *font32*))
	    ((< diff-hit 0)
	     (gk:draw-text (format nil "~a" diff-hit) (gk:vec2 510 360) :fill-color (gk:vec4 1 0 0 1)
									:font *font32*)))
	  )))))

;;クリティカル率
(defun critical-rate (cri)
  (let ((n (cond
	     ((= cri 11) 3)
	     ((= cri 10) 6)
	     ((= cri 9)  10))))
    (round (* (/ n 36) 100.0))))

;;武器の持ちかた
(defun draw-weapon-holding (hand shield)
  (cond
    ((eq hand :1h) "片手持ち")
    ((eq hand :2h) "両手持ち")
    ((eq hand :1hor2h)
     (if shield
	 "片手持ち"
	 "両手持ち"))))

;;持ち方による武器の威力
(defun 1hor2h-damage (damage shield hand category)
  (cond
    ((and (eq hand :1hor2h)
	  (null shield))
     (cond
       ((eq category :sword)
	(+ damage 10))
       ((eq category :axe)
	(+ damage 19))
       ((eq category :spear)
	(+ damage 5))))
    (t damage)))


;;装備中の武器情報
(defun draw-equiped-weapon-info (weapon shield)
  (with-slots (name damage critical hit rangemin rangemax required-str hand category) weapon
    (let ((dmg (1hor2h-damage damage shield hand category)))
    (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 390 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "威力 : ~a" dmg) (gk:vec2 390 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "命中 : ~a" hit) (gk:vec2 390 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "必殺 : ~a％" (critical-rate critical)) (gk:vec2 390 630) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "射程 : ~a～~a" rangemin rangemax) (gk:vec2 390 600) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 390 570) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "持ち方 : ~a" (draw-weapon-holding hand shield)) (gk:vec2 390 540) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text "装備中の武器" (gk:vec2 430 755) :fill-color (gk:vec4 0.5 0.4 1 1) :font *font32*)
    (gk:draw-rect (gk:vec2 380 525) 330 260 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
    )))

;;選択している武器情報
(defun draw-selected-weapon-info (weapon shield str)
  (with-slots (name damage critical hit rangemin rangemax required-str hand category equiped) weapon
    (let ((dmg (1hor2h-damage damage shield hand category)))
      (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 390 420) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "威力 : ~a" dmg) (gk:vec2 390 390) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "命中 : ~a" hit) (gk:vec2 390 360) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "必殺 : ~a％" (critical-rate critical)) (gk:vec2 390 330) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "射程 : ~a～~a" rangemin rangemax) (gk:vec2 390 300) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 390 270) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "持ち方 : ~a" (draw-weapon-holding hand shield)) (gk:vec2 390 240) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-text (format nil "装備中 : ~a" equiped) (gk:vec2 390 210) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (when (> required-str str)
	(gk:draw-text "筋力不足！" (gk:vec2 550 270) :fill-color (gk:vec4 1 0 0 1) :font *font32*)))))

;;カーソルと重なってる武器情報
(defmethod draw-item-info-with-cursor ((item weapondesc))
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (canequip weapon str shield) selected-unit
	(with-slots (name damage critical hit rangemin rangemax category equiped required-str hand) item
	  ;;装備中の武器
	  (when weapon
	    (draw-equiped-weapon-info weapon shield))
	  ;;選択中の武器
	  (gk:draw-rect (gk:vec2 380 185) 330 300 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	  (gk:draw-text "選択中の武器" (gk:vec2 430 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	  (cond
	    ((and shield
		  (eq hand :2h))
	     (gk:draw-text "盾を装備しているため" (gk:vec2 420 400) :fill-color (gk:vec4 1 0 0 1) :font *font32*)
	     (gk:draw-text "装備できません" (gk:vec2 420 370) :fill-color (gk:vec4 1 0 0 1) :font *font32*))
	    ((find category canequip)
	     (draw-selected-weapon-info item shield str)
	     (draw-diff-item-ability damage critical hit selected-unit))
	    (t
	     (gk:draw-text "装備できません" (gk:vec2 420 400) :fill-color (gk:vec4 1 1 1 1) :font *font32*))))))))

(defmethod draw-item-info-with-cursor ((item armordesc))
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (armor str) selected-unit
	(cond
	  (armor
	   (with-slots (name avoid def required-str) armor
	     (gk:draw-text "装備中の防具" (gk:vec2 730 755) :fill-color (gk:vec4 1 1 0.4 1) :font *font32*)
	     (gk:draw-rect (gk:vec2 680 565) 310 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	     (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 690 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 690 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 690 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 690 630) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))
	  (t (gk:draw-text  "なし" (gk:vec2 790 680) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))
	(with-slots (name avoid def equiped required-str) item
	  (gk:draw-rect (gk:vec2 680 265) 310 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	  (gk:draw-text "選択中の防具" (gk:vec2 730 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	  (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 690 420) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	  (gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 690 390) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	  (gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 690 360) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	  (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 690 330) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	  (gk:draw-text (format nil "装備中 : ~a" equiped) (gk:vec2 690 300) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	  (when (> required-str str)
	    (gk:draw-text "筋力不足！" (gk:vec2 860 330) :fill-color (gk:vec4 1 0 0 1) :font *font32*)))))))

;;盾情報表示
(defmethod draw-item-info-with-cursor ((item shielddesc))
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (shield str weapon) selected-unit
	(cond
	  ((and weapon (eq (hand weapon) :2h))
	   (gk:draw-rect (gk:vec2 730 265) 390 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	   (gk:draw-text "両手持ち武器を装備しているので" (gk:vec2 760 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	   (gk:draw-text "盾を装備できない" (gk:vec2 760 425) :fill-color (gk:vec4 0 1 1 1) :font *font32*))
	  (t
	   (gk:draw-text "装備中の盾" (gk:vec2 780 755) :fill-color (gk:vec4 1 1 0.4 1) :font *font32*)
	   (gk:draw-rect (gk:vec2 730 565) 310 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	   (cond
	     (shield
	      (with-slots (name avoid def required-str) shield
		(gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 740 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
		(gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 740 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
		(gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 740 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
		(gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 740 630) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))
	     (t (gk:draw-text  "なし" (gk:vec2 790 680) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))
	   (with-slots (name avoid def equiped required-str) item
	     (gk:draw-rect (gk:vec2 730 265) 310 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	     (gk:draw-text "選択中の盾" (gk:vec2 780 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 740 420) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 740 390) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 740 360) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 740 330) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "装備中 : ~a" equiped) (gk:vec2 740 300) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (when (> required-str str)
	       (gk:draw-text "筋力不足！" (gk:vec2 910 330) :fill-color (gk:vec4 1 0 0 1) :font *font32*)))
	   ;;1hor2h武器を装備していて縦を装備していなかった場合
	   (when (and (null shield)
		      (eq (hand weapon) :1hor2h))
	     (gk:draw-rect (gk:vec2 380 185) 330 300 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	     (gk:draw-text "装備中の武器情報変更" (gk:vec2 430 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	     (draw-equiped-weapon-info weapon nil)
	     (draw-selected-weapon-info weapon t str)
	     )))))))


;;装備メニュー画面のアイテムボタン表示
(defun draw-item-btn-list ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (with-slots (box?) btn
		(if box?
		    (draw-text-btn-with-waku btn 2)
		    (draw-text-button-no-waku btn))
		(when (and  (collide-p *mouse* btn)
			    (eq 'equip-item-btn (type-of btn)))
		  (draw-item-info-with-cursor (item btn)))))))

(defun draw-equip-menu ()
  ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
  (draw-equip-menu-description)
  (draw-item-btn-list)
  ;;(draw-equiped-item-info)
  )
;;---------------------------------------------------------------------------------------------------------
(Defun draw-world-map-data ()
  (with-slots (scroll) *game*
    (gk:draw-image (gk:subt (gk:vec2 0 0) scroll) :world-map)
    ;;test
    (loop :for x :from 32 :to 3200 :by 32
	  :do
	     (gk:draw-line (gk:vec2 (- x (gk:x scroll)) 0) (gk:vec2 (- x (gk:x scroll)) 3200) (gk:vec4 1 0 0 1))
	     (gk:draw-line (gk:vec2 0 (- x (gk:y scroll))) (gk:vec2 3200 (- x (gk:y scroll))) (gk:vec4 1 0 0 1)))
    (loop :for x :from 0 :below 100
	  :do (loop :for y :from 0 :below 100
		    :do (let ((num (aref *world-map-test-arr* y x)))
			  (gk:draw-text (format nil "~a" num) (gk:subt (gk:vec2 (+ (*  x 32) 10) (+ 7 (* y 32))) scroll)
					:fill-color (gk:vec4 1 1 0 1) :font *font32*)))))
  )
;;---------------------------------------------------------------------------------------------------------
;;ワールドマップ
(defun draw-world-map ()
  (with-slots (scroll) *game*
    (gk:draw-image (gk:subt (gk:vec2 0 0) scroll) :world-map)))

;;ワールドマップ上のキャラ
(defun draw-world-unit ()
  (with-slots (party world-pos scroll monster-symbol) *game*
    (let ((unit (car party)))
      (with-slots (img-id origin-w origin-h origin) unit
	(gk:draw-image (gk:subt world-pos (gk:vec2 16 16) scroll) img-id  :origin origin :width origin-w :height origin-h))
      ;;モンスターシンボル
      (loop :for monster :in monster-symbol
	    :do (with-slots (pos img-id origin origin-h origin-w) monster
		  (gk:draw-image (gk:subt pos scroll) img-id :origin origin :width origin-w :height origin-h))))))

(defun draw-world ()
  (draw-world-map)
  (draw-world-unit))
;;---------------------------------------------------------------------------------------------------------

(Defun draw-town-menu ()
  (with-slots (btn-list) *game*
    (draw-btn-list)))

;;---------------------------------------------------------------------------------------------------------
