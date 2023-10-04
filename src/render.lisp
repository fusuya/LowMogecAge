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
  (with-slots (pos w h) obj
    (gk:draw-rect pos w h :fill-paint fill-paint :stroke-paint stroke-paint :thickness 4)))


;;test macro
(defmacro collide-mouse (btn adjust rounding thickness &body body)
  (let ((btn-g (gensym))
	(g-adjust (gensym))
	(g-rounding (gensym))
	(g-thickness (gensym)))
    `(let ((,btn-g ,btn)
	   (,g-adjust ,adjust)
	   (,g-rounding ,rounding)
	   (,g-thickness ,thickness))
       (with-slots (pos color font w h string) ,btn-g
	   (if (collide-p *mouse* ,btn-g)
	       (progn
		 (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding ,g-rounding)
      		 (gk:draw-text string (gk:add pos ,g-adjust) :font font :fill-color (gk:vec4 0 0 0 1))
		 ,@body)
	       (progn
		 (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
						:thickness ,g-thickness :rounding ,g-rounding)
     		 (gk:draw-text string (gk:add pos ,g-adjust) :font font :fill-color color)))))))
;;----------------------------------------------------------------------------------------------------
;;持ち方
(defun get-weapon-hold-string (hand)
  (case hand
    (:1h "片手")
    (:2h "両手")
    (:1hor2h "片手or両手")))

;;両手持ちの武器別ダメージ
(defun 2h-damage-by-category (damage category)
  (cond
    ((eq category :sword)
     (+ damage 10))
    ((eq category :axe)
     (+ damage 10))
    ((eq category :spear)
     (+ damage 5))
    (t damage)))

;;両手持ちの武器別命中
(defun 2h-hit-by-category (hit category)
  (cond
    ((eq category :spear)
     0)
    (t hit)))

;;カーソルと重なってる店武器情報
(defmethod draw-sale-item-info-with-cursor ((item weapondesc))
  (with-slots (name damage critical hit rangemin rangemax category equiped required-str hand) item
    (let ((posx 770)
	  (posy 650)
	  (decy 40)
	  (font *font48*))
      (gk:draw-rect (gk:vec2 760 350) 510 370 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2 :rounding 9)
      (gk:draw-text (format nil "  名前   : ~a" name) (gk:vec2 posx posy) :fill-color (gk:vec4 1 1 1 1) :font font)
      (if (eq hand :1hor2h)
	  (gk:draw-text (format nil "  威力   : ~a or ~a" damage (2h-damage-by-category damage category)) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
	  (gk:draw-text (format nil "  威力   : ~a" damage) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font))
      (if (eq hand :1hor2h)
	  (gk:draw-text (format nil "  命中   : ~a or ~a" hit (2h-hit-by-category hit category)) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
	  (gk:draw-text (format nil "  命中   : ~a" hit) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font))
      (gk:draw-text (format nil "  必殺   : ~a％" (critical-rate critical)) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil "  射程   : ~a～~a" rangemin rangemax) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil " 持ち方  : ~a" (get-weapon-hold-string hand)) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font))))

;;店防具データ
(defmethod draw-sale-item-info-with-cursor ((item armordesc))
  (with-slots (name category required-str def avoid) item
    (let ((posx 770)
	  (posy 650)
	  (decy 40)
	  (font *font48*))
      (gk:draw-rect (gk:vec2 760 350) 510 370 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2 :rounding 9)
      (gk:draw-text (format nil "   名前   : ~a" name) (gk:vec2 posx posy) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil "  防護点  : ~a" def) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil "   回避   : ~a" avoid) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil " 必要筋力 : ~a" required-str) (gk:vec2 posx (decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font font)
      )))

;;仕様アイテム
(defmethod draw-sale-item-info-with-cursor ((item use-item))
  (with-slots (name category tag) item
    (let ((posx 770)
	  (posy 650)
	  (decy 40)
	  (font *font48*))
      (gk:draw-rect (gk:vec2 760 350) 510 370 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2 :rounding 9)
      (gk:draw-text (format nil "   名前   : ~a" name) (gk:vec2 posx posy) :fill-color (gk:vec4 1 1 1 1) :font font)
      (gk:draw-text (format nil "~a" (getf *btn-description* tag)) (gk:vec2 posx (decf posy decy))
		    :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      )))

;;----------------------------------------------------------------------------------------------------
;; button
(defmethod draw-text-button-no-waku ((btn button) adjust)
  (with-slots (pos string font color w h) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1))
      	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1)))
     	(gk:draw-text string (gk:add pos adjust):font font :fill-color color))))

;; 装備メニュー画面のアイテムリストボタン
(Defmethod draw-text-button-no-waku ((btn equip-item-btn) adjust)
  (with-slots (selected-unit) *game*
    (with-slots (pos string font color w h equiped-unit new) btn
      (if (collide-p *mouse* btn)
     	  (progn
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1))
      	    (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1)))
	  (if (eq (name selected-unit) equiped-unit)
	      (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0.5 1 1))
     	      (gk:draw-text string (gk:add pos adjust) :font font :fill-color color)))
      (cond
	(new
	 (gk:draw-text "N" (gk:subt pos (gk:vec2 33 -18)) :fill-color (gk:vec4 0.3 0.8 1 1) :font font)
	 (gk:draw-text "e" (gk:subt pos (gk:vec2 24 -13)) :fill-color (gk:vec4 0.4 0.9 0.7 1) :font font)
	 (gk:draw-text "w" (gk:subt pos (gk:vec2 15 -8)) :fill-color (gk:vec4 1 0.6 1 1) :font font))
	(equiped-unit
	 (gk:draw-text "E" (gk:subt pos (gk:vec2 20 -8)) :fill-color (gk:vec4 1 0 1 1) :font font))))))

;;店アイテムボタン
(defmethod draw-text-button-no-waku ((btn sale-item-btn) adjust)
  (with-slots (selected-unit) *game*
    (with-slots (pos string font color w h new price item) btn
      (if (collide-p *mouse* btn)
     	  (progn
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1))
      	    (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1))
	    (draw-sale-item-info-with-cursor item))
     	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color color))
      (gk:draw-text (format nil "~4d G" price) (gk:add pos (gk:vec2 360 0)) :fill-color (gk:vec4 1 1 1 1)
		    :font font))))


;;ボタンの説明
(defun draw-btn-description (btn)
  (with-slots (description pos) btn
    (let ((posy (if (< (gk:y pos) 300) (- *window-h* 20) 15)))
      (gk:draw-rect (gk:vec2 100 (- posy 10)) 650 35 :fill-paint (gk:vec4 0 0 0.3 0.6) :thickness 2
						    :stroke-paint (gk:vec4 1 1 1 0.7) :rounding 3)
      (gk:draw-text description (gk:vec2 110 posy) :fill-color (gk:vec4 0.7 1 1 1) :font *font32*))))

;;枠ありボタン bg=黒背景
(defmethod draw-text-btn-with-waku ((btn button) thickness adjust &key (bg nil) (rounding 1) (descri nil))
  (with-slots (pos string font color w h description) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding rounding)
      	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1))
	  (when descri
	    (gk:draw-text (button/description btn) (gk:vec2 100 20) :fill-color (gk:vec4 0 1 0 1) :font *font64*)))
	(progn
	  (when bg
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 0 0 0 0.6) :rounding rounding))
	  (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
							:thickness thickness :rounding rounding)
     	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color color)))))



;;枠ありボタン bg=黒背景
(defmethod draw-text-btn-with-waku ((btn command-btn) thickness adjust &key (bg nil) (rounding 1))
  (with-slots (pos string font color w h) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding rounding)
      	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1))
	  (draw-btn-description btn))
	(progn
	  (when bg
	    (gk:draw-rect pos w h :fill-paint (gk:vec4 0 0 0 0.6) :rounding rounding))
	  (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
							:thickness thickness :rounding rounding)
     	  (gk:draw-text string (gk:add pos adjust) :font font :fill-color color)))))

;;宣言スキル欄  枠ありボタン bg=黒背景
(defmethod draw-text-btn-with-waku ((btn declare-skill-btn) thickness adjust &key (bg nil) (rounding 1))
  (with-slots (selected-unit) *game*
    (with-slots (active-declare-skill) selected-unit
      (with-slots (pos string font color w h tag) btn
	(when (collide-p *mouse* btn)
	  (draw-btn-description btn))
	(if (find tag active-declare-skill)
     	    (progn
              (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding rounding)
      	      (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1)))
	    (progn
	      (when bg
		(gk:draw-rect pos w h :fill-paint (gk:vec4 0 0 0 0.6) :rounding rounding))
	      (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
				    :thickness thickness :rounding rounding)
     	      (gk:draw-text string (gk:add pos adjust) :font font :fill-color color)))))))


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
	  :do (draw-text-btn-with-waku btn 1 (gk:vec2 3 4) :bg t)
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

;;battle画面の基本オブジェクト
(defun draw-obj-img-in-battle (obj)
  (with-slots (pos img-id translate-x translate-y) obj
    (bg:draw-image pos *battle-obj-w* *battle-obj-h* (gk::resource-by-id img-id)
		   :scale-x *scale-obj-w* :scale-y *scale-obj-h*
		   :translate-x translate-x :translate-y translate-y)))




;;移動範囲表示
(defun draw-movearea ()
  (with-slots (selected-unit) *game*
    (when selected-unit
      (let ((stroke-paint (if (eq (team selected-unit) :player)
			      (gk:vec4 0 0 0.7 0.6)
			      (gk:vec4 0.7 0 0 0.6)))
	    (fill-paint (if (eq (team selected-unit) :player)
			    (gk:vec4 0 0 0.7 0.4)
			    (gk:vec4 0.7 0 0 0.4))))
	(loop :for xy :in (movearea selected-unit)
	      :do (let* ((w *battle-obj-w*)
			 (h *battle-obj-h*)
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
		  :do (let* ((w *battle-obj-w*)
			     (h *battle-obj-h*)
			     (pos (gk:vec2 (* (car xy) w) (* (cadr xy) h))))
			(gk:draw-rect pos w h :stroke-paint stroke-paint :fill-paint fill-paint)))
	    (loop :for xy :in scope
		  :do (let* ((w *battle-obj-w*)
			     (h *battle-obj-h*)
			     (pos (gk:vec2 (* (car xy) w) (* (cadr xy) h))))
			(gk:draw-rect pos w h :stroke-paint (gk:vec4 1 0.7 0 0.6) :fill-paint (gk:vec4 1 0.7 0 0.7))))
	    ))))))

;;TODO 表示する順番 情報は地形やモンスターの上に表示されるようにしたい
(defun draw-battle-field ()
  (with-slots (selected-skill action-state) *game*
    (with-slots (field player-init-pos p-sight-coord) *battle-field*
      (let ((select-cell nil))
	(loop :for cell :in field
	      :do (with-slots (x y pos w h)  cell
		    (when (find (list x y) p-sight-coord :test #'equal)
		      (draw-obj-img-in-battle cell)
		      (when (collide-p *mouse* cell)
			(setf select-cell cell))
		      )))
	(when (and select-cell
		   (or (eq action-state :player-turn)
		       (eq action-state :skill-mode)
		       (eq action-state :attack-mode)
		       (eq action-state :move-mode)))
	  ;;(with-slots (x y) select-cell
	  ;;(if (and selected-skill ;;スキルエリア
	  ;;	     (find (list x y) (area selected-skill) :test #'equal))
	  ;;	(draw-select-rect select-cell (gk:vec4 0 1 1 1) (gk:vec4 1 0 1 0.7))
	  (draw-select-rect select-cell (gk:vec4 1 1 1 1) (gk:vec4 0 0 0 0))
	  )))))

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
      (let* ((font *font32*)
	     (bottom-y (+ rect-pos-y 10))
	     (line-width 25))
	(multiple-value-bind (act-str act-color) (get-unit-state-string e)
	  (gk:draw-text (format nil "~a" act-str) (gk:vec2 text-x bottom-y) :font font :fill-color act-color))
	(gk:draw-text (format nil "精神力: ~a" res) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 知力 : ~a" int)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "器用度: ~a" dex)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "敏捷度: ~a" agi)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "生命力: ~a" vit) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 筋力 : ~a" str)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "  MP  : ~a/~a" mp maxmp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "  HP  : ~a/~a" hp maxhp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "名前 :~a" name)  (gk:vec2 text-x (incf bottom-y 24)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	)))

;;モンスターのユニットデータ
(defmethod draw-unit-data ((e monster) rect-pos-y text-x)
  (with-slots (hp maxhp hit-value avoid-value vit-bonus res-bonus name state def mp maxmp atk-point) e
    (with-slots (x) *mouse*
      (let* ((font *font32*)
	     (bottom-y (+ rect-pos-y 10))
	     (line-width 25))
	(multiple-value-bind (act-str act-color) (get-unit-state-string e)
	  (gk:draw-text (format nil "~a" act-str) (gk:vec2 text-x bottom-y) :font font :fill-color act-color))
	(gk:draw-text (format nil " 防護点 : ~2d" def) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 回避力 : ~2d" avoid-value) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 命中力 : ~2d" hit-value)   (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "精神抵抗: ~2d" res-bonus)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "生命抵抗: ~2d" vit-bonus)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " 打撃点 : ~2d" atk-point) (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "   MP   : ~2d" mp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "   HP   : ~2d" hp)  (gk:vec2 text-x (incf bottom-y line-width)) :font font :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "名前 :~a" name)  (gk:vec2 text-x (incf bottom-y 24)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	))))


;;ユニット情報の枠
(defun draw-unit-info-box (rect-h rect-pos-y rect-margin rect-w name)
  (with-slots (x) *mouse*
    (let* ((rect-pos (if (>= *window-center* x)
			 (gk:vec2 (- *window-w* rect-w rect-margin) rect-pos-y)
			 (gk:vec2 rect-margin rect-pos-y))))
      (gk:draw-rect rect-pos rect-w rect-h :fill-paint (gk:vec4 0 0 0 1)
					   :stroke-paint (gk:vec4 1 1 1 1) :thickness 3 :rounding 10))))

;;ユニット情報表示
(defun draw-unit-info (unit)
  (with-slots (x) *mouse*
    (let* ((rect-h 255)
	   (rect-margin 5)
	   (rect-pos-y (- *window-h* rect-h rect-margin))
	   (rect-w (max 160 (* (length (name unit)) 29)))
	   (text-x (if (>= *window-center* x)
		       (+ (- *window-w* rect-w) 1)
		       10)))
      (draw-unit-info-box rect-h rect-pos-y rect-margin rect-w (name unit))
      (draw-unit-data unit rect-pos-y text-x))))
;;---------------------enemy-----------------------------------------------------------


(defun draw-enemies ()
  (with-slots (enemies) *battle-field*
    (with-slots (selected-unit skill-target-units) *game*
	(let ((select-monster nil))
	  (loop :for e :in enemies
		:do (with-slots (pos w h atked-pos x y) e
		      (when atked-pos
			(gk:draw-rect pos w h :fill-paint (gk:vec4 0.7 0 0 0.7)))
		      (draw-obj-img-in-battle e)
		      (when (collide-p *mouse* e)
			(setf select-monster e))
		      (when (find e skill-target-units :test #'equal)
			(gk:draw-rect pos w h :fill-paint (gk:vec4 1 0 0 0.5)))))
	  (when select-monster
	    (draw-unit-info select-monster))
	  ))))
;; (gk:draw-rect pos 32 32 :fill-paint (gk:vec4 0.6 0 0 0.3)
;; 	      :stroke-paint (gk:vec4 1 0 0 1))))))
;;---------------------------------------------------------------------------------------------
;;----------------battle-ready------------------------------------------------------------------------
(defun draw-battle-start-text ()
  (with-slots (flash-flag) *game*
    (when flash-flag
      (gk:draw-text (format nil "初期位置を決めてください") (gk:vec2 400 470) :fill-color (gk:vec4 0 1 1 1) :font *font64*)
      (gk:draw-text (format nil "戦闘開始") (gk:vec2 548 400) :fill-color (gk:vec4 1 0 1 1) :font *font64*)
      (gk:draw-text (format nil "spaceキー") (gk:vec2 548 350) :fill-color (gk:vec4 1 0 1 1) :font *font64*))))

;;バトル開始時の初期位置範囲
(defun draw-battle-init-pos ()
  (with-slots (player-init-pos) *battle-field*
    (loop :for cell :in player-init-pos
	  :do (with-slots (pos) cell
		(gk:draw-rect pos *battle-obj-w* *battle-obj-h* :fill-paint (gk:vec4 0 0 0.7 0.3)
					:stroke-paint (gk:vec4 0 0 1 1))))))

;;プレイヤーユニット表示
(Defun draw-party-chara ()
  (with-slots (party) *game*
    (let ((on-cursor nil))
      (loop :for p :in party
	    :do (with-slots (pos origin origin-w origin-h img-id atked-pos w h) p
		  (when atked-pos
		    (gk:draw-rect pos 32 32 :fill-paint (gk:vec4 0.9 0.9 0.0 0.9)))
		  (draw-obj-img-in-battle p)
		  (when (collide-p *mouse* p)
		    (draw-select-rect p (gk:vec4 1 1 1 1) (gk:vec4 0 0 0 0))
		    (setf on-cursor p))))
      (when on-cursor
	(draw-unit-info on-cursor)))))


;;選択したユニットに四角つける
(defun draw-selected-unit-rect ()
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (pos) selected-unit
	(gk:draw-rect pos *battle-obj-w* *battle-obj-h* :stroke-paint *white* :thickness 3)))))
;;-----------------------------------------------------------------------------------
;; skill
(defun draw-skill-anime ()
  (with-slots (selected-skill skill-target-units) *game*
    (with-slots (pos scope translate-x translate-y img) selected-skill
      (loop :for unit :in skill-target-units
	    :do (with-slots (pos) unit
		  (bg:draw-image pos *battle-obj-w* *battle-obj-h*
				 (gk::resource-by-id img) :scale-x *scale-obj-w* :scale-y *scale-obj-h*
							  :translate-x translate-x :translate-y translate-y))))))

;;-----------------------------------------------------------------------------------
;;スキルターゲット選択中の宣言スキルの説明
(defun draw-declare-skill-description ()
  (with-slots (selected-unit selected-skill skill-target-units expand-magic-area expand-magic-dist) *game*
    (with-slots (active-declare-skill) selected-unit
      (with-slots (target mp) selected-skill
	(with-slots ((mouse-y y)) *mouse*
	  (let* ((posy (if (>= mouse-y 408) 10 (- *window-h* 30)))
		 (posx 200)
		 (h 30)
		 (func (if (> mouse-y 408)
			   #'- #'+))
		 (desc-list `((:me-number :one ,(format nil "魔法拡大/数:魔法発動は対象を選んでから右クリック 消費MP:~d"
							(* mp (length skill-target-units))))
			      (:me-area :area ,(format nil "魔法拡大/範囲:Eキーで拡大,Rキーで縮小 消費MP:~d"
						       (* mp (1+ expand-magic-area))))
			      (:me-distance ,target ,(format nil "魔法拡大/距離:Fキーで拡大、Gキーで縮小 消費MP:~d"
							(* mp expand-magic-dist))))))
	    (loop :for desc :in desc-list
		  :do (let ((declare-skill (car desc))
			    (skill-target (cadr desc))
			    (desc-string (caddr desc)))
			(when (and (find declare-skill active-declare-skill)
				   (eq target skill-target))
			  (gk:draw-rect (gk:vec2 190 (- posy 10)) 850 35 :fill-paint (gk:vec4 0 0 0.3 0.6) :thickness 2
									 :stroke-paint (gk:vec4 1 1 1 0.7) :rounding 3)
			  (gk:draw-text desc-string
					(gk:vec2 posx posy) :fill-color *white* :font *font32*)
			  (setf posy (funcall func posy (+ h 1))))))))))))


;;-----------------------------------------------------------------------------------

(defun draw-battle-field-border ()
  (with-slots (battle-field-border-x) *battle-field*
    (gk:draw-line (gk:vec2 battle-field-border-x 0) (gk:vec2 battle-field-border-x *scale-window-h*)
		  (gk:vec4 1 1 1 1) :thickness 3)))




(defun draw-title ()
  (with-slots (btn-list) *game*
  (gk:draw-rect (gk:vec2 0 0) *scale-window-w* *scale-window-h* :fill-paint (gk:vec4 0 0 0 1))
  ;; (bg:draw-image (gk:vec2 320 320) 48 48 (gk::resource-by-id :monster-img) :scale-x 1.5 :scale-y 1.5
  ;; 		 :translate-y -48)
  (gk:draw-text "Low Mogec Age" (gk:vec2 40 520) :font *font256* :fill-color (gk:vec4 1 0.5 0.5 1))
    (loop :for btn :in btn-list
	  :do (draw-text-button-no-waku btn (gk:vec2 4 8)))))


;;バトルモード
(defun draw-battle ()
  (with-slots (action-state) *game*
    ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
    (draw-battle-field )
    (draw-movearea)
    (when (eq action-state :skill-mode)
      (draw-skill-area))
    (draw-party-chara)
    (draw-enemies)
    (when (eq action-state :skill-anime)
      (draw-skill-anime))
    (when (eq action-state :skill-mode)
      (draw-declare-skill-description))
    (draw-action-command-btn)
    (draw-dmg-font)))


(defun draw-battle-ready ()
  ;;(gk:scale-canvas *scale-obj-w* *scale-obj-h*)
  (draw-battle-field)
  ;;(draw-battle-field-border)
  ;;(Draw-movearea)
  (draw-battle-init-pos)
  (draw-party-chara)
  (draw-selected-unit-rect)
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


;;持ち方による武器の威力
(defun 1hor2h-damage (damage shield hand category)
  (cond
    ((and (eq hand :1hor2h)
	  (null shield))
     (2h-damage-by-category damage category))
    (t damage)))

;;装備中のアイテムとカーソル上のアイテムの比較数値
(defun draw-diff-item-ability (damage critical hit selected-unit)
  (with-slots (weapon shield) selected-unit
    (when weapon
      (with-slots ((w-damage damage) (w-critical critical) (w-hit hit) hand category) weapon
	(let ((diff-damage (- damage (1hor2h-damage w-damage shield hand category)))
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
      (with-slots (weapon str shield) selected-unit
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
	    (t
	     (draw-selected-weapon-info item shield str)
	     (draw-diff-item-ability (1hor2h-damage damage shield hand category) critical hit selected-unit))))))))
	    ;;(t
	     ;;(gk:draw-text "装備できません" (gk:vec2 420 400) :fill-color (gk:vec4 1 1 1 1) :font *font32*))))))))

;;カーソル重なってる防具上号
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

;;盾情報表示
(defmethod draw-item-info-with-cursor ((item use-item))
  (with-slots (name equiped) item
    (let ((posx 690)
	  (posy 720)
	  (decy 30)
	  (font *font32*))
    (gk:draw-rect (gk:vec2 680 565) 310 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
    (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 posx posy) :fill-color (gk:vec4 1 1 1 1) :font FONT)
    (gk:draw-text (format nil "装備者 : ~a" equiped) (gk:vec2 posx (Decf posy decy)) :fill-color (gk:vec4 1 1 1 1) :font FONT))))

;;装備メニュー画面のアイテムボタン表示
(defun draw-item-btn-list ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (with-slots (box?) btn
		(if box?
		    (draw-text-btn-with-waku btn 2 (gk:vec2 3 4))
		    (draw-text-button-no-waku btn (gk:vec2 3 8)))
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
		    :do (let ((num (aref *world-map-data* y x)))
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

;;ワールドマップガイド
(defun draw-world-map-guide ()
  (with-slots (scroll flash-flag) *game*
    (when flash-flag
      (gk:draw-text "↑W" (gk:vec2 620 770) :fill-color (gk:vec4 1 1 0 1) :font *font64*)
      (gk:draw-text "↓S" (gk:vec2 620 10) :fill-color (gk:vec4 0 1 1 1) :font *font64*)
      (gk:draw-text "→D" (gk:vec2 1240 400) :fill-color (gk:vec4 0.5 0.3 1 1) :font *font64*)
      (gk:draw-text "A←" (gk:vec2 4 400) :fill-color (gk:vec4 1 0 0.3 1) :font *font64*)
      )))

(defun draw-world ()
  (draw-world-map)
  (draw-world-map-guide)
  (draw-world-unit))
;;---------------------------------------------------------------------------------------------------------
;;町
;;ようこそ
(defun draw-welcome-town ()
  (with-slots (selected-town) *game*
    (gk:draw-text (format nil "ようこそ、~aの町へ" (name selected-town))
		  (gk:vec2 410 750) :fill-color (gk:vec4 0.5 0.3 0.7 1) :font *font64*)))

;;所持金
(defun draw-money ()
  (with-slots (money) *game*
    (gk:draw-rect (gk:vec2 880 37) 300 50 :stroke-paint (gk:vec4 1 1 1 1)
		  :thickness 2 :rounding 9)
    (gk:draw-text (format nil "所持金  ~dG" money) (gk:vec2 900 50)
		  :fill-color (gk:vec4 1 0.5 0.7 1) :font *font48*)))

;;町メニュー
(Defun draw-town-menu ()
  (draw-welcome-town)
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-btn-with-waku btn 2 (gk:vec2 10 8) :rounding 8 :descri t))
    (draw-money)))



;;店の補足文
(defun draw-shop-description ()
  (with-slots (selected-town) *game*
    (with-slots (sale-item sale-item-page) selected-town
      (let ((item-page-max (1+ (floor  (length sale-item) *sale-item-show-max*))))
	(gk:draw-text "商品" (gk:vec2 150 740) :fill-color (gk:vec4 1 1 1 1)  :font *font48*)
	(gk:draw-text (format nil "~d / ~d" (1+ sale-item-page) item-page-max) (gk:vec2 250 740)
		      :fill-color (gk:vec4 1 1 1 1) :font *font48*)
	(gk:draw-text "※購入したものは所持品に追加されます" (gk:vec2 800 150)
		      :fill-color (gk:vec4 0.2 1 0.4 1) :font *font32*)
	(draw-money)))))

;;店売ってるアイテム
(defun draw-shop-item ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-button-no-waku btn (gk:vec2 3 7)))
    (gk:draw-rect (gk:vec2 150 105) 480 620 :stroke-paint (gk:vec4 1 1 1 1) :thickness 3 :rounding 8)
    (draw-shop-description)))


;;ステータス表示
(defun draw-unit-status (unit status-posx status-posy bonus-posx bonus-posy job-posx job-posy skill-posx skill-posy line-width font)
  (with-slots (hp maxhp mp maxmp str dex agi vit res int dex-bonus vit-bonus res-bonus
	       int-bonus str-bonus agi-bonus magic-power job-level-list name
	       passive-skill action-skill declare-skill race move) unit
    (gk:draw-text (format nil " 名前  : ~a" name) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "  HP   : ~d / ~d" hp maxhp) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "  MP   : ~d / ~d" mp maxmp) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil " 筋力  : ~2d" str) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "器用度 : ~2d" dex) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "敏捷度 : ~2d" agi) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "生命力 : ~2d" vit) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "精神力 : ~2d" res) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil " 知力  : ~2d" int) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "移動力 : ~2d" move) (gk:vec2 status-posx (decf status-posy line-width))
		  :fill-color *white* :font font)
    ;;種族
    (gk:draw-text "種族" (gk:vec2 bonus-posx (+ bonus-posy 100))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "~a" race) (gk:vec2 bonus-posx (+ bonus-posy 55))
		  :fill-color *white* :font font)
    ;;ボーナス
    (gk:draw-text (format nil " 筋力ボーナス : ~d" str-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "器用度ボーナス: ~d" dex-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "敏捷度ボーナス: ~d" agi-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "生命力ボーナス: ~d" vit-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil "精神力ボーナス: ~d" res-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    (gk:draw-text (format nil " 知力ボーナス : ~d" int-bonus) (gk:vec2 bonus-posx (Decf bonus-posy line-width))
		  :fill-color *white* :font font)
    ;;技能
    (loop :for job :in *all-job-list*
	  :for posx = job-posx
	  :for posy :from job-posy :downto 0 :by line-width
	  :do (let* ((job-data (getf *all-job-tag-and-data-list* job))
		     (name (getf job-data :name))
		     (level (getf job-level-list job)))
		(gk:draw-text (format nil "~a" name) (gk:vec2 posx posy)
			      :fill-color *white* :font font)
		(gk:draw-text (format nil "Lv ~d" level) (gk:vec2 (+ posx 220) posy)
			      :fill-color *white* :font font)))
    ;;特技
    (gk:draw-text "初期特技" (gk:vec2 skill-posx skill-posy) :fill-color *white* :font font)
    (loop :for skill-tag :in (append action-skill declare-skill passive-skill)
	  :for posx :from skill-posx :to 900 :by 400
	  :for posy = (- skill-posy line-width)
	  :do (let* ((skill-data (getf *all-skill-tag-and-data* skill-tag))
		     (name (getf skill-data :name)))
		(gk:draw-text (format nil "~a" name) (gk:vec2 posx posy)
			      :fill-color *white* :font font)))))

;;ユニットの名前ボタンとステータス表示用
(defun show-unit-btn-and-show-status (btn)
  (with-slots (unit pos string w h font color) btn
    (let* ((adjust (gk:vec2 8 10))
	   (rounding 10)
	   (thickness 3))
      (collide-mouse btn adjust rounding thickness
	(draw-unit-status unit 300 700 690 560 970 660 300 200 45 *font40*)))))

;;ランダムユニット
(defmethod draw-recruit-btn ((btn recruit-random-unit-btn))
  (show-unit-btn-and-show-status btn))

;;戻るボタン
(defmethod draw-recruit-btn ((btn end-recruit-btn))
  (draw-text-btn-with-waku btn 3 (gk:vec2 4 8) :rounding 10))

;;仲間募集
(defun draw-recruit ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-recruit-btn btn))))


;;ユニットステータス
(defmethod draw-show-status-btn ((btn show-unit-status-btn))
  (show-unit-btn-and-show-status btn))
;;戻るボタン
(defmethod draw-show-status-btn ((btn end-recruit-btn))
  (draw-text-btn-with-waku btn 3 (gk:vec2 4 8) :rounding 10))

;;仲間ステータス確認
(defun draw-show-status ()
  (with-slots (btn-list) *game*
    (gk:draw-text "ステータス" (gk:vec2 500 760) :fill-color *white* :font *font64*)
    (gk:draw-text "※ユニットを右クリックでパーティから外せます" (gk:vec2 200 30) :fill-color (gk:vec4 1 1 0 1) :font *font32*)
    (loop :for btn :in btn-list
	  :do (draw-show-status-btn btn))))

(defun draw-town ()
  (with-slots (action-state) *game*
    (case action-state
      (:town-menu (draw-town-menu))
      (:shop (draw-shop-item))
      (:recruit (draw-recruit))
      (:show-status (draw-show-status)))))

;;---------------------------------------------------------------------------------------------------------
;;レベルアップ
(defun draw-lvup-description ()
  (with-slots (selected-unit) *game*
    (with-slots (str dex agi vit res int name id) selected-unit
      (gk:draw-text "レベルアップ！" (gk:vec2 360 730) :fill-color (gk:vec4 0 0.5 1 1) :font *font128*)
      (gk:draw-text "上昇させたいステータスを選んでください" (gk:vec2 160 670) :fill-color (gk:vec4 0.3 0.5 0.7 1) :font *font64*)
      ;;現在のステータス
      (gk:draw-rect (gk:vec2 300 60) 570 400 :stroke-paint *white* :thickness 3 :rounding 10)
      (let ((posx 320)
	    (posy 400)
	    (des-posx 540)
	    (line-width 50)
	    (font *font48*))
	(gk:draw-text (format nil "名前:~a ~a" name (getf *show-class* id)) (gk:vec2 posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil " 筋力 : ~2d" str) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "物理攻撃力に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "器用度: ~2d" dex) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "命中率に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "敏捷度: ~2d" agi) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "回避率に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil " 知力 : ~2d" int) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "魔法攻撃力に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "生命力: ~2d" vit) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "HPに影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "精神力: ~2d" res) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-text "MPに影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
      ))))

(defun draw-lvup-btn ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-btn-with-waku btn 3 (gk:vec2 10 13) :rounding 9))))


(defun draw-lvup ()
  (draw-lvup-description)
  (draw-lvup-btn))
;;---------------------------------------------------------------------------------------------------------
;;初期パーティ作成画面

(defun draw-init-job-btn ()
  (with-slots (btn-list temp-init-party) *game*
    (loop :for btn :in (append btn-list temp-init-party)
	  :do (if (button/box? btn)
		  (draw-text-btn-with-waku btn 4 (gk:vec2 8 7) :rounding 10)
		  (draw-text-button-no-waku btn (gk:vec2 10 8))))))

(defun draw-create-init-party-supple ()
  (gk:draw-text "初期ジョブ" (gk:vec2 160 680) :fill-color *white* :font *font64*)
  (gk:draw-rect (gk:vec2 130 200) 320 460 :stroke-paint *white* :thickness 4 :rounding 10)
  (gk:draw-text "初期パーティ" (gk:vec2 560 680) :fill-color *white* :font *font64*)
  (gk:draw-text "※5人まで" (gk:vec2 860 680) :fill-color (gk:vec4 1 0 1 1) :font *font48*)
  (gk:draw-rect (gk:vec2 530 200) 320 460 :stroke-paint *white* :thickness 4 :rounding 10)
  )

(defun draw-create-init-party ()
  (draw-init-job-btn)
  (draw-create-init-party-supple))
;;---------------------------------------------------------------------------------------------------------
;;種族選択画面

(defun draw-creating ()
  (gk:draw-text "主人公キャラ作成中" (gk:vec2 200 730) :fill-color (gk:vec4 0.3 0.6 0.8 1) :font *font128*))

(defun draw-select-race ()
  (with-slots (btn-list) *game*
    (draw-creating)
    (gk:draw-text "種族を選んでください" (gk:vec2 160 660) :fill-color (gk:vec4 0.8 0.6 0.4 1) :font *font64*)
    (loop :for btn :in btn-list
	  :do (draw-text-btn-with-waku btn 4 (gk:vec2 8 14) :rounding 10))))
;;---------------------------------------------------------------------------------------------------------
;;技能選択画面
;;取得技能ボタン
(defmethod draw-job-btn ((btn job-btn))
  (with-slots (job pos string w h font color) btn
    (let* ((adjust (gk:vec2 8 14))
	   (data (getf *init-job-data-list* job))
	   (tec (getf data :tec))
	   (con (getf data :con))
	   (mnd (getf data :mnd))
	   (rounding 10)
	   (thickness 3)
	   (exp-point (getf data :exp-point)))
      ;; (if (collide-p *mouse* btn)
      ;; 	  (progn
      ;; 	    (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding rounding)
      ;; 	    (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1))
      (collide-mouse btn adjust rounding thickness
	    (gk:draw-text "基礎能力値" (gk:vec2 790 600) :fill-color *white* :font *font64*)
	    (gk:draw-text (format nil "技:~d 体:~d 心:~d" tec con mnd)
			  (gk:vec2 740 540) :fill-color *white* :font *font64*)
	    (gk:draw-text (format nil "初期経験点:~d" exp-point) (gk:vec2 740 480)
			  :fill-color *white* :font *font64*)))))
	  ;; (progn
	  ;;   (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
	  ;; 			  :thickness thickness :rounding rounding)
     	  ;;   (gk:draw-text string (gk:add pos adjust) :font font :fill-color color))))))

;;戻るボタン
(defmethod draw-job-btn ((btn back-select-race-btn))
  (draw-text-btn-with-waku btn 3 (gk:vec2 3 8) :rounding 10))

(defun draw-select-job ()
  (with-slots (btn-list selected-unit) *game*
    (draw-creating)
    (gk:draw-text "初期技能を選んでください" (gk:vec2 130 660) :fill-color (gk:vec4 0.8 0.6 0.4 1) :font *font64*)
    (gk:draw-text (format nil "種族：~a" (race selected-unit)) (gk:vec2 730 660) :fill-color (gk:vec4 1 0.1 0.7 1) :font *font64*)
    (gk:draw-text "技：命中や回避に影響" (gk:vec2 700 350) :fill-color *white* :font *font48*)
    (gk:draw-text "体：物理ダメージやHPに影響" (gk:vec2 700 290) :fill-color *white* :font *font48*)
    (gk:draw-text "心：魔法ダメージやMPに影響" (gk:vec2 700 230) :fill-color *white* :font *font48*)
    (gk:draw-text "経験点：消費して技能を取得できる" (gk:vec2 670 180) :fill-color *white* :font *font48*)
    (loop :for btn :in btn-list
	  :do (draw-job-btn btn))))

;;---------------------------------------------------------------------------------------------------------
;;初期能力値決定画面
(defun draw-init-ability ()
  (with-slots (selected-unit btn-list) *game*
    (with-slots (str-dice dex-dice vit-dice agi-dice int-dice res-dice num) *ability-dice*
    (with-slots (con mnd tec) selected-unit
      (draw-creating)
      (loop :for btn :in btn-list
	    :do (draw-text-btn-with-waku btn 3 (gk:vec2 4 9) :rounding 10))
      (gk:draw-text "初期能力値を決定してください" (gk:vec2 160 660) :fill-color (gk:vec4 0.8 0.6 0.4 1) :font *font64*)
      (gk:draw-text (format nil "基礎能力値 技:~d 体:~d 心:~d" tec con mnd)
		    (gk:vec2 160 610) :fill-color (gk:vec4 1 0.3 0.4 1) :font *font48*)
      (gk:draw-text (format nil "あと~d回" num) (gk:vec2 650 50) :fill-color (gk:vec4 1 1 0 1) :font *font64*)
      (let ((posx 120)
	    (posy 560)
	    (dice-posx 385)
	    (des-posx 540)
	    (rect-x 360)
	    (line-width 58)
	    (font *font48*))
	(gk:draw-text (format nil "　筋力 = ~2d + " con) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" str-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "物理攻撃力に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "器用度 = ~2d + " tec) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" dex-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "命中率に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "敏捷度 = ~2d + " tec) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" agi-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "回避率に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "　知力 = ~2d + " mnd) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" int-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "魔法攻撃力に影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "生命力 = ~2d + " con) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" vit-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "HPに影響" (gk:vec2 des-posx posy) :fill-color *white* :font font)
	(gk:draw-text (format nil "精神力 = ~2d + " mnd) (gk:vec2 posx (decf posy line-width)) :fill-color *white* :font *font48*)
	(gk:draw-rect (gk:vec2 rect-x (- posy 15)) 80 50 :stroke-paint *white* :thickness 3)
	(gk:draw-text (format nil "~2d" res-dice) (gk:vec2 dice-posx posy) :fill-color (gk:vec4 0 1 0 1) :font *font48*)
	(gk:draw-text "MPに影響" (gk:vec2 des-posx posy) :fill-color *white* :font font))))))


;;---------------------------------------------------------------------------------------------------------
;;初期技能レベルアップ
(defmethod draw-job-levelup-btn ((btn job-level-up-btn))
  (with-slots (selected-unit) *game*
    (with-slots (job-level-list) selected-unit
      (draw-text-btn-with-waku btn 3 (gk:vec2 5 10) :rounding 10)
      (with-slots (job pos) btn
	(let* ((now-level (getf job-level-list job))
	       (job-data (getf *all-job-tag-and-data-list* job))
	       (exp-table (getf job-data :exp-point-table))
	       (required-point (getf exp-table (1+ now-level))))
	  (gk:draw-text (format nil "Lv ~d → Lv ~d" now-level (1+ now-level))
			(gk:add pos (gk:vec2 270 10)) :fill-color (gk:vec4 1 0.4 0 1) :font *font48*)
	  (gk:draw-text (format nil "必要経験点 ~d" required-point)
			(gk:add pos (gk:vec2 500 10)) :fill-color (gk:vec4 0 0.4 1 1) :font *font48*))))))

(defmethod draw-job-levelup-btn ((btn back-ability-dice-btn))
  (draw-text-btn-with-waku btn 3 (gk:vec2 5 10) :rounding 10))


(defun draw-init-job-level-up ()
  (with-slots (btn-list selected-unit) *game*
    (with-slots (exp-point job-level-list) selected-unit
      (draw-creating)
      (gk:draw-text "成長させたい技能を選んでください (Lv2まで)" (gk:vec2 70 670)
		    :fill-color (gk:vec4 0.2 0.6 0.2 1) :font *font48*)
      (gk:draw-text (format nil "残り経験点:~d" exp-point) (gk:vec2 890 670)
		    :fill-color (gk:vec4 0.4 0.6 1 1) :font *font48*)
      (gk:draw-rect (gk:vec2 875 658) 290 50 :stroke-paint *white* :rounding 10 :thickness 3)
      (loop :for btn :in btn-list
	    :do (draw-job-levelup-btn btn)))))

;;---------------------------------------------------------------------------------------------------------


;;初期スキル選択画面
(defun draw-select-init-skill ()
  (with-slots (btn-list) *game*
    (gk:draw-text "初期特技を選んでください" (gk:vec2 300 760) :fill-color (gk:vec4 1 0 1 1) :font *font64*)
    (loop :for btn :in btn-list
	  :do (with-slots (pos w h description font color string) btn
	      (let ((adjust (gk:vec2 6 9))
		    (rounding 10)
		    (thickness 3))
		(collide-mouse btn adjust 10 3 (when description
						 (gk:draw-text description (gk:vec2 60 120) :fill-color (gk:vec4 0 1 0 1) :font *font32*))))))))
		;; (if (collide-p *mouse* btn)
     		;;     (progn
		;;       (gk:draw-rect pos w h :fill-paint (gk:vec4 1 1 1 1) :rounding rounding)
      		;;       (gk:draw-text string (gk:add pos adjust) :font font :fill-color (gk:vec4 0 0 0 1))
		;;       (when description
		;; 	(gk:draw-text (button/description btn) (gk:vec2 60 120) :fill-color (gk:vec4 0 1 0 1) :font *font32*)))
		;;     (progn
		;;       (gk:draw-rect pos w h :stroke-paint (gk:vec4 1 1 1 1)
		;; 			    :thickness thickness :rounding rounding)
     		;;       (gk:draw-text string (gk:add pos adjust) :font font :fill-color color))))))))

;;---------------------------------------------------------------------------------------------------------

;;初期作成ユニットステータス表示
(defun draw-init-player-unit-status ()
  (with-slots (btn-list selected-unit) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-btn-with-waku btn 3 (gk:vec2 6 9) :rounding 10))
    (with-slots (hp maxhp mp maxmp str dex agi vit res int dex-bonus vit-bonus res-bonus
		 int-bonus str-bonus agi-bonus magic-power job-level-list name
		 passive-skill action-skill declare-skill race)  selected-unit
      (gk:draw-text "主人公キャラステータス" (gk:vec2 300 750) :fill-color (gk:vec4 1 0 1 1) :font *font64*)
      (let ((status-posx 100)
	    (status-posy 700)
	    (bonus-posx 500)
	    (bonus-posy 560))
	(draw-unit-status selected-unit 100 700 500 560 880 660 150 180 45 *font48*)
	(gk:draw-rect (gk:vec2 (- status-posx 10) (- status-posy 470)) 380 470
		      :stroke-paint *white* :thickness 3 :rounding 10)
	;;種族
	(gk:draw-rect (gk:vec2 (- bonus-posx 10) (- bonus-posy -40)) 320 100 :stroke-paint *white* :thickness 3 :rounding 10)
	;;ボーナス
	(gk:draw-rect (gk:vec2 (- bonus-posx 10) (- bonus-posy 290)) 320 290 :stroke-paint *white* :thickness 3 :rounding 10)
	;;技能
	(gk:draw-rect (gk:vec2 870 200) 320 500 :stroke-paint *white* :thickness 3 :rounding 10)
	))))
