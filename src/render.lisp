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
  (with-slots (pos string font color w h equiped-unit new) btn
    (if (collide-p *mouse* btn)
     	(progn
          (gk:draw-rect (gk:subt pos (gk:vec2 3 7)) w h :fill-paint (gk:vec4 1 1 1 1))
      	  (gk:draw-text string pos :font font :fill-color (gk:vec4 0 0 0 1)))
     	(gk:draw-text string pos :font font :fill-color color))
    (when equiped-unit
      (gk:draw-text "E" (gk:subt pos (gk:vec2 20)) :fill-color (gk:vec4 1 0 1 1) :font font))))

;;枠ありボタン bg=黒背景
(defun draw-text-btn-with-waku (btn thickness &key (bg nil))
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
	  :do (with-slots (pos dmg-num color) dmg
		(draw-stroke-text (format nil "~a" dmg-num) (gk:x pos) (gk:y pos) (gk:vec4 0 0 0 1) color *font28*)))))

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
;;ユニット情報
(defun draw-unit-info (e)
  (with-slots (hp maxhp str agi vit res int name state) e
    (with-slots (x) *mouse*
      (let* ((rect-w (max 98 (* (length name) 22)))
	     (rect-h 170)
	     (rect-margin 5)
	     (rect-pos-y (- (/ *window-h* 1.5) rect-h rect-margin))
	     (rect-pos (if (>= *window-center* x)
			     (gk:vec2 (- (/ *window-w* 1.5) rect-w rect-margin) rect-pos-y)
			     (gk:vec2 rect-margin rect-pos-y)))
	     (text-x (if (>= *window-center* x)
			 (+ (- (/ *window-w* 1.5) rect-w) 1)
			 10))
	     (act-str nil)
	     (act-color nil))
	(cond ((eq state :inaction)
	       (setf act-str "未行動"
		     act-color (gk:vec4 0 1 0 1)))
	      ((eq state :end)
	       (setf act-str "行動済"
		     act-color (gk:vec4 1 1 0 1)))
	      ((eq state :atk)
	       (setf act-str "攻撃中"
		     act-color (gk:vec4 0 1 1 1))))
	(gk:draw-rect rect-pos rect-w rect-h :fill-paint (gk:vec4 0 0 0 1)
					     :stroke-paint (gk:vec4 1 1 1 1) :thickness 5)
	(gk:draw-text (format nil "名前:~a" name)  (gk:vec2 text-x (+ rect-pos-y 152)) :font *font18* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil " HP :~a/~a" hp maxhp)  (gk:vec2 text-x (+ rect-pos-y 130)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "物攻: ~a" str)   (gk:vec2 text-x (+ rect-pos-y 110)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "物防: ~a" vit) (gk:vec2 text-x (+ rect-pos-y 90)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "速さ: ~a" agi)  (gk:vec2 text-x (+ rect-pos-y 70)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "魔攻: ~a" int)   (gk:vec2 text-x (+ rect-pos-y 50)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "魔防: ~a" res) (gk:vec2 text-x (+ rect-pos-y 30)) :font *font24* :fill-color (gk:vec4 1 1 1 1))
	(gk:draw-text (format nil "~a" act-str) (gk:vec2 text-x (+ rect-pos-y 10)) :font *font24* :fill-color act-color)))))


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
  ;;(render-background )
  ;;(render-bgmonoff-button)
  ;;(render-weapon-list)
  (with-slots (item-page item) *game*
    ;; (text-out *hmemdc* (format nil "~aの現在の武器" (name selected)) 270 5)
    ;; (when (buki selected)
    ;;   (let ((atk (cond
    ;; 		   ((eq (categoly (buki selected)) :wand) "魔攻")
    ;; 		   ((eq (categoly (buki selected)) :staff) "回復力")
    ;; 		   (t "攻撃力"))))
    ;;   (text-out *hmemdc* (format nil "名前 : ~a" (name (buki selected))) 270 40)
    ;;   (text-out *hmemdc* (format nil "~a : ~a" atk (damage (buki selected))) 270 70)
    ;;   (text-out *hmemdc* (format nil "命中 : ~a" (hit (buki selected))) 270 100)
    ;;   (text-out *hmemdc* (format nil "射程 : ~a～~a" (rangemin (buki selected))
    ;; 				 (rangemax (buki selected))) 270 130)
    ;;   (text-out *hmemdc* (format nil "会心 : ~a%" (critical (buki selected))) 270 160)))

    ;; (text-out *hmemdc* (format nil "~aの現在の防具" (name selected)) 570 5)
    ;; (when (armor selected)
    ;;   (text-out *hmemdc* (format nil "名前 : ~a" (name (armor selected))) 570 40)
    ;;   (text-out *hmemdc* (format nil "防御力 : ~a" (def (armor selected))) 570 70)
    ;;   (text-out *hmemdc* (format nil "ブロック率 : ~a%" (blk (armor selected))) 570 100))
    ;; (render-selecting-item selected)
    ;; (select-object *hogememdc* *waku-img*)
    ;; (new-trans-blt 260 35 0 0 128 128 250 160)
    ;; (new-trans-blt 560 35 0 0 128 128 250 150)
    ;; (new-trans-blt 260 280 0 0 128 128 250 220)
    ;; (select-object *hmemdc* *font20*)
    (let ((item-page-max (1+ (floor  (length item) *item-show-max*))))
      (gk:draw-text "持ち物" (gk:vec2 99 790) :fill-color (gk:vec4 1 1 1 1)  :font *font32*)
      (gk:draw-text (format nil "~d / ~d" (1+ item-page) item-page-max) (gk:vec2 230 790)
		    :fill-color (gk:vec4 1 1 1 1) :font *font32*)
      (gk:draw-rect (gk:vec2 20 130) 300 650 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
      (gk:draw-text "※捨てる:アイテムにカーソルを合わせて右クリック" (gk:vec2 20 50) :fill-color (gk:vec4 1 1 1 1)  :font *font32*)
      (gk:draw-text "※装備中のアイテムは捨てることができません" (gk:vec2  20 20) :fill-color (gk:vec4 1 1 1 1) :font *font32*))))

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
	))))

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

;;カーソルと重なってるアイテム情報
(defmethod draw-item-info-with-cursor ((item weapondesc))
  (with-slots (selected-unit) *game*
    (when selected-unit
      (with-slots (canequip weapon str shield) selected-unit
	(with-slots (name damage critical hit rangemin rangemax category equiped required-str hand) item
	  ;;装備中の武器
	  (when weapon
	    (with-slots (name damage-table critical hit rangemin rangemax required-str hand) weapon
	      (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 390 720) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "威力 : ~a" damage) (gk:vec2 390 690) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "命中 : ~a" hit) (gk:vec2 390 660) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "必殺 : ~a％" (critical-rate critical)) (gk:vec2 390 630) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "射程 : ~a～~a" rangemin rangemax) (gk:vec2 390 600) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 390 570) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text (format nil "持ち方 : ~a" (draw-weapon-holding hand shield)) (gk:vec2 390 540) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	      (gk:draw-text "装備中の武器" (gk:vec2 430 755) :fill-color (gk:vec4 0.5 0.4 1 1) :font *font32*)
	      (gk:draw-rect (gk:vec2 380 525) 330 260 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	      ))
	  ;;選択中の武器
	  (gk:draw-rect (gk:vec2 380 185) 330 300 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
	  (gk:draw-text "選択中の武器" (gk:vec2 430 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
	  (cond
	    ((find category canequip)
	     (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 390 420) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "威力 : ~a" damage) (gk:vec2 390 390) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "命中 : ~a" hit) (gk:vec2 390 360) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "必殺 : ~a％" (critical-rate critical)) (gk:vec2 390 330) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "射程 : ~a～~a" rangemin rangemax) (gk:vec2 390 300) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "必要筋力 : ~a" required-str) (gk:vec2 390 270) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "持ち方 : ~a" (draw-weapon-holding hand shield)) (gk:vec2 390 240) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (gk:draw-text (format nil "装備中 : ~a" equiped) (gk:vec2 390 210) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
	     (draw-diff-item-ability damage critical hit selected-unit)
	     (when (> required-str str)
	       (gk:draw-text "筋力不足！" (gk:vec2 550 270) :fill-color (gk:vec4 1 0 0 1) :font *font32*)))
	    (t
	     (gk:draw-text "装備できません" (gk:vec2 420 400) :fill-color (gk:vec4 1 1 1 1) :font *font32*))))))))

(defmethod draw-item-info-with-cursor ((item armordesc))
  (with-slots (name avoid def) item
    (gk:draw-rect (gk:vec2 680 265) 280 220 :stroke-paint (gk:vec4 1 1 1 1) :thickness 2)
    (gk:draw-text "選択中の防具" (gk:vec2 730 455) :fill-color (gk:vec4 0 1 1 1) :font *font32*)
    (gk:draw-text (format nil "名前 : ~a" name) (gk:vec2 690 420) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "防御 : ~a" def) (gk:vec2 690 390) :fill-color (gk:vec4 1 1 1 1) :font *font32*)
    (gk:draw-text (format nil "回避 : ~a" avoid) (gk:vec2 690 360) :fill-color (gk:vec4 1 1 1 1) :font *font32*)))




;;装備メニュー画面のアイテムボタン表示
(defun draw-item-btn-list ()
  (with-slots (btn-list) *game*
    (loop :for btn :in btn-list
	  :do (draw-text-button-no-waku btn)
	      (when (and  (collide-p *mouse* btn)
			  (eq 'equip-item-btn (type-of btn)))
		(draw-item-info-with-cursor (item btn))))))

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
  (with-slots (party world-pos scroll) *game*
    (let ((unit (car party)))
      (with-slots (img-id origin-w origin-h origin) unit
	(gk:draw-image (gk:subt world-pos (gk:vec2 16 16) scroll) img-id  :origin origin :width origin-w :height origin-h)))))

(defun draw-world ()
  (draw-world-map)
  (draw-world-unit))
;;---------------------------------------------------------------------------------------------------------

(Defun draw-town-menu ()
  (with-slots (btn-list) *game*
    (draw-btn-list)))

;;---------------------------------------------------------------------------------------------------------
;; ;;アイテムリストからマウス位置のアイテムを帰す
;; (defun get-item-mouse-pos ()
;;   (with-slots (x y) *mouse*
;;     (cond
;;       ((and (>= *item-x2* x *item-x1*)
;; 	    (>= *item1-y2* y *item1-y1*))
;;        (nth (+ 0 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item2-y2* y *item2-y1*))
;;        (nth (+ 1 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item3-y2* y *item3-y1*))
;;        (nth (+ 2 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item4-y2* y *item4-y1*))
;;        (nth (+ 3 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item5-y2* y *item5-y1*))
;;        (nth (+ 4 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item6-y2* y *item6-y1*))
;;        (nth (+ 5 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item7-y2* y *item7-y1*))
;;        (nth (+ 6 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item8-y2* y *item8-y1*))
;;        (nth (+ 7 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item9-y2* y *item9-y1*))
;;        (nth (+ 8 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item10-y2* y *item10-y1*))
;;        (nth (+ 9 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item11-y2* y *item11-y1*))
;;        (nth (+ 10 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item12-y2* y *item12-y1*))
;;        (nth (+ 11 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item13-y2* y *item13-y1*))
;;        (nth (+ 12 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       ((and
;; 	    (>= *item-x2* x *item-x1*)
;; 	    (>= *item14-y2* y *item14-y1*))
;;        (nth (+ 13 (* (item-page *p*) *item-show-max*)) (item *p*)))
;;       )))






;; (defun render-unit (unit)
;;   (when (not (eq (state unit) :dead)) ;;死んでなかったら表示
;;     (new-trans-blt (posx unit) (posy unit) 0 (* *obj-h* (img-h unit))
;; 		   (moto-w unit) (moto-h unit) (w unit) (h unit))))




;; ;;敵表示
;; (defun render-enemies ()
;;   (select-object *hogememdc* *job-monsters*)
;;   (loop for e in (enemies *donjon*)
;;      do (Render-unit e)))




;; ;;現在の方向
;; (defun p-dir-num ()
;;   (case (dir *p*)
;;     (:up +up+)
;;     (:down +down+)
;;     (:left +left+)
;;     (:right +right+)))








;; ;;鍵とか描画
;; (defun render-items ()
;;   (loop for obj in (chest *donjon*)
;;      do (render-objs-img (x obj) (y obj) (img obj))))

;; ;;武器経験値表示
;; (defun render-bugu-exp (exp num)
;;   (let* ((len (floor (* (/ exp 100) *bukiexpbar-max*)))
;; 	 (left *map-w*)
;; 	 (bottom num)
;; 	 (top (- bottom 15))
;; 	 (hp-w (+ left len)))
;;     ;;残りHP
;;     (select-object *hmemdc* (aref *brush* +green+))
;;     (rectangle *hmemdc* left top hp-w bottom)
;;     ;;減ったHP
;;     (select-object *hmemdc* (aref *brush* +red+))
;;     (rectangle *hmemdc* hp-w top (+ hp-w (- *bukiexpbar-max* len)) bottom)))








;; (defun create-render-button (x1 x2 y1 y2 str strx stry &key (font *font40*))
;;   (select-object *hmemdc* font)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (if (and (>= x2 (x *mouse*) x1)
;; 	   (>= y2 (y *mouse*) y1))
;;       (progn (select-object *hmemdc* (get-stock-object :white-brush))
;; 	     (rectangle *hmemdc* x1 y1 x2 y2)
;; 	     (set-text-color *hmemdc* (encode-rgb 0 0 0)))
;;       (progn (select-object *hogememdc* *waku-img*)
;; 	     (new-trans-blt x1 y1 0 0 128 128 (- x2 x1) (- y2 y1))
;; 	     (set-text-color *hmemdc* (encode-rgb 255 255 255))))
;;   (text-out *hmemdc* (format nil "~a" str) strx stry))

;; (defun create-render-button-no-waku (x1 x2 y1 y2 str strx stry &key (font *font40*))
;;   (select-object *hmemdc* font)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (if (and (>= x2 (x *mouse*) x1)
;; 	   (>= y2 (y *mouse*) y1))
;;       (progn (select-object *hmemdc* (get-stock-object :white-brush))
;; 	     (rectangle *hmemdc* x1 y1 x2 y2)
;; 	     (set-text-color *hmemdc* (encode-rgb 0 0 0)))
;;       (progn
;; 	     (set-text-color *hmemdc* (encode-rgb 255 255 255))))
;;   (text-out *hmemdc* (format nil "~a" str) strx stry))

;; ;;バックグラウンド
;; (defun render-background ()
;;   (select-object *hmemdc* (get-stock-object :black-brush))
;;   (rectangle *hmemdc* 0 0 *change-screen-w* *change-screen-h*))


;; ;;BGMoffボタン
;; (defun render-bgmonoff-button ()
;;   (let ((str (if (eq (bgm *p*) :on) "BGM OFF" "BGM ON")))
;;   (create-render-button *bgmoff-x1* *bgmoff-x2* *bgmoff-y1* *bgmoff-y2*
;; 			str (+ *bgmoff-x1* 3) *bgmoff-y1* :font *font20*)))


;; ;;持っている武器を表示 TODO
;; (Defun render-weapon-list ()
;;   (select-object *hmemdc* *font30*)
;;   (text-out *hmemdc* "所持品リスト" 60 5)
;;   (select-object *hogememdc* *waku-img*)
;;   (new-trans-blt 30 35 0 0 128 128 200 450)
;;   (create-render-button *item-next-btn-x1* *item-next-btn-x2* *item-next-btn-y1* *item-next-btn-y2*
;; 			"次へ" *item-next-btn-x1* *item-next-btn-y1* :font *font30*)
;;   (create-render-button *item-prev-btn-x1* *item-prev-btn-x2* *item-prev-btn-y1* *item-prev-btn-y2*
;; 			"前へ" *item-prev-btn-x1* *item-prev-btn-y1* :font *font30*)
;;   (create-render-button *item-decision-x1* *item-decision-x2* *item-decision-y1*
;; 			*item-decision-y2* "完了" (+ *item-decision-x1* 3)
;; 			*item-decision-y1* :font *font30*)
;;   (loop ;;:for buki :in (item *p*)
;;      :for x = 50
;;      :for y :from 45 :by 30
;;      :for b :from 0 :below *item-show-max*
;;      ;;:repeat 10
;;      :do (let* ((num (+ b (* (item-page *p*) *item-show-max*))))
;; 	   (if (>= num (length (item *P*)))
;; 	       (return)
;; 	       (let ((buki (nth num (item *P*))))
;; 		 (cond
;; 		   ((new buki)
;; 		    (select-object *hmemdc* *font2*)
;; 		    (set-text-color *hmemdc* (encode-rgb 255 255 255))
;; 		    (text-out *hmemdc* "N" 35 y)
;; 		    (text-out *hmemdc* "e" 40 (+ y 8))
;; 		    (text-out *hmemdc* "w" 45 (+ y 16)))
;; 		   ((equiped buki)
;; 		    (select-object *hmemdc* *font20*)
;; 		    (set-text-color *hmemdc* (encode-rgb 255 255 255))
;; 		    (text-out *hmemdc* "E" 34 y)))
;; 		 (create-render-button-no-waku x (+ x 170) y (+ y 25)
;; 					       (name buki) (+ x 4) y :font *font30*))))))


;; ;;マウス位置にあるアイテムデータ表示
;; (defun render-selecting-item (unit)
;;   (let* ((item (get-item-mouse-pos ))
;; 	 (y 255))
;;     (text-out *hmemdc* "選択中のアイテム" 270 250)
;;     (macrolet ((hoge (n)
;; 		 `(incf ,n 30)))
;;     (cond
;;       ((and item
;; 	    (eq (categoly item) :armor))
;;        (let ((canequip (if (find (categoly item) (get-job-data (job unit) :canequip) :test #'equal)
;; 			   "可能" "不可")))
;; 	 (text-out *hmemdc* (format nil "装備 : ~a" canequip) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "名前 : ~a" (name item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "防御力 : ~a" (def item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "ブロック率 : ~a%" (blk item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "装備者 : ~a" (equiped item)) 270 (hoge y))))
;;       (item
;;        (let ((atk (cond
;; 		    ((eq (categoly item) :wand) "魔攻")
;; 		    ((eq (categoly item) :staff) "回復力")
;; 		    (t "攻撃力")))
;; 	     (canequip (if (find (categoly item) (get-job-data (job unit) :canequip) :test #'equal)
;; 			   "可能" "不可")))
;; 	 (text-out *hmemdc* (format nil "装備 : ~a" canequip) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "名前 : ~a" (name item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "~a : ~a" atk (damage item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "命中 : ~a" (hit item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "射程 : ~a～~a" (rangemin item)
;; 				    (rangemax item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "会心 : ~a%" (critical item)) 270 (hoge y))
;; 	 (text-out *hmemdc* (format nil "装備者 : ~a" (equiped item)) 270 (hoge y))))))))



;; ;;武器変更画面
;; (defun render-weapon-change-gamen ()
;;   (render-background )
;;   (render-bgmonoff-button)
;;   (render-weapon-list)
;;   (with-slots (selected) *mouse*
;;     (select-object *hmemdc* *font30*)
;;     (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;     ;;(set-bk-mode *hmemdc* :transparent)
;;     (text-out *hmemdc* (format nil "~aの現在の武器" (name selected)) 270 5)
;;     (when (buki selected)
;;       (let ((atk (cond
;; 		   ((eq (categoly (buki selected)) :wand) "魔攻")
;; 		   ((eq (categoly (buki selected)) :staff) "回復力")
;; 		   (t "攻撃力"))))
;;       (text-out *hmemdc* (format nil "名前 : ~a" (name (buki selected))) 270 40)
;;       (text-out *hmemdc* (format nil "~a : ~a" atk (damage (buki selected))) 270 70)
;;       (text-out *hmemdc* (format nil "命中 : ~a" (hit (buki selected))) 270 100)
;;       (text-out *hmemdc* (format nil "射程 : ~a～~a" (rangemin (buki selected))
;; 				 (rangemax (buki selected))) 270 130)
;;       (text-out *hmemdc* (format nil "会心 : ~a%" (critical (buki selected))) 270 160)))

;;     (text-out *hmemdc* (format nil "~aの現在の防具" (name selected)) 570 5)
;;     (when (armor selected)
;;       (text-out *hmemdc* (format nil "名前 : ~a" (name (armor selected))) 570 40)
;;       (text-out *hmemdc* (format nil "防御力 : ~a" (def (armor selected))) 570 70)
;;       (text-out *hmemdc* (format nil "ブロック率 : ~a%" (blk (armor selected))) 570 100))
;;     (render-selecting-item selected)
;;     (select-object *hogememdc* *waku-img*)
;;     (new-trans-blt 260 35 0 0 128 128 250 160)
;;     (new-trans-blt 560 35 0 0 128 128 250 150)
;;     (new-trans-blt 260 280 0 0 128 128 250 220)
;;     (select-object *hmemdc* *font20*)
;;     (text-out *hmemdc* "※捨てる:アイテムにカーソルを合わせて右クリック" 270 500)
;;     (text-out *hmemdc* "装備中のアイテムは捨てることができません" 280 525)))


;; ;;s1のs2までの割合をバーで表示
;; (defun render-bar (left top bot s1 s2 barmax)
;;   (let* ((len (floor (* (/ s1 s2) barmax)))
;; 	 (w (+ left len)))
;;     ;;残りHP
;;     (select-object *hmemdc* (aref *brush* +green+))
;;     (rectangle *hmemdc* left top w bot)
;;     ;;減ったHP
;;     (select-object *hmemdc* (aref *brush* +red+))
;;     (rectangle *hmemdc* w top (+ w (- barmax len)) bot)))


;; ;;キャラのステータス表示
;; (defun render-chara-status (p x)
;;   (with-slots (buki armor) p
;;   (let* ((num 50))
;; 	 ;;(cell (aref *celldescs* (aref (field *donjon*) (y p) (x p)))))
;;     (macrolet ((hoge (n)
;; 		 `(incf ,n 25)))
;;       (let ((makou  0)
;; 	    (kou 0))
;; 	(if (or (eq (categoly buki) :wand)
;; 		(eq (categoly buki) :stagg))
;; 	    (setf makou (damage buki))
;; 	    (setf kou (damage buki)))
;; 	(text-out *hmemdc* (format nil "~a" (name p)) x num)
;; 	(text-out *hmemdc* (format nil "Lv:~2d" (level p)) x (hoge num))
;; 	(text-out *hmemdc* (format nil "HP:~2d/~2d" (hp p) (maxhp p)) x (hoge num))
;; 	(text-out *hmemdc* (format nil " 攻  :~2d" (+ (str p) kou)) x (hoge num))
;; 	(text-out *hmemdc* (format nil " 防  :~2d" (+ (vit p) (def armor))) x (hoge num))
;; 	(text-out *hmemdc* (format nil " 速  :~2d" (agi p)) x (hoge num))
;; 	(text-out *hmemdc* (format nil "魔攻:~2d" (+ (int p) makou)) x (hoge num))
;; 	(text-out *hmemdc* (format nil "魔防:~2d" (res p)) x (hoge num))
;; 	(text-out *hmemdc* (format nil "exp:") x (hoge num))
;; 	(render-bar (+ x 50) (+ num 27) (+ num 6) (expe p) (lvup-exp p) 100)
;; 	(when (buki p)
;; 	  (text-out *hmemdc* (format nil "武器：~a" (name (buki p))) x (hoge num)))
;; 	(text-out *hmemdc* (format nil "防具：~a" (name armor)) x (hoge num))
;; 	(text-out *hmemdc* (format nil "~a" (if (eq (state p) :action) "行動可" "行動済み")) x (hoge num)))))))
;; 	;;(text-out *hmemdc* (format nil "~a" (cell p)) x (hoge num)))))))
;; 	;;(text-out *hmemdc* (format nil "x:~a y:~a" (x p) (y p)) x (hoge num)))))))
;; ;; (hoge num)
;;       ;; (create-render-button x (+ x (* 23 (length (get-buki-name (buki p))))) *st-buki-y* *st-buki-y2*
;;       ;; 			    (format nil "~a" (get-buki-name (buki p)))
;;       ;; 			    (+ x 4) *st-buki-y* :font *font20*))))


;; ;;selectedキャラに枠つける
;; (defun render-select-waku ()
;;   (with-slots (selected) *mouse*
;;     (when selected
;;       (select-object *hmemdc* (get-stock-object :white-brush))
;;       (rectangle *hmemdc* (posx selected) (posy selected) (+ (posx selected) 32)
;; 		 (+ (posy selected) 32)))))
;;       ;;(delete-object (select-object *hmemdc* (create-solid-brush (encode-rgb 255 255 255)))))))
;;       ;;(select-object *hogememdc* *waku-ao*)
;;       ;;(new-trans-blt (* (x selected) *obj-w*) (* (y selected) *obj-h*) 0 0 32 32 32 32))))

;; ;;マウスカーソルのある場所に枠付ける
;; (defun render-mouse-cursor-waku-and-cell-info ()
;;   (let* ((x1 (floor (x *mouse*) *obj-w*))
;; 	 (y1 (floor (y *mouse*) *obj-h*)))
;;     (when (and (>= *map-w* (x *mouse*))
;; 	       (>= *map-h* (y *mouse*)))
;;       (select-object *hogememdc* *waku-img*)
;;       (new-trans-blt (* x1 *obj-w*) (* y1 *obj-h*) 0 0 128 128 32 32))
;;     (when (and (> *yoko-block-num* x1)
;; 	       (> *tate-block-num* y1))
;;       (let* ((cell (aref *celldescs* (aref (field *donjon*) y1 x1)))
;; 	     (heal-str (if (heal cell)
;; 			 (format nil "回復:~d%" (heal cell))
;; 			 (format nil "回復:なし"))))
;; 	(select-object *hmemdc* *font30*)
;; 	(set-text-color *hmemdc* (encode-rgb 255 255 255))
;; 	(text-out *hmemdc* (format nil "地形:~a" (name cell)) 500 350)
;; 	(text-out *hmemdc* (format nil "回避:+~d%" (avoid cell)) 500 380)
;; 	(text-out *hmemdc* heal-str 500 410)))))

;; ;;攻撃可能な敵に枠つける
;; (defun render-can-atk-waku ()
;;   (with-slots (selected) *mouse*
;;     (when (and selected
;; 	       (canatkenemy selected))
;;       (if (and (buki selected)
;; 	       (eq (atktype (buki selected)) :atk))
;; 	  (select-object *hmemdc* (aref *brush* +red+))
;; 	  (select-object *hmemdc* (aref *brush* +yellow+)))
;;       (loop :for e :in (canatkenemy selected)
;; 	 :do (rectangle *hmemdc* (posx e) (posy e) (+ (posx e) 32) (+ (posy e) 32) )))))

;; 	 ;;(delete-object br)))))
;; ;;(select-object *hogememdc* *waku-aka*)
;; 	      ;;(new-trans-blt (* (x e) *obj-w*) (* (y e) *obj-h*) 0 0 32 32 32 32)))))



;; ;;選択してるキャラの移動できる範囲表示
;; (defun render-move-waku ()
;;   (with-slots (selected) *mouse*
;;     (when (and selected
;; 	       (movearea selected))
;;       (let ((x1 (floor (x *mouse*) *obj-w*))
;; 	    (y1 (floor (y *mouse*) *obj-h*)))
;; 	;;(select-object *hogememdc* *waku-img*)
;; 	(loop :for area :in (movearea selected)
;; 	   :do (let ((x (car area)) (y (cadr area)))
;; 		 (cond
;; 		   ((and (= x x1) (= y y1)) ;;移動可能範囲内にマウスがあったら
;; 		    (select-object *hogememdc* *waku-ao*)
;; 		    (new-trans-blt (* x *obj-w*) (* y *obj-h*) 0 0 32 32 32 32))
;; 		   (t
;; 		    (select-object *hogememdc* *waku-aka*)
;; 		    (new-trans-blt (* x *obj-w*) (* y *obj-h*) 0 0 32 32 32 32)))))))))


;; ;;マウスと重なってる敵キャラのステータス表示
;; (defun render-enemy-status-on-mouse ()
;;   (loop :for p :in (enemies *donjon*)
;;      :do (when (and (not (eq (state p) :dead))
;; 		    (>= (+ (posx p) *obj-w*) (x *mouse*) (posx p))
;; 		    (>= (+ (posy p) *obj-h*) (y *mouse*) (posy p)))
;; 	     (render-chara-status p *cursor-pos-unit-status-x*))))

;; ;;マウスと重なってるキャラのステータス表示
;; (defun render-party-status-on-mouse ()
;;   (loop :for p :in (party *p*)
;;      :do (when (and (not (eq (state p) :dead))
;; 		    (>= (+ (posx p) *obj-w*) (x *mouse*) (posx p))
;; 		    (>= (+ (posy p) *obj-h*) (y *mouse*) (posy p)))
;; 	     (render-chara-status p *cursor-pos-unit-status-x* ))))

;; (defun render-unit-status-on-mouse ()
;;   (select-object *hmemdc* *font30*)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (render-party-status-on-mouse)
;;   (render-enemy-status-on-mouse))

;; (defun render-selected-unit-status ()
;;   (with-slots (selected) *mouse*
;;     (when selected
;;       (select-object *hmemdc* *font30*)
;;       (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;       (render-chara-status selected *selected-unit-status-x*))))

;; ;;行動できるウニっとの色付け
;; (defun can-action-unit-waku ()
;;   (select-object *hmemdc* (aref *brush* +cyan+))
;;   (loop
;;      :for p :in (party *p*)
;;      :do (when (eq (state p) :action)
;; 	   (rectangle *hmemdc* (posx p) (posy p) (+ (posx p) 32) (+ (posy p) 32)))))
;; 	   ;;(delete-object (select-object *hmemdc* (create-solid-brush (encode-rgb 157 204 227)))))))


;; ;;出撃キャラを表示
;; (defun render-fight-party ()
;;   (select-object  *hogememdc* *job-img*)
;;   (loop
;;      :for p :in (party *p*)
;;      :do (render-unit p)))


;; ;;選択中のキャラ
;; (defun render-selected-chara ()
;;   (with-slots (selected) *mouse*
;;     (when (and selected
;; 	       (eq (team selected) :ally))
;;       (select-object  *hogememdc* *job-img*)
;;       (new-trans-blt (posx selected) (posy selected) 0 (* (img-h selected) 32) 32 32 32 32))))



;; ;;装備変更ボタン
;; (defun render-weapon-change-btn ()
;;   (when (and (selected *mouse*)
;; 	     (eq (team (selected *mouse*)) :ally))
;;     (create-render-button *w-change-btn-x1* *w-change-btn-x2* *w-change-btn-y1* *w-change-btn-y2*
;; 			  "装備変更" (+ 3 *w-change-btn-x1*) *w-change-btn-y1* :font *font40*)))

;; ;;出撃ボタン
;; (defun render-ready-btn ()
;;   (create-render-button *battle-btn-x1* *battle-btn-x2* *battle-btn-y1* *battle-btn-y2*
;; 			"出撃" 500 450 :font *font40*))

;; ;;セーブボタン
;; (defun render-save-button ()
;;   (create-render-button *save-x1* *save-x2* *save-y1* *save-y2*
;; 			"セーブ" *save-x1* *save-y1* :font *font40*))

;; (defun render-load-button ()
;;   (create-render-button *load-x1* *load-x2* *load-y1* *load-y2*
;; 			"ロード" (+ *load-x1* 4) *load-y1* :font *font40*))


;; (defun render-pre-battle-explain ()
;;   (select-object *hmemdc* *font40*)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (text-out *hmemdc* "初期位置を決めてください" 0 480))

;; ;;いろいろ表示
;; (defun render-status ()
;;   (select-object *hmemdc* *font30* )
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (if (eq (turn *p*) :ally)
;;       (text-out *hmemdc* "プレイヤーのターン" 500 3)
;;       (text-out *hmemdc* "敵のターン" 500 3))
;;   (text-out *hmemdc* "選択中のユニット(右クリで解除)" 480 25)
;;   (select-object *hmemdc* *font40*)
;;   (text-out *hmemdc* (format nil "地下~3d階" (stage *donjon*)) 0 480)
;;   ;;ターン終了ボタン
;;   (select-object *hmemdc* *font40*)
;;   (create-render-button *turn-end-x1* *turn-end-x2* *turn-end-y1*  *turn-end-y2*
;; 			"ターン終了" *turn-end-x1* *turn-end-y1* :font *font40*))


;; ;;テキスト描画
;; (defun render-text (str posx posy)
;;   (select-object *hmemdc* *font20*)
;;   ;;縁取り
;;   (set-text-color *hmemdc* (encode-rgb 0 0 0))
;;   (text-out *hmemdc* (format nil "~a" str) (- posx 2) posy)
;;   (text-out *hmemdc* (format nil "~a" str) (+ posx 2) posy)
;;   (text-out *hmemdc* (format nil "~a" str) posx (- posy 2))
;;   (text-out *hmemdc* (format nil "~a" str) posx (+ posy 2))
;;   ;;
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (text-out *hmemdc* (format nil "~a" str) posx posy))

;; ;;ゲットしたアイテム表示
;; (defun render-get-item-text ()
;;   (with-slots (getitem) *p*
;;     (when getitem
;;       (render-text (name getitem) (posx getitem) (posy getitem)))))



;; ;;ダメージ表示
;; (defun render-damage (e)
;;   (with-slots (dmg) e
;;     (when dmg
;;       (select-object *hmemdc* *font20*)

;;       ;;(set-bk-mode *hmemdc* :transparent)
;;       ;;縁取り
;;       (set-text-color *hmemdc* (encode-rgb 0 0 0))
;;       (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (- (posx dmg) 2) (posy dmg))
;;       (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (+ (posx dmg) 2) (posy dmg))
;;       (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (posx dmg) (- (posy dmg) 2))
;;       (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (posx dmg) (+ (posy dmg) 2))
;;       ;;
;;       (set-text-color *hmemdc* (color dmg))
;;       (text-out *hmemdc* (format nil "~d" (dmg-num dmg)) (posx dmg) (posy dmg))
;;       )))

;; ;;全てのダメージ表示
;; (defun render-all-damage ()
;;   ;;(render-damage *p*  (encode-rgb 255 147 122))
;;   ;;(render-hpbar *p*)
;;   (loop for e in (party *p*)
;;      do (render-damage e))
;;   (loop for e in (enemies *donjon*)
;;      do (render-damage e)))
;;        ;; (when (and (/= (maxhp e) (hp e))
;;        ;; 		  (null (dead e)))
;;        ;; 	 (render-hpbar e))))



;; (defun render-save-slots ()
;;   (let ((str1 (or *save1-day* (car *load-game-data1*) "なし"))
;; 	(str2 (or *save2-day* (car *load-game-data2*) "なし"))
;; 	(str3 (or *save3-day* (car *load-game-data3*) "なし")))
;;     (create-render-button *save-slot1-x1* *save-slot1-x2* *save-slot1-y1* *save-slot1-y2*
;; 			  (format nil "slot1 ~a" str1)
;; 			  (+ *save-slot1-x1* 20) *save-slot1-y1* :font *font40*)
;;     (create-render-button *save-slot2-x1* *save-slot2-x2* *save-slot2-y1* *save-slot2-y2*
;; 			  (format nil "slot1 ~a" str2)
;; 			  (+ *save-slot2-x1* 20) *save-slot2-y1* :font *font40*)
;;     (create-render-button *save-slot3-x1* *save-slot3-x2* *save-slot3-y1* *save-slot3-y2*
;; 			  (format nil "slot1 ~a" str3)
;; 			  (+ *save-slot3-x1* 20) *save-slot3-y1* :font *font40*)
;;     (create-render-button *save-end-x1* *save-end-x2* *save-end-y1* *save-end-y2*
;; 			  "終了" *save-end-x1* *save-end-y1* :font *font40*)))


;; (defun render-load-slots ()
;;   (let ((str1 (if *load-game-data1* (car *load-game-data1*) "なし"))
;; 	(str2 (if *load-game-data2* (car *load-game-data2*) "なし"))
;; 	(str3 (if *load-game-data3* (car *load-game-data3*) "なし")))
;;     (create-render-button *save-slot1-x1* *save-slot1-x2* *save-slot1-y1* *save-slot1-y2*
;; 			  (format nil "slot1 ~a" str1)
;; 			  (+ *save-slot1-x1* 20) *save-slot1-y1* :font *font40*)
;;     (create-render-button *save-slot2-x1* *save-slot2-x2* *save-slot2-y1* *save-slot2-y2*
;; 			  (format nil "slot1 ~a" str2)
;; 			  (+ *save-slot2-x1* 20) *save-slot2-y1* :font *font40*)
;;     (create-render-button *save-slot3-x1* *save-slot3-x2* *save-slot3-y1* *save-slot3-y2*
;; 			  (format nil "slot1 ~a" str3)
;; 			  (+ *save-slot3-x1* 20) *save-slot3-y1* :font *font40*)
;;     (create-render-button *save-end-x1* *save-end-x2* *save-end-y1* *save-end-y2*
;; 			  "終了" *save-end-x1* *save-end-y1* :font *font40*)))


;; ;;セーブ画面
;; (defun render-save-gamen ()
;;   (render-background)
;;   (render-save-slots))

;; (defun render-load-gamen ()
;;   (render-background)
;;   (render-load-slots))

;; ;;出撃準備画面を表示
;; (defun render-battle-preparation ()
;;   (render-background)
;;   (render-bgmonoff-button)
;;   (render-field)
;;   (render-items)
;;   (render-enemies)
;;   (render-fight-party)

;;   (render-selected-unit-status)
;;   (render-unit-status-on-mouse)
;;   (render-mouse-cursor-waku-and-cell-info)
;;   (render-select-waku)
;;   (render-selected-chara)
;;   ;;(render-weapon-change-btn)
;;   (render-pre-battle-explain)
;;   (render-save-button)
;;   (render-load-button)
;;   (render-ready-btn))

;; ;;バトル画面を表示
;; (defun render-battle ()
;;   (render-background)
;;   (render-bgmonoff-button)
;;   (render-field t)
;;   (render-items)
;;   (render-status)

;;   (can-action-unit-waku )
;;   (render-select-waku)
;;   (render-can-atk-waku)
;;   (render-enemies)
;;   (render-fight-party)
;;   (render-selected-unit-status)
;;   (render-unit-status-on-mouse)
;;   (render-get-item-text)
;;   (render-mouse-cursor-waku-and-cell-info )
;;   (render-move-waku)

;;   (render-selected-chara)
;;   (render-all-damage)
;;   (render-weapon-change-btn )
;;   ;;(render-select-waku)
;;   )

;; ;;マップを表示
;; (defun render-donjon ()
;;   (render-background)
;;   (render-bgmonoff-button)
;;   (render-field)
;;   (render-fight-party)
;;   (render-unit-status-on-mouse)
;;   (render-select-waku)

;;   (render-selected-chara)
;;   ;; (render-yuka)
;;   ;; (render-block)
;;    ;;(render-item)
;;    )












;; ;;test
;; (defun render-test ()
;;   (select-object *hogememdc*  *anime-monsters-img*)
;;   (transparent-blt *hmemdc* 0 0 *hogememdc* 0 32 :width-source 32
;; 		   :height-source 32
;; 		   :width-dest 32 :height-dest 32
;; 		   :transparent-color (encode-rgb 0 255 0)))

;; ;;タイトル画面のボタンの位置
;; (defun title-gamen-mouse-pos ()
;;   (cond
;;     ((and (>= 475 (x *mouse*) 330)
;;   	  (>= 405 (y *mouse*) 360))
;;      1)
;;     ((and (>= 475 (x *mouse*) 330)
;;   	  (>= 455 (y *mouse*) 410))
;;      2)
;;     ((and (>= 420 (x *mouse*) 330)
;;   	  (>= 505 (y *mouse*) 460))
;;      3)))

;; ;;タイトル画面
;; (defun render-title-gamen ()
;;   (render-background)
;;   (render-bgmonoff-button)
;;   (select-object *hmemdc* *font140*)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (set-text-color *hmemdc* (encode-rgb 0 155 255))
;;   (text-out *hmemdc* "もげてぃかる仮" 50 70)
;;   (create-render-button-no-waku *title-start-x1*  *title-start-x2* *title-start-y1* *title-start-y2*
;; 				"はじめる" *title-start-x1* *title-start-y1* :font *font40*)
;;   (create-render-button-no-waku *continue-x1*  *continue-x2* *continue-y1* *continue-y2*
;; 				"つづきから" *continue-x1* *continue-y1* :font *font40*)
;;   (create-render-button-no-waku *title-end-x1*  *title-end-x2* *title-end-y1* *title-end-y2*
;; 				"おわる" *title-end-x1* *title-end-y1* :font *font40*)
;;   (select-object *hmemdc* *font40*)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))

;;   (text-out *hmemdc* (format nil "x:~d y:~d" (x *mouse*) (y *mouse*)) 300 200)
;;   (text-out *hmemdc* (format nil "winx:~d winy:~d" *change-screen-w* *change-screen-h*) 300 250))

;;     ;; (select-object *hogememdc* *waku2-img*)
;;     ;; (alpha-blend *hmemdc* 0 0 *hogememdc* 0 0 :width-dest 128 :height-dest 128
;;     ;; 		 :width-source 128 :height-source 128)));;source-constant-alpha 50)))

;; ;;ゲームオーバー画面
;; (defun render-gameover-gamen ()
;;   (render-background)
;;   (render-bgmonoff-button)
;;   (select-object *hmemdc* *font140*)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (set-text-color *hmemdc* (encode-rgb 0 155 255))
;;   (text-out *hmemdc* "全 滅" 280 70)
;;   (create-render-button-no-waku *title-start-x1*  *title-start-x2* *title-start-y1* *title-start-y2*
;; 				"はじめから" *title-start-x1* *title-start-y1* :font *font40*)
;;   (create-render-button-no-waku *continue-x1*  *continue-x2* *continue-y1* *continue-y2*
;; 				"再挑戦" *continue-x1* *continue-y1* :font *font40*)
;;   (create-render-button-no-waku *title-end-x1*  *title-end-x2* *title-end-y1* *title-end-y2*
;; 				"おわる" *title-end-x1* *title-end-y1* :font *font40*)
;;   (select-object *hmemdc* *font40*)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (text-out *hmemdc* (format nil "x:~d y:~d" (x *mouse*) (y *mouse*)) 300 200)
;;   (text-out *hmemdc* (format nil "winx:~d winy:~d" *change-screen-w* *change-screen-h*) 300 250))




;; ;;エンディング画面
;; (defun render-ending-gamen ()
;;   (let ((time1 (- (endtime *p*) (starttime *p*))))
;;     (multiple-value-bind (h m s ms) (get-hms time1)
;;       (render-background)
;;       (render-bgmonoff-button)
;;       (select-object *hmemdc* *font70*)
;;       ;;(set-bk-mode *hmemdc* :transparent)
;;       (set-text-color *hmemdc* (encode-rgb 0 155 255))
;;       (text-out *hmemdc* (format nil "冒険者たちは") 250 10)
;;       (text-out *hmemdc* (format nil "モゲダンジョンを踏破した") 150 80)
;;       (select-object *hmemdc* *font70*)
;;       (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;       (text-out *hmemdc* (format nil "クリアタイム") 270 180)
;;       (text-out *hmemdc* (format nil  "~2,'0d 時間 ~2,'0d 分 ~2,'0d 秒 ~2,'0d" h m s ms) 100 260)
;;       (create-render-button-no-waku *title-start-x1*  *title-start-x2* *title-start-y1* *title-start-y2*
;; 				"はじめから" *title-start-x1* *title-start-y1* :font *font40*)
;;       (create-render-button-no-waku *title-end-x1*  *title-end-x2* *title-end-y1* *title-end-y2*
;; 				    "おわる" *title-end-x1* *title-end-y1* :font *font40*))))

;; ;;初期パーティ編成画面のボタンの位置
;; (defun party-edit-gamen-mouse-pos ()
;;   (loop
;;      :for num :from 0
;;      :for btn-pos :in *init-party-edit-gamen-btn-pos-list*
;;      :do
;;        (multiple-value-bind (xmin ymin xmax ymax)
;; 	   (apply #'values btn-pos)
;; 	 (when (and (>= xmax (x *mouse*) xmin)
;; 		    (>= ymax (y *mouse*) ymin))
;; 	   (return-from party-edit-gamen-mouse-pos num)))))

;; ;;選んだ初期パーティ表示
;; (defun render-my-init-party-list (mouse)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (loop
;;      :for chara :in (party *p*)
;;      :for y :from 110 :by 50
;;      :for mousepos :from 7
;;      :do (if (and mouse
;; 		  (= mouse mousepos))
;; 	     (progn
;; 	       (select-object *hmemdc* (get-stock-object :white-brush))
;; 	       (multiple-value-bind (x1 y1 x2 y2)
;; 		   (apply #'values (nth mouse *init-party-edit-gamen-btn-pos-list*))
;; 		 (rectangle *hmemdc* x1 y1 x2 y2))
;; 	       (set-text-color *hmemdc* (encode-rgb 0 0 0)))
;; 	     (set-text-color *hmemdc* (encode-rgb 255 255 255)))
;;        (text-out *hmemdc* (format nil "~a" (get-job-data (job chara) :name)) 330 y)))

;; ;;初期クラス表示
;; (defun render-init-class ()
;;   (let ((mouse (party-edit-gamen-mouse-pos)))
;;     (render-bgmonoff-button)
;;     (select-object *hmemdc* (get-stock-object :white-brush))
;;     (cond
;;       ((and mouse (>= 6 mouse))
;;        (multiple-value-bind (x1 y1 x2 y2)
;; 	   (apply #'values (nth mouse *init-party-edit-gamen-btn-pos-list*))
;; 	 (rectangle *hmemdc* x1 y1 x2 y2)))
;;       ((and mouse (= mouse 12))
;;        (multiple-value-bind (x1 y1 x2 y2)
;; 	   (apply #'values (nth mouse *init-party-edit-gamen-btn-pos-list*))
;; 	 (rectangle *hmemdc* x1 y1 x2 y2))
;;        (set-text-color *hmemdc* (encode-rgb 0 0 0))
;;        (select-object *hmemdc* *font70*)
;;        (text-out *hmemdc* "出発" 550 400)))
;;     (loop
;;        :for name :in '("戦士" "魔術師" "僧侶" "射手" "騎士" "盗賊" "天馬騎士")
;;        :for y :from 110 :by 50
;;        :for mousepos :from 0
;;        :do
;; 	 (select-object *hmemdc* *font40*)
;; 	 (if (and mouse
;; 		  (= mouse mousepos))
;; 	     (set-text-color *hmemdc* (encode-rgb 0 0 0))
;; 	     (set-text-color *hmemdc* (encode-rgb 255 255 255)))
;; 	 (text-out *hmemdc* (format nil "~a" name) 40 y))
;;     (render-my-init-party-list mouse)))



;; ;;初期パーティ画面
;; (defun render-party-edit-gamen ()
;;   (render-background)
;;   (select-object *hmemdc* *font70*)
;;   ;;(set-bk-mode *hmemdc* :transparent)
;;   (set-text-color *hmemdc* (encode-rgb 0 155 255))
;;   (text-out *hmemdc* "初期パーティを作ってください(5人)" 5 10)
;;   (text-out *hmemdc* "→" 225 260)
;;   (text-out *hmemdc* "出発" 550 400)
;;   (select-object *hogememdc* *waku-img*)
;;   (new-trans-blt 10 100 0 0 128 128 200 400)
;;   (new-trans-blt 300 100 0 0 128 128 200 400)
;;   (new-trans-blt 540 400 0 0 128 128 140 70)
;;   (select-object *hmemdc* *font40*)
;;   (text-out *hmemdc* (format nil "x:~d y:~d" (x *mouse*) (y *mouse*)) 500 200)
;;   (text-out *hmemdc* "右クリでキャラ削除" 510 150)
;;   (set-text-color *hmemdc* (encode-rgb 255 255 255))
;;   (render-init-class))




;; ;;ゲーム全体描画
;; (defun render-game (hdc)
;;   (case (state *p*)
;;     (:title ;;タイトル画面
;;      (render-title-gamen))
;;     (:initpartyedit
;;      (render-party-edit-gamen))
;;     (:battle-preparation
;;      (render-battle-preparation))
;;     (:save
;;      (render-save-gamen))
;;     (:load
;;      (render-load-gamen))
;;     (:weaponchange
;;      (render-weapon-change-gamen))
;;     (:battle
;;      (render-battle))
;;     (:dead
;;      (render-gameover-gamen))
;;     (:ending ;;エンディング画面
;;      (render-ending-gamen)
;;      ))
;;   (transparent-blt hdc 0 0 *hmemdc* 0 0
;;   		   :width-dest *change-screen-w* :height-dest *change-screen-h*
;;   		   :width-source (rect-right *c-rect*) :height-source (rect-bottom *c-rect*)
;;   		   :transparent-color (encode-rgb 0 255 0)))
;; (select-object *hmemdc* *waku2-img*)
;; (alpha-blend hdc 0 0 *hmemdc* 0 0 :width-dest 128 :height-dest 128
;; 	       :width-source 128 :height-source 128 :source-constant-alpha 50))
