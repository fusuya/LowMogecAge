(in-package :lowmogecage)


(defmacro my-enum (&rest names)
  `(progn
     ,@(loop for i from 0
             for name in names
        collect `(defparameter ,name ,i))))

;;class copy
(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defparameter *atk-width* 8)
(defparameter *atk-pos-max* (* *atk-width* 3))


(defparameter *job-monsters* nil)
(defparameter *objs-img* nil)
(defparameter *arrow-img* nil)
(defvar *waku-img* nil)
(defvar *waku2-img* nil)
(defvar *waku-ao* nil)
(defvar *waku-aka* nil)
(defparameter *job-img* nil)

;;game data '(day playerdata donjondata)
(defparameter *load-game-data1* nil)
(defparameter *load-game-data2* nil)
(defparameter *load-game-data3* nil)
(defparameter *save1-day* nil)
(defparameter *save2-day* nil)
(defparameter *save3-day* nil)


(gk:register-resource-package :keyword (merge-pathnames "assets/" (asdf:system-source-directory :lowmogecage)))


(gk:define-font :mplus "font/mplus-1mn-regular.ttf")

(gk:define-image :job-img "img/job-img.png")
(gk:define-image :obj-img "img/new-objs-img.png")
(gk:define-image :world-map "img/worldmap.png")
(gk:define-image :skill-img "img/skill.png")
(gk:define-image :monster-img "img/monster-img.png")


;;プレイヤー画像切り替えよう
(defconstant +down+ 0)
(defconstant +left+ 2)
(defconstant +right+ 3)
(defconstant +up+ 1)

;;敵画像切り替えよう
(defconstant +brigand-anime+ 0)
(defconstant +dragon-anime+ 1)
(defconstant +hydra-anime+ 2)
(defconstant +yote-anime+ 3)
(defconstant +orc-anime+ 4)
(defconstant +slime-anime+ 5)
(defconstant +boss-anime+ 11)
(defconstant +hydra-atk+ 6)
(defconstant +brigand-ball+ 7)
(defconstant +dragon-fire+ 8)
(defconstant +orc-atk+ 9)

;;敵の攻撃演出時間
(defparameter *orc-atk-effect-time* 30)
(defparameter *hydra-atk-effect-time* 30)

(defparameter *4-dir* '((1 0) (0 1) (-1 0) (0 -1)))
(defparameter *8-dir* '((1 0) (0 1) (-1 0) (0 -1) (1 1) (1 -1) (-1 1) (-1 -1)))


(defparameter *backup-player-data* nil)
(defparameter *backup-donjon-data* nil)


(defparameter *map* nil)
(defparameter *p* nil)

(defparameter *end* 0)
(defparameter *lv-exp* 100)
(defparameter *images* nil)
(defparameter *anime-monsters-img* nil)

(defparameter *sound-name-and-path-list*
  '((:knuckle-wav "sound/knuckle90.wav")
    ))

(defun set-sound ()
  (loop :for name-path :in *sound-name-and-path-list*
	:do (gk:define-sound (car name-path) (cadr name-path))))

;; (defparameter *knuckle-wav*    (namestring (merge-pathnames "knuckle90.wav"      *sound-root*)))
;; (defparameter *ax-wav*         (namestring (merge-pathnames "ax90.wav"           *sound-root*)))
;; (defparameter *sword-wav*      (namestring (merge-pathnames "sword90.wav"        *sound-root*)))
;; (defparameter *spear-wav*      (namestring (merge-pathnames "spear90.wav"        *sound-root*)))
;; (defparameter *bow-wav*        (namestring (merge-pathnames "bow90.wav"          *sound-root*)))
;; (defparameter *wand-wav*       (namestring (merge-pathnames "wand90.wav"         *sound-root*)))
;; (defparameter *heal-wav*       (namestring (merge-pathnames "heal90.wav"         *sound-root*)))
;; (defparameter *chest-wav*      (namestring (merge-pathnames "chest90.wav"        *sound-root*)))
;; (defparameter *move-wav*       (namestring (merge-pathnames "move90.wav"         *sound-root*)))
;; (defparameter *lvup-wav*       (namestring (merge-pathnames "lvup90.wav"         *sound-root*)))
;; (defparameter *mouse-move-wav* (namestring (merge-pathnames "mouse-select90.wav" *sound-root*)))
;; (defparameter *select-wav*     (namestring (merge-pathnames "select90.wav"       *sound-root*)))
;; ;;bgm
;; (defparameter *title-bgm-path*      (namestring (merge-pathnames "titleBGM.wav"  *sound-root*)))
;; (defparameter *edit-bgm-path*       (namestring (merge-pathnames "editBGM.wav"   *sound-root*)))
;; (defparameter *prebattle-bgm-path*  (namestring (merge-pathnames "prebattle.wav" *sound-root*)))
;; (defparameter *battle1-bgm-path*    (namestring (merge-pathnames "battle1.wav"   *sound-root*)))
;; (defparameter *battle2-bgm-path*    (namestring (merge-pathnames "battle2.wav"   *sound-root*)))
;; (defparameter *battle3-bgm-path*    (namestring (merge-pathnames "battle3.wav"   *sound-root*)))
;; (defparameter *battle4-bgm-path*    (namestring (merge-pathnames "battle4.wav"   *sound-root*)))
;; (defparameter *battle5-bgm-path*    (namestring (merge-pathnames "battle5.wav"   *sound-root*)))

;;alias
(defparameter *titlebgm*      "title")
(defparameter *editbgm*       "edit")
(defparameter *prebattle*     "prebattle")
(defparameter *battle1bgm*    "battle1")
(defparameter *battle2bgm*    "battle2")
(defparameter *battle3bgm*    "battle3")
(defparameter *battle4bgm*    "battle4")
(defparameter *battle5bgm*    "battle5")
(defparameter *bgm* nil)

(defparameter *battle-bgm-list* (list *battle1bgm* *battle2bgm* *battle3bgm* *battle4bgm* *battle5bgm*))

;;window 拡大
(Defparameter *scale-w* 1.0)
(Defparameter *scale-h* 1.0)

;;obj表示拡大率
(defparameter *scale-obj-w* 1.5)
(defparameter *scale-obj-h* 1.5)


;;基本サイズ 元の画像サイズ
(defparameter *origin-obj-w* 32)
(defparameter *origin-obj-h* 32)

(defparameter *obj-w* (* *scale-obj-w* *origin-obj-w*))
(defparameter *obj-h* (* *scale-obj-h* *origin-obj-h*))


;;プレイヤーのサイズ
(defparameter *p-w* 24)
(defparameter *p-h* 32)



;;オブジェクト画像表示サイズ
(defparameter *w-test* 36)
(defparameter *h-test* 36)





(defparameter *window-w* (* *obj-w* 27))
(defparameter *window-h* (* *obj-h* 17))

(defparameter *origin-window-w* (* *origin-obj-w* 27))
(defparameter *origin-window-w/2* (/ *origin-window-w* 2))

(defparameter *scale-window-w* (* *window-w* *scale-w*))
(defparameter *scale-window-h* (* *window-h* *scale-h*))

(defparameter *window-center* (/ *window-w* 2))


(defparameter *waku-size* 10) ;;ゲームフィールドの周りの枠太さ
(defparameter *c-rect* nil) ;;クライアント領域

(defparameter *start-time* 0)
(defparameter *name* nil)
(defparameter *donjon* nil)
(defparameter *battle-field* nil)


(defparameter *screen-center-x* nil)

(defparameter *brush* nil)
(defparameter *start* nil)
(defparameter *hmemDC* nil)
(defparameter *hbitmap* nil)


(defparameter *hogememDC* nil)
(defparameter *hogebitmap* nil)

(defparameter *HPbar-max* 40)
(defparameter *bukiexpbar-max* 100)


(defparameter *mouse-hosei-x* 1)
(defparameter *mouse-hosei-y* 1)

(defparameter *full-command-string-list* '(("通常移動" normal-move-cmd-btn)
					   ("全力移動" fast-move-cmd-btn)
					   ("通常攻撃" attack-cmd-btn)
					   ("スキル" skill-cmd-btn)
					   ("待機" wait-cmd-btn)))

(defparameter *command-string-after-move* '(("通常攻撃" attack-cmd-btn)
					   ("スキル" skill-cmd-btn)
					   ("待機" wait-cmd-btn)))

(Defparameter *world-map-test-arr* (make-array '(100 100) :initial-element 0))

;;(defparameter *selected-unit-status-x* (+ *map-w* 10))
;;(defparameter *cursor-pos-unit-status-x* (+ *selected-unit-status-x* 200))

(defun get-char-width-height (font)
  "文字の幅と高さを取得"
  (cond
    ((equal font *font64*)
     (values 45 38))))

(defun get-font (size)
  (case size
    (64 *font64*)))

(defclass button ()
  ((pos :initarg :pos :initform (gk:vec2 0 0) :accessor button/pos)
   (string :initarg :string :initform nil :accessor button/string)
   (color :initarg :color :initform (gk:vec4 0 0 0 1) :accessor button/color)
   (font-size :initarg :font-size :initform 0 :accessor button/font-size)
   (font :initarg :font :initform nil :accessor button/font)
   (w :initarg :w :initform nil :accessor button/w)
   (h :initarg :h :initform nil :accessor button/h)
   (char-w :initarg :char-w :initform nil :accessor button/char-w)
   (char-h :initarg :char-h :initform nil :accessor button/char-h)
   (x1 :initarg :x1 :initform nil :accessor button/x1)
   (x2 :initarg :x2 :initform nil :accessor button/x2)
   (y1 :initarg :y1 :initform nil :accessor button/y1)
   (y2 :initarg :y2 :initform nil :accessor button/y2)
   ))




(defclass game-start-btn (button)
  ())

(defclass game-end-btn (button)
  ())

(defclass equip-item-btn (button)
  ((item       :accessor item        :initform 0      :initarg :item)
   (new        :accessor new        :initform nil      :initarg :new)
   (equiped-unit    :accessor equiped-unit        :initform nil      :initarg :equiped-unit)))

(defclass next-item-page (button)
  ())
(defclass prev-item-page (button)
  ())

(Defclass shop-btn (button)
  ())

(defclass recruit-btn (button)
  ())

(defclass quest-btn (button)
  ())

(defclass town-exit-btn (button)
  ())


(defclass command-btn (button)
  ())

(defclass normal-move-cmd-btn (command-btn)
  ())

(defclass fast-move-cmd-btn (command-btn)
  ())

(defclass attack-cmd-btn (command-btn)
  ())

(defclass skill-cmd-btn (command-btn)
  ())

(defclass wait-cmd-btn (command-btn)
  ())

(defclass skill-btn (command-btn)
  ((tag :initarg :tag :initform (gk:vec2 0 0) :accessor tag)))

;;アイテムリスト表示マックス
(defparameter *item-show-max* 20)



(defconstant +action-end+ 2)

(my-enum +purple+ +red+ +green+ +blue+ +yellow+ +cyan+ +pink+ +white+)

(my-enum +arrow-right+ +arrow-left+ +arrow-down+ +arrow-up+)

(my-enum +title-bgm+)

(defparameter *game* nil)

(defclass game ()
  ((state  :accessor game/state  :initform nil :initarg :state)
   (frame           :accessor game/frame       :initform 2    :initarg :frame)
   (flash-flag           :accessor game/flash-flag       :initform t    :initarg :flash-flag)
   (action-state  :accessor game/action-state  :initform :ready :initarg :action-state)
   (enemy-rate :accessor game/enemy-rate  :initform nil    :initarg :enemy-rate)
   (btn-list :initarg :btn-list :initform nil :accessor game/btn-list)
   (scroll           :accessor game/scroll       :initform nil    :initarg :scroll)
   (move           :accessor game/move       :initform 2    :initarg :move)
   (move-paths           :accessor game/move-paths       :initform nil    :initarg :move-paths)
   (move-goal           :accessor game/move-goal       :initform nil    :initarg :move-goal)
   (world-pos           :accessor game/world-pos       :initform nil    :initarg :world-pos)
   (party           :accessor game/party       :initform nil    :initarg :party)
   (cursor          :accessor game/cursor      :initform 0      :initarg :cursor)
   (item            :accessor game/item        :initform nil    :initarg :item)
   (bgm             :accessor game/bgm         :initform :on    :initarg :bgm)
   (endtime         :accessor game/endtime     :initform 0      :initarg :endtime)
   (starttime       :accessor game/starttime   :initform 0      :initarg :starttime)
   (save            :accessor game/save        :initform nil    :initarg :save)
   (item-page       :accessor game/item-page   :initform 0      :initarg :item-page)
   (getitem         :accessor game/getitem     :initform nil    :initarg :getitem)
   (selected-unit   :accessor game/selected-unit     :initform nil    :initarg :selected-unit)
   (selected-skill   :accessor game/selected-skill     :initform nil    :initarg :selected-skill)
   (turn            :accessor game/turn        :initform :ally  :initarg :turn)
   (prestate        :accessor game/prestate    :initform nil    :initarg :prestate)
   (temp-dmg        :accessor game/temp-dmg    :initform nil    :initarg :temp-dmg)
   (dmg-font        :accessor game/dmg-font    :initform nil    :initarg :dmg-font)
   ;;アイテムのドロップ率 モンスターの装備
   (warrior-weapon    :accessor warrior-weapon     :initform nil    :initarg :warrior-weapon)
   (sorcerer-weapon   :accessor sorcerer-weapon    :initform nil    :initarg :sorcerer-weapon)
   (thief-weapon      :accessor thief-weapon       :initform nil    :initarg :thief-weapon)
   (knight-weapon     :accessor knight-weapon      :initform nil    :initarg :knight-weapon)
   (priest-weapon     :accessor priest-weapon      :initform nil    :initarg :priest-weapon)
   (archer-weapon     :accessor archer-weapon      :initform nil    :initarg :archer-weapon)
   ))


;;ブロックとか
(defclass obj ()
  ((x        :accessor x        :initform 0      :initarg :x)
   (y        :accessor y        :initform 0      :initarg :y)
   (w        :accessor w        :initform 32      :initarg :w)
   (h        :accessor h        :initform 32      :initarg :h)
   (origin-w        :accessor origin-w        :initform 32      :initarg :origin-w)
   (origin-h        :accessor origin-h        :initform 32      :initarg :origin-h)
   (pos      :accessor pos      :initform 0      :initarg :pos)
   (origin   :accessor origin      :initform 0      :initarg :origin)
   (img-id   :accessor img-id      :initform nil    :initarg :img-id)))

(defclass unit-btn (obj)
  ())

(defclass cell (obj)
  ((name  :accessor name  :initform nil :initarg :name)
   (cell-num   :accessor cell-num   :initform 0   :initarg :cell-num)
   (def   :accessor def   :initform 0   :initarg :def)
   (heal  :accessor heal  :initform 0 :initarg :heal)
   (avoid :accessor avoid :initform 0   :initarg :avoid)))

(defclass keystate ()
  ((right-key :accessor right-key :initform nil :initarg :right-key)
   (left-key  :accessor left-key  :initform nil :initarg :left-key)
   (down  :accessor down  :initform nil :initarg :down)
   (up    :accessor up    :initform nil :initarg :up)
   (space1    :accessor space1    :initform nil :initarg :space1)
   (enter :accessor enter :initform nil :initarg :enter)
   (shift :accessor shift :initform nil :initarg :shift)
   (key1     :accessor key1     :initform nil :initarg :key1)
   (key2     :accessor key2     :initform nil :initarg :key2)
   (key3     :accessor key3     :initform nil :initarg :key3)
   (key4     :accessor key4     :initform nil :initarg :key4)
   (key5     :accessor key5     :initform nil :initarg :key5)
   (key0     :accessor key0     :initform nil :initarg :key0)
   (z     :accessor z     :initform nil :initarg :z)
   (w     :accessor w     :initform nil :initarg :w)
   (s     :accessor s     :initform nil :initarg :s)
   (d     :accessor d     :initform nil :initarg :d)
   (a     :accessor a     :initform nil :initarg :a)
   (x     :accessor x     :initform nil :initarg :x)
   (c     :accessor c     :initform nil :initarg :c)))

(defclass mouse ()
  ((right :accessor right :initform nil :initarg :right)
   (left  :accessor left  :initform nil :initarg :left)
   (selected  :accessor selected  :initform nil :initarg :selected)
   (x     :accessor x :initform 0 :initarg :x)
   (y     :accessor y  :initform 0 :initarg :y)
   (x-for-obj     :accessor x-for-obj :initform 0 :initarg :x-for-obj)
   (y-for-obj     :accessor y-for-obj  :initform 0 :initarg :y-for-obj)
   ))

(defparameter *keystate* (make-instance 'keystate))
(defvar *mouse* (make-instance 'mouse))


(my-enum +plain+ +wall+ +block+ +forest+ +mtlow+ +mthigh+ +water+ +fort+ +kaidan+ +chest+ +cursor+ +obj-max+)


(defparameter *celldescs*
  (make-array +obj-max+ :initial-contents
	      (list (make-instance 'cell :name "草原" :heal nil :def 0 :avoid 0)
		    (make-instance 'cell :name "壁")
		    (make-instance 'cell :name "脆い壁")
		    (make-instance 'cell :name "森"   :heal nil :def 5  :avoid 5)
		    (make-instance 'cell :name "山"   :heal nil :def 10 :avoid 10)
		    (make-instance 'cell :name "高山" :heal nil :def 10 :avoid 10)
		    (make-instance 'cell :name "川"   :heal nil :def 10 :avoid 0)
		    (make-instance 'cell :name "砦"   :heal 20  :def 20 :avoid 10)
		    (make-instance 'cell :name "階段" :heal nil :def 0  :avoid 0)
		    (make-instance 'cell :name "dummy" :heal nil :def 0 :avoid 0)
		    (make-instance 'cell :name "dummy" :heal nil :def 0 :avoid 0))))

;;ドロップアイテムリスト
(defparameter *drop-item*
  '(:boots :atkup :defup))

(defclass donjon ()
  ((field             :accessor field              :initform nil    :initarg :field)  ;;マップ
   (field-array       :accessor field-array              :initform nil    :initarg :field-array)
   (tate              :accessor tate               :initform 0 :initarg :tate)  ;;縦幅
   (yoko              :accessor yoko               :initform 0 :initarg :yoko)  ;;横幅
   (battle-field-border-x    :accessor battle-field-border-x     :initform nil    :initarg :battle-field-border-x)
   (battle-field-border-y    :accessor battle-field-border-y     :initform nil    :initarg :battle-field-border-y)
   (enemy-init-pos    :accessor enemy-init-pos     :initform nil    :initarg :enemy-init-pos)
   (player-init-pos   :accessor player-init-pos    :initform nil    :initarg :player-init-pos)
   (enemies           :accessor enemies            :initform nil    :initarg :enemies)
   (p-sight-coord     :accessor p-sight-coord            :initform nil    :initarg :p-sight-coord)
   (chest-max         :accessor chest-max          :initform 0      :initarg :chest-max)
   (kaidan-init-pos   :accessor kaidan-init-pos    :initform nil    :initarg :kaidan-init-pos)
   (donjonnum         :accessor donjonnum          :initform nil    :initarg :donjonnum)
   (kaidan            :accessor kaidan             :initform nil    :initarg :kaidan) ;;ブロック
   (chest             :accessor chest              :initform nil    :initarg :chest) ;;宝箱
   (stage             :accessor stage              :initform 1      :initarg :stage)
   (field-type        :accessor field-type         :initform 1      :initarg :field-type)
   (chest-init-pos    :accessor chest-init-pos     :initform nil    :initarg :chest-init-pos)
   (drop-item         :accessor drop-item          :initform nil    :initarg :drop-item)
   ))

(defclass dmg-font (obj)
  ((dmg-num  :accessor dmg-num   :initform 0     :initarg :dmg-num)
   (miny     :accessor miny      :initform 0     :initarg :miny)
   (maxy     :accessor maxy      :initform 0     :initarg :maxy)
   (y-dir    :accessor y-dir     :initform :up   :initarg :y-dir)
   (x-dir    :accessor x-dir     :initform :left :initarg :x-dir)
   (color    :accessor color     :initform :left :initarg :color)
   (font     :accessor font      :initform nil :initarg :font)
   ))

(defclass itemtext (dmg-font)
  ((name :accessor name      :initform nil :initarg :name)
   (timer :accessor timer      :initform 0     :initarg :timer)))


(defclass status ()
  ((hp        :accessor hp        :initform 30    :initarg :hp)
   (maxhp     :accessor maxhp     :initform 30    :initarg :maxhp)
   (mp        :accessor mp        :initform 30    :initarg :mp)
   (maxmp        :accessor maxmp        :initform 30    :initarg :maxmp)
   (con       :accessor con       :initform 30    :initarg :con) ;;体
   (tec       :accessor tec       :initform 30    :initarg :tec) ;;技
   (mnd       :accessor mnd       :initform 30    :initarg :mnd) ;;心
   (dex       :accessor dex       :initform 30    :initarg :dex) ;;器用度
   (agi       :accessor agi       :initform 30    :initarg :agi) ;;敏捷度
   (str       :accessor str       :initform 30    :initarg :str) ;;筋力
   (vit       :accessor vit       :initform 30    :initarg :vit) ;;生命力
   (int       :accessor int       :initform 30    :initarg :int) ;;知力
   (res       :accessor res       :initform 30    :initarg :res) ;;精神力
   (agi-bonus       :accessor agi-bonus       :initform 30    :initarg :agi-bonus)
   (str-bonus       :accessor str-bonus       :initform 30    :initarg :str-bonus)
   (dex-bonus       :accessor dex-bonus       :initform 30    :initarg :dex-bonus)
   (vit-bonus       :accessor vit-bonus       :initform 30    :initarg :vit-bonus) ;;モンスターの場合生命抵抗力として使う
   (int-bonus       :accessor int-bonus       :initform 30    :initarg :int-bonus) ;;モンスターの場合精神抵抗力として使う
   (res-bonus       :accessor res-bonus       :initform 30    :initarg :res-bonus)
   (hit-value       :accessor hit-value       :initform 0    :initarg :hit-value) ;;命中基準値
   (avoid-value       :accessor avoid-value       :initform 0    :initarg :avoid-value) ;;回避基準値
   (magic-power       :accessor magic-power       :initform 0    :initarg :magic-power) ;;魔力
   (level        :accessor level        :initform 1    :initarg :level)
   (expe      :accessor expe      :initform 0     :initarg :expe) ;;もらえる経験値orプレイヤーの所持経験値
   (lvup-exp  :accessor lvup-exp  :initform 50   :initarg :lvup-exp))) ;;次のレベルアップに必要な経験値

;;プレイヤーと敵で共通で使うやつ
(defclass anime ()
  ((atk-frame      :accessor atk-frame      :initform 0   :initarg :atk-frame)    ;;死亡判定
   (vx           :accessor vx         :initform 2   :initarg :vx)
   (vy           :accessor vy         :initform 2   :initarg :vy)
   (temp-pos :accessor temp-pos   :initform 0     :initarg :temp-pos)
   (atk-dir :accessor atk-dir   :initform 0     :initarg :atk-dir)
   (atk-pos-f :accessor atk-pos-f :initform nil   :initarg :atk-pos-f)
   (dmg       :accessor dmg       :initform nil   :initarg :dmg)     ;;ダメージ表示用
   (dmg-c     :accessor dmg-c     :initform 0     :initarg :dmg-c)   ;;ダメージを受ける間隔
   (race      :accessor race      :initform nil   :initarg :race)    ;;種族  0:プレイヤー 1:オーク 2:スライム 3:ヒドラ 4:ブリガンド 5 メテルヨテイチ
   (walk-c    :accessor walk-c    :initform 0     :initarg :walk-c)  ;;歩行アニメカウンター
   (walk-func :accessor walk-func :initform #'+   :initarg :walk-func)
   (dir-c     :accessor dir-c     :initform 0     :initarg :dir-c)   ;;方向転換用カウンター
   (atk-now   :accessor atk-now   :initform nil   :initarg :atk-now) ;;攻撃中か
   (atk-c     :accessor atk-c     :initform 0     :initarg :atk-c)   ;;攻撃モーション更新用
   (atk-img   :accessor atk-img   :initform 0     :initarg :atk-img) ;;攻撃画像番号 ０～２
   (atk-spd   :accessor atk-spd   :initform 10    :initarg :atk-spd) ;;攻撃速度
   ))

;;適用
(defclass enemy (common)
  ((centerx      :accessor centerx    :initform 30  :initarg :centerx)
   (centery      :accessor centery    :initform 30  :initarg :centery)
   (deg          :accessor deg        :initform 10  :initarg :deg)))

;;画像
(my-enum +img-e-p-knight+ +img-e-thief+ +img-e-s-knight+ +img-e-archer+ +img-e-priest+ +img-e-sorcerer+
	 +img-e-warrior+
	 +img-p-p-knight+ +img-p-thief+ +img-p-s-knight+ +img-p-archer+ +img-p-priest+ +img-p-sorcerer+ +img-p-warrior+)


;;プレイヤー用
(defclass unit (obj status anime)
  ((name        :accessor name        :initform nil :initarg :name)     ;;名前
   (weapon        :accessor weapon        :initform nil :initarg :weapon)
   (shield        :accessor shield        :initform nil :initarg :shield)
   (skill        :accessor skill        :initform nil :initarg :skill)
   (selected-cmd        :accessor selected-cmd        :initform nil :initarg :selected-cmd)
   (atked?       :accessor atked?       :initform nil  :initarg :atked?)
   (atking-enemy       :accessor atking-enemy       :initform nil  :initarg :atking-enemy)
   (sight       :accessor sight       :initform 5   :initarg :sight)
   (temparea    :accessor temparea       :initform nil   :initarg :temparea)
   (movearea    :accessor movearea    :initform nil :initarg :movearea)
   (move        :accessor move        :initform nil :initarg :move)
   (movecost    :accessor movecost    :initform nil :initarg :movecost)
   (job         :accessor job         :initform nil :initarg :job)
   (atking-type         :accessor atking-type         :initform :short :initarg :atking-type)
   (team        :accessor team        :initform nil :initarg :team)
   (armor       :accessor armor       :initform nil :initarg :armor)
   (accessory   :accessor accessory   :initform nil :initarg :accessory)
   (state       :accessor state       :initform :inaction :initarg :state)
   (canatkenemy :accessor canatkenemy :initform nil   :initarg :canatkenemy)
   (atked-pos   :accessor atked-pos   :initform nil :initarg :atked-pos)
   (job-name    :accessor job-name      :initform nil :initarg :job-name)
   (id        :accessor id        :initform 0   :initarg :id)
   (move-paths        :accessor move-paths        :initform nil   :initarg :move-paths)
   (canequip  :accessor canequip  :initform nil :initarg :canequip)
   (lvuprate  :accessor lvuprate  :initform 0   :initarg :lvuprate)
   ))

(defmethod initialize-instance :after ((unit unit) &rest initargs)
  (declare (ignore initargs))
  (with-slots (img-id) unit
    (setf img-id :job-img)))

(defclass e-unit (unit)
  (
   (drop         :accessor drop       :initform nil :initarg :drop)    ;;ドロップするアイテム
   (wakeup      :accessor wakeup      :initform nil :initarg :wakeup)))

(defclass monster (e-unit)
  ((def         :accessor def       :initform nil :initarg :def)
   (atk-point         :accessor atk-point       :initform 0 :initarg :atk-point)
   (rangemin         :accessor rangemin       :initform 1 :initarg :rangemin)
   (rangemax         :accessor rangemax       :initform 1 :initarg :rangemax)))

(defmethod initialize-instance :after ((monster monster) &rest initargs)
  (declare (ignore initargs))
  (with-slots (img-id) monster
    (setf img-id :monster-img)))


(defclass player ()
  ((party           :accessor party       :initform nil    :initarg :party)
   (cursor          :accessor cursor      :initform 0      :initarg :cursor)
   (item            :accessor item        :initform nil    :initarg :item)
   (bgm             :accessor bgm         :initform :on    :initarg :bgm)
   (endtime         :accessor endtime     :initform 0      :initarg :endtime)
   (starttime       :accessor starttime   :initform 0      :initarg :starttime)
   (save            :accessor save        :initform nil    :initarg :save)
   (item-page       :accessor item-page   :initform 0      :initarg :item-page)
   (getitem         :accessor getitem     :initform nil    :initarg :getitem)
   (turn            :accessor turn        :initform :ally  :initarg :turn)
   (prestate        :accessor prestate    :initform nil    :initarg :prestate)
   (state           :accessor state       :initform :title :initarg :state)))     ;;武器


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


(defclass p-warrior (unit)
  ())

(defmethod initialize-instance :after ((e p-warrior) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move weapon armor team level
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "戦士"
	  tec 7 con 10 mnd 4
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hit-value (+ level dex-bonus) avoid-value (+ level agi-bonus)
	  hp (+ vit 3) mp res
	  team :player
	  weapon (item-make +w_knife+)
	  armor (item-make +a_cloth_armor+)
	  move 4
	  movecost  #(1 -1 -1 2 2 3 -1 1 1)
	  lvuprate '(:hp 90 :str 60 :vit 70 :agi 30 :int 10 :res 30)
	  canequip  '(:sword :spear :ax :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-warrior+))
	  id :warrior)
    ))

(defclass p-sorcerer (unit)
  ())

(defmethod initialize-instance :after ((e p-sorcerer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move team weapon armor level magic-power
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "魔術師"
	  tec 7 con 4 mnd 10
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hit-value (+ 0 dex-bonus) avoid-value (+ 0 agi-bonus)
	  magic-power (+ level int-bonus)
	  hp (+ vit 3) mp (+ res 3) 
	  move 3
	  team :player
	  weapon (item-make +w_mage_staff+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 -1 -1 1 1)
	  lvuprate '(:hp 60 :str 10 :vit 30 :agi 20 :int 90 :res 60)
	  canequip  '(:wand :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-sorcerer+))
	  id :sorcerer)))


(defclass p-priest (unit)
  ())

(defmethod initialize-instance :after ((e p-priest) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move team weapon armor level magic-power
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "僧侶"
	  tec 4 con 8 mnd 9
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hit-value (+ 0 dex-bonus) avoid-value (+ 0 agi-bonus)
	  magic-power (+ level int-bonus)
	  hp (+ vit 3) mp (+ res 3) 
	  move 3
	  team :player
	  weapon (item-make +w_mage_staff+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 -1 -1 1 1)
	  lvuprate '(:hp 60 :str 10 :vit 30 :agi 30 :int 80 :res 90)
	  canequip  '(:staff :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-priest+))
	  id :priest)))

(defclass p-archer (unit)
  ())

(defmethod initialize-instance :after ((e p-archer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move team weapon armor level
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "射手"
	  tec 8 con 4 mnd 9
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hit-value (+ level dex-bonus) avoid-value (+ level agi-bonus)
	  hp (+ vit 3) mp res
	  move 3
	  team :player
	  weapon (item-make +w_short_bow+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 -1 -1 1 1)
	  lvuprate '(:hp 70 :str 60 :vit 50 :agi 60 :int 10 :res 40)
	  canequip  '(:bow :item :armor)
	  origin (gk:vec2 0 (* 32 +img-p-archer+))
	  id :archer)))

(defclass p-s-knight (unit)
  ())

(defmethod initialize-instance :after ((e p-s-knight) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move weapon team armor level 
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "騎士"
	  tec (dice 2 6) con (dice 2 6) mnd (dice 2 6)
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hp (+ vit 3) mp res
	  hit-value (+ level dex-bonus) avoid-value (+ level agi-bonus)
	  move 5
	  team :player
	  weapon (item-make +w_javelin+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 2 2 3 -1 1 1)
	  lvuprate '(:hp 80 :str 90 :vit 70 :agi 40 :int 10 :res 40)
	  canequip  '(:spear :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-s-knight+))
	  id :s-knight)))

(defclass p-p-knight (unit)
  ())

(defmethod initialize-instance :after ((e p-p-knight) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move weapon team armor level
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "天馬騎士"
	  tec (dice 2 6) con (dice 2 6) mnd (dice 2 6)
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hp (+ vit 3) mp res
	  hit-value (+ level dex-bonus) avoid-value (+ level agi-bonus)
	  move 5
	  team :player
	  weapon (item-make +w_javelin+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 1 1 1 1 1 1)
	  lvuprate '(:hp 80 :str 60 :vit 70 :agi 60 :int 10 :res 80)
	  canequip  '(:spear :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-p-knight+))
	  id :p-knight)))

(Defclass p-thief (unit)
  ())

(defmethod initialize-instance :after ((e p-thief) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost lvuprate canequip id origin move weapon team armor level
	       hp mp str vit con dex tec mnd int res agi str-bonus agi-bonus int-bonus res-bonus vit-bonus dex-bonus
	       hit-value avoid-value) e
    (setf job-name "盗賊"
	  tec 10 con 7 mnd 4
	  dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6))
	  str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hp (+ vit 3) mp res
	  hit-value (+ level dex-bonus) avoid-value (+ level agi-bonus)
	  move 4
	  team :player
	  weapon (item-make +w_knife+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 2 3 3 2 1 1)
	  lvuprate '(:hp 70 :str 60 :vit 50 :agi 80 :int 10 :res 40)
	  canequip  '(:sword :item :armor :shield)
	  origin (gk:vec2 0 (* 32 +img-p-thief+))
	  id :thief)))

;;ジョブデータ
(defclass jobdesc ()
  ((name      :accessor name      :initform nil :initarg :name)
   (img       :accessor img       :initform 0   :initarg :img)
   (id        :accessor id        :initform 0   :initarg :id)
   (canequip  :accessor canequip  :initform nil :initarg :canequip)
   (move      :accessor move      :initform 0   :initarg :move)
   (lvuprate  :accessor lvuprate  :initform 0   :initarg :lvuprate)
   (movecost  :accessor movecost  :initform nil :initarg :movecost)))


(defparameter *move-cost* (make-array 50 :initial-element 0))
(setf (aref *move-cost* 40) 10
      (aref *move-cost* 30) 10
      (aref *move-cost* 0)   0)


(defparameter *init-class-list*
  '(:warrior :sorcerer :priest :archer :knight :thief :p-knight))

(defparameter *show-class*
  '(:warrior "戦士" :sorcerer "魔術師" :priest "僧侶" :archer "射手" :knight "騎士" :thief "盗賊"
    :p-knight "天馬騎士"))

(my-enum +job_warrior+ +job_sorcerer+ +job_priest+ +job_archer+ +job_s_knight+ +job_thief+
	 +job_p_knight+
	 +job_brigand+ +job_dragon+ +job_hydra+ +job_yote1+ +job_orc+ +job_slime+ +job_goron+
	 +job_max+)
;;ジョブ
;; (my-enum +job_lord+ +job_paradin+ +job_s_knight+ +job_a_knight+ +job_archer+
;; 	 +job_p_knight+ +job_pirate+ +job_hunter+ +job_thief+ +job_bandit+
;; 	 +job_d_knight+ +job_shogun+ +job_mercenary+ +job_yusha+ +job_max+)

;;movecost= (草原 壁 弱壁 森 低山 高山 水 砦 階段)
(defparameter *jobdescs*
  (make-array +job_max+ :initial-contents
	      (list (make-instance 'jobdesc :name "戦士" :move 2 :img +job_warrior+
				   :canequip '(:sword :spear :ax :item :armor)
				   :lvuprate '(:hp 90 :str 60 :vit 70 :agi 30 :int 10 :res 30)
				   :movecost #(1 -1 -1 2 2 3 -1 1 1) :id :warrior)
		    (make-instance 'jobdesc :name "魔術師" :move 2 :img +job_sorcerer+
				   :canequip '(:wand :item :armor)
				   :lvuprate '(:hp 60 :str 10 :vit 30 :agi 20 :int 90 :res 60)
				   :movecost #(1 -1 -1 2 2 -1 -1 1 1) :id :sorcerer)
		    (make-instance 'jobdesc :name "僧侶" :move 2 :img +job_priest+
				   :canequip '(:staff :item :armor)
				   :lvuprate '(:hp 60 :str 10 :vit 30 :agi 30 :int 80 :res 90)
				   :movecost #(1 -1 -1 2 2 -1 -1 1 1) :id :priest)
		    (make-instance 'jobdesc :name "射手" :move 2 :img +job_archer+
				   :canequip '(:bow :item :armor)
				   :lvuprate '(:hp 70 :str 60 :vit 50 :agi 60 :int 10 :res 40)
				   :movecost #(1 -1 -1 2 2 -1 -1 1 1) :id :archer)
		    (make-instance 'jobdesc :name "騎士" :move 3 :img +job_s_knight+
				   :canequip '(:spear :item :armor)
				   :lvuprate '(:hp 80 :str 90 :vit 70 :agi 40 :int 10 :res 40)
				   :movecost #(1 -1 -1 2 2 3 -1 1 1) :id :s_knight)
		    (make-instance 'jobdesc :name "盗賊" :move 3 :img +job_thief+
				   :canequip '(:sword :item :armor)
				   :lvuprate '(:hp 70 :str 60 :vit 50 :agi 80 :int 10 :res 40)
				   :movecost #(1 -1 -1 2 3 3 2 1 1) :id :thief)
		    (make-instance 'jobdesc :name "天馬騎士" :move 3 :img +job_p_knight+
				   :canequip '(:spear :item :armor)
				   :lvuprate '(:hp 80 :str 60 :vit 70 :agi 60 :int 10 :res 80)
				   :movecost #(1 -1 -1 1 1 1 1 1 1) :id :p_knight)
		    ;;敵
		    (make-instance 'jobdesc :name "ブリガンド" :move 2 :img +job_brigand+
				   :canequip '(:bow :item :armor)
				   :lvuprate '(:hp 70 :str 60 :vit 50 :agi 40 :int 10 :res 40)
				   :movecost #(1 -1 -1 2 2 3 -1 2 1) :id :brigand)
		    (make-instance 'jobdesc :name "ドラゴン" :move 3 :img +job_dragon+
				   :canequip '(:item :armor)
				   :lvuprate '(:hp 90 :str 60 :vit 80 :agi 30 :int 60 :res 60)
				   :movecost #(1 -1 -1 2 2 2 2 1 1) :id :dragon)
		    (make-instance 'jobdesc :name "ヒドラ" :move 2 :img +job_hydra+
				   :canequip '(:item :armor)
				   :lvuprate '(:hp 90 :str 50 :vit 80 :agi 20 :int 10 :res 30)
				   :movecost #(1 -1 -1 2 3 3 1 1 1) :id :hydra)
		    (make-instance 'jobdesc :name "メタルよていち" :move 3 :img +job_yote1+
				   :canequip '(:item :armor)
				   :lvuprate '(:hp 0 :str 20 :vit 90 :agi 90 :int 10 :res 90)
				   :movecost #(1 -1 -1 1 1 1 1 1 1) :id :yote1)
		    (make-instance 'jobdesc :name "オーク" :move 2 :img +job_orc+
				   :canequip '(:spear :item :armor)
				   :lvuprate '(:hp 70 :str 90 :vit 70 :agi 20 :int 10 :res 20)
				   :movecost #(1 -1 -1 2 2 3 -1 1 1) :id :orc)
		    (make-instance 'jobdesc :name "スライム" :move 2 :img +job_slime+
				   :canequip '(:spear :item :armor)
				   :lvuprate '(:hp 50 :str 30 :vit 40 :agi 30 :int 10 :res 30)
				   :movecost #(1 -1 -1 2 2 2 2 1 1) :id :slime)
		    (make-instance 'jobdesc :name "ゴロン" :move 2 :img +job_goron+
				   :canequip '(:item :armor)
				   :lvuprate '(:hp 50 :str 50 :vit 50 :agi 30 :int 10 :res 30)
				   :movecost #(1 -1 -1 2 2 1 1 1 1) :id :goron)
		    )))



(defclass skill ()
  ((name       :accessor name      :initform nil :initarg :name)
   (rangemin   :accessor rangemin  :initform 0   :initarg :rangemin)
   (rangemax   :accessor rangemax  :initform 0   :initarg :rangemax)
   (atking-type    :accessor atking-type   :initform :atk :initarg :atking-type)
   (r   :accessor r  :initform 0   :initarg :r)
   (scope   :accessor scope  :initform nil   :initarg :scope)
   (temparea   :accessor temparea  :initform nil   :initarg :temparea)
   (range   :accessor range  :initform nil   :initarg :range)
   (target   :accessor target  :initform 0   :initarg :target)
   (element   :accessor element  :initform 0   :initarg :element)
   (power   :accessor power  :initform 0   :initarg :power)
   (dmg-table   :accessor dmg-table  :initform 0   :initarg :dmg-table)
   (critical   :accessor critical  :initform 0   :initarg :critical)
   (mp   :accessor mp  :initform 0   :initarg :mp)
   (status   :accessor status  :initform 0   :initarg :status)
   (depend   :accessor depend  :initform 0   :initarg :depend)
   (origin   :accessor origin  :initform 0   :initarg :origin)
   (pos   :accessor pos  :initform 0   :initarg :pos)
   (img   :accessor img  :initform 0   :initarg :img)
   (frame   :accessor frame  :initform 0   :initarg :frame)
   (max-frame   :accessor max-frame  :initform 0   :initarg :max-frame)
   (interval   :accessor interval  :initform 0   :initarg :interval)
   (team   :accessor team  :initform 0   :initarg :team)
   ))

(defclass itemdesc ()
  ((name       :accessor name      :initform nil :initarg :name)
   (rarity       :accessor rarity      :initform :normal :initarg :rarity)
   (price      :accessor price     :initform 0   :initarg :price)
   (new        :accessor new       :initform nil :initarg :new)
   (level      :accessor level   :initform 0 :initarg :level)
   (category   :accessor category  :initform nil :initarg :category)
   (equiped    :accessor equiped   :initform nil :initarg :equiped)
   (damage     :accessor damage    :initform 0   :initarg :damage)
   (hit        :accessor hit       :initform 0   :initarg :hit)
   (atktype    :accessor atktype   :initform :short :initarg :atktype)
   (tokkou     :accessor tokkou    :initform 0   :initarg :tokkou)
   (critical   :accessor critical  :initform 0   :initarg :critical)
   (rangemin   :accessor rangemin  :initform 0   :initarg :rangemin)
   (rangemax   :accessor rangemax  :initform 0   :initarg :rangemax)
   (avoid        :accessor avoid      :initform 0 :initarg :avoid)
   (def        :accessor def       :initform 0   :initarg :def)
   (hand        :accessor hand       :initform :1h   :initarg :hand)
   (required-str        :accessor required-str       :initform 0   :initarg :required-str)
   (itemnum        :accessor itemnum      :initform nil :initarg :itemnum)))

(defclass weapondesc (itemdesc)
  ((dmg-table        :accessor dmg-table       :initform 0   :initarg :dmg-table)))


(defclass armordesc (itemdesc)
  ())

(defclass shielddesc (itemdesc)
  ())



(defun weapon-make (item)
  (make-instance 'weapondesc :name (getf item :name) :damage (getf item :damage)
		 :hit (getf item :hit) :critical (getf item :critical) :categoly (getf item :categoly)
		 :rangemin (getf item :rangemin) :rangemax (getf item :rangemax)
		 :price (getf item :price) :atktype (getf item :atktype)))

(defun armor-make (item)
  (make-instance 'armordesc :name (getf item :name) :def (getf item :def)
		 :categoly (getf item :categoly)
		 :blk (getf item :blk) :price (getf item :price)))


;;ブラシ生成
(defun set-brush ()
  (setf *brush* (make-array 8 :initial-contents
                              (list
                                (create-solid-brush (encode-rgb 128 0 255))
                                (create-solid-brush (encode-rgb 255 0 0))
                                (create-solid-brush (encode-rgb 1 255 0))
                                (create-solid-brush (encode-rgb 0 0 255))
                                (create-solid-brush (encode-rgb 255 255 0))
                                (create-solid-brush (encode-rgb 0 255 255))
                                (create-solid-brush (encode-rgb 255 0 255))
				(create-solid-brush (encode-rgb 255 255 255))))))

;;font----------------------------------------------------------
(defvar *font12* nil)
(defvar *font14* nil)
(defvar *font16* nil)
(defvar *font18* nil)
(defvar *font20* nil)
(defvar *font24* nil)
(defvar *font28* nil)
(defvar *font32* nil)
(defvar *font48* nil)
(defvar *font64* nil)
(defvar *font128* nil)
(defvar *font256* nil)
(defvar *font300* nil)


(defun set-font ()
  (setf *font24* (gk:make-font :mplus 24)
	*font18* (gk:make-font :mplus 18)
	*font20* (gk:make-font :mplus 20)
	*font16* (gk:make-font :mplus 16)
	*font14* (gk:make-font :mplus 14)
	*font12* (gk:make-font :mplus 12)
        *font28* (gk:make-font :mplus 28)
        *font32* (gk:make-font :mplus 32)
	*font48* (gk:make-font :mplus 48)
        *font64* (gk:make-font :mplus 64)
        *font128* (gk:make-font :mplus 128)
	*font256* (gk:make-font :mplus 256)
        *font300* (gk:make-font :mplus 300)))






;;ジョブデータ取得
(defun get-job-data (job ability)
  (case ability
    (:name     (name     (aref *jobdescs* job)))
    (:move     (move     (aref *jobdescs* job)))
    (:movecost (movecost (aref *jobdescs* job)))
    (:canequip (canequip (aref *jobdescs* job)))
    (:lvuprate (lvuprate (aref *jobdescs* job)))
    (:img      (img      (aref *jobdescs* job)))))


(defparameter *default-damage-table-list*
  '((0 0 0 0 0 0 1 2 2 3 3 4 4)
    (0 0 0 0 0 0 1 2 3 3 3 4 4)
    (0 0 0 0 0 0 1 2 3 4 4 4 4)
    (0 0 0 0 0 1 1 2 3 4 4 4 5)
    (0 0 0 0 0 1 2 2 3 4 4 5 5)
    (0 0 0 0 1 1 2 2 3 4 5 5 5)
    (0 0 0 0 1 1 2 3 3 4 5 5 5)
    (0 0 0 0 1 1 2 3 4 4 5 5 6)
    (0 0 0 0 1 2 2 3 4 4 5 6 6)
    (0 0 0 0 1 2 3 3 4 4 5 6 7)
    (0 0 0 1 1 2 3 3 4 5 5 6 7)
    (0 0 0 1 2 2 3 3 4 5 6 6 7)
    (0 0 0 1 2 2 3 4 4 5 6 6 7)
    (0 0 0 1 2 3 3 4 4 5 6 7 7)
    (0 0 0 1 2 3 4 4 4 5 6 7 8)
    (0 0 0 1 2 3 4 4 5 5 6 7 8)
    (0 0 0 1 2 3 4 4 5 6 7 7 8)
    (0 0 0 1 2 3 4 5 5 6 7 7 8)
    (0 0 0 1 2 3 4 5 6 6 7 7 8)
    (0 0 0 1 2 3 4 5 6 7 7 8 9)
    (0 0 0 1 2 3 4 5 6 7 8 9 10)
    (0 0 0 1 2 3 4 6 6 7 8 9 10)
    (0 0 0 1 2 3 5 6 6 7 8 9 10)
    (0 0 0 2 2 3 5 6 7 7 8 9 10)
    (0 0 0 2 3 4 5 6 7 7 8 9 10)
    (0 0 0 2 3 4 5 6 7 8 8 9 10)
    (0 0 0 2 3 4 5 6 8 8 9 9 10)
    (0 0 0 2 3 4 6 6 8 8 9 9 10)
    (0 0 0 2 3 4 6 6 8 9 9 10 10)
    (0 0 0 2 3 4 6 7 8 9 9 10 10)
    (0 0 0 2 4 4 6 7 8 9 10 10 10)
    (0 0 0 2 4 5 6 7 8 9 10 10 11)
    (0 0 0 3 4 5 6 7 8 10 10 10 11)
    (0 0 0 3 4 5 6 8 8 10 10 10 11)
    (0 0 0 3 4 5 6 8 9 10 10 11 11)
    (0 0 0 3 4 5 7 8 9 10 10 11 12)
    (0 0 0 3 5 5 7 8 9 10 11 11 12)
    (0 0 0 3 5 6 7 8 9 10 11 12 12)
    (0 0 0 3 5 6 7 8 10 10 11 12 13)
    (0 0 0 4 5 6 7 8 10 11 11 12 13)
    (0 0 0 4 5 6 7 9 10 11 11 12 13)
    (0 0 0 4 6 6 7 9 10 11 12 12 13)
    (0 0 0 4 6 7 7 9 10 11 12 13 13)
    (0 0 0 4 6 7 8 9 10 11 12 13 14)
    (0 0 0 4 6 7 8 10 10 11 12 13 14)
    (0 0 0 4 6 7 9 10 10 11 12 13 14)
    (0 0 0 4 6 7 9 10 10 12 13 13 14)
    (0 0 0 4 6 7 9 10 11 12 13 13 15)
    (0 0 0 4 6 7 9 10 12 12 13 13 15)
    (0 0 0 4 6 7 10 10 12 12 13 14 15)
    (0 0 0 4 6 8 10 10 12 12 13 15 15)))

;;　スキル
(my-enum +heal+ +fire+)

(defparameter *skill-list*
  `(:heal ,(make-instance 'skill :name "ヒール" :target :ally :r 1 :mp 3 :rangemin 1 :rangemax 5
				 :element :holy :power 10 :depend :int :img :skill-img :origin (gk:vec2 0 (* +heal+ 32))
				 :max-frame 100 :interval 20 :atking-type :magic :critical 99
				 :dmg-table (nth 10 *default-damage-table-list*))
    :fire ,(make-instance 'skill :name "ファイア" :target :enemy :r 2 :mp 3 :rangemin 1 :rangemax 5
				 :element :fire :power 10 :depend :int :img :skill-img :origin (gk:vec2 0 (* +fire+ 32))
				 :max-frame 120 :interval 20 :atking-type :magic
				 :critical 10 :dmg-table (nth 10 *default-damage-table-list*))
    ))



;; (defun get-cell-data (cell data)
;;   (case data
;;     (:heal    (heal  (aref *celldescs* cell)))
;;     (:def     (def   (aref *celldescs* cell)))
;;     (:avoid   (avoid (aref *celldescs* cell)))
;;     (:name    (name  (aref *celldescs* cell)))))
;;-------------------------------------------------------------------
;;武器データ
;; (defstruct weapondesc2
;;   (name   nil)
;;   (price    0) ;;価格
;;   (num      0) ;;使用可能回数
;;   (damage   0)
;;   (weight   0)
;;   (hit      0)
;;   (tokkou   nil)
;;   (critical 0)
;;   (rangeMin 0)
;;   (rangeMax 0))









;武器データ配列
;; (defparameter *weapondescs*
;;   (make-array +w_max+ :initial-contents
;;         (list (make-instance 'weapondesc :name "鉄の剣" :damage 5
;;                                :hit 100 :critical 0 :rangemin 1
;;                                :rangemax 1 :price 320)
;;               (make-instance 'weapondesc :name "レイピア" :damage 5
;; 			       :hit 100 :critical 10 :rangemin 1
;; 			       :tokkou (list +job_paradin+ +job_a_knight+ +job_s_knight+
;; 					     +job_shogun+)
;; 			       :rangemax 1 :price 9999)
;;               (make-instance 'weapondesc :name "やり" :damage 8
;; 			       :hit 80 :critical 0 :rangemin 1
;; 			       :rangemax 1 :price 450)
;;               (make-instance 'weapondesc :name "銀の槍" :damage 12
;; 			       :hit 80 :critical 0 :rangemin 1
;; 			       :rangemax 1 :price 1800)
;;               (make-instance 'weapondesc :name "てやり" :damage 7
;; 			       :hit 70 :critical 0 :rangemin 1
;; 			       :rangemax 2 :price 820)
;;               (make-instance 'weapondesc :name "ゆみ" :damage 4
;; 			       :hit 90 :critical 0 :rangemin 2
;; 			       :tokkou (list +job_p_knight+ +job_d_knight+)
;; 			       :rangemax 2 :price 400)
;;               (make-instance 'weapondesc :name "鋼の弓" :damage 7
;; 			       :hit 80 :critical 0 :rangemin 2
;; 			       :tokkou (list +job_p_knight+ +job_d_knight+)
;; 			       :rangemax 2 :price 560)
;;               (make-instance 'weapondesc :name "ボウガン" :damage 5
;; 			       :hit 100 :critical 20 :rangemin 2
;; 			       :tokkou (list +job_p_knight+ +job_d_knight+)
;; 			       :rangemax 2 :price 950)
;;               (make-instance 'weapondesc :name "おの" :damage 7
;; 			       :hit 80 :critical 0 :rangemin 1
;; 			       :rangemax 1 :price 360)
;;               (make-instance 'weapondesc :name "鋼の斧" :damage 9
;; 			       :hit 70 :critical 0 :rangemin 1
;; 			       :rangemax 1 :price 550)
;; 	      (make-instance 'weapondesc :name "銀の剣" :damage 12
;; 			       :hit 100 :critical 0 :rangemin 1
;; 			       :rangemax 1 :price 2000)
;; 	      (make-instance 'weapondesc :name "アーマーキラー" :damage 5
;; 			       :hit 80 :critical 0 :rangemin 1
;; 			       :tokkou (list +job_a_knight+ +job_shogun+)
;; 			       :rangemax 1 :price 760)
;; 	      (make-instance 'weapondesc :name "ナイトキラー" :damage 5
;; 			       :hit 90 :critical 0 :rangemin 1
;; 			       :tokkou (list +job_s_knight+)
;; 			       :rangemax 1 :price 820)
;; 	      (make-instance 'weapondesc :name "ハンマー" :damage 6
;; 			       :hit 70 :critical 0 :rangemin 1
;; 			       :tokkou (list +job_a_knight+ +job_shogun+)
;; 			       :rangemax 1 :price 300)
;; 	      (make-instance 'weapondesc :name "ドラゴンキラー" :damage 6
;; 			       :hit 80 :critical 0 :rangemin 1
;; 			       :tokkou (list +job_d_knight+)
;; 			       :rangemax 1 :price 5000)
;; 	      (make-instance 'weapondesc :name "ライブ" :damage 0
;; 			       :hit 100 :critical 0 :rangemin 1 :atktype :heal
;; 			       :rangemax 1 :price 99999)
;; 	      (make-instance 'weapondesc :name "傷薬" :damage 0
;; 			       :hit 100 :critical 0 :rangemin 1 :atktype :heal
;; 			       :rangemax 1 :price 220)
;; 	      (make-instance 'weapondesc :name "コブシ" :damage 1
;; 			       :hit 100 :critical 0 :rangemin 1 :atktype :atk
;; 			       :rangemax 1 :price 0)
;; 	      ;;鎧
;; 	      (armor-make "服" 1 0 10)
;; 	      (armor-make "皮の鎧" 2 0 50)
;; 	      (armor-make "鉄の鎧" 3 0 100)
;; 	      ;;盾
;; 	      (armor-make "皮の盾" 0 5 30)
;; 	      (armor-make "鉄の盾" 1 8 80)
;; 	      )))
