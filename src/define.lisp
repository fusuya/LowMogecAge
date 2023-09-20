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
;;(gk:register-resource-package :keyword "./assets/")


(gk:define-font :mplus "font/mplus-1mn-regular.ttf")

(gk:define-image :job-img "img/job-img.png")
(gk:define-image :obj-img "img/new-objs-img.png")
(gk:define-image :world-map "img/worldmap.png")
(gk:define-image :skill-img "img/skill.png")
(gk:define-image :monster-img "img/monster-img.png")
;;効果音
(gk:define-sound :buy "sound/chest.wav")
(gk:define-sound :heal "sound/heal-mogec.wav")
(gk:define-sound :fire "sound/fire.wav")
(gk:define-sound :cancel "sound/cancel-mogec.wav")
(gk:define-sound :encount "sound/battle-start.wav")
(gk:define-sound :normal-attack "sound/spear90.wav")
(gk:define-sound :walk "sound/move90.wav")
(gk:define-sound :button "sound/button.wav")
(gk:define-sound :monster-dead "sound/enemy-dead.wav")
(gk:define-sound :cash-exchange "sound/cash-exchange.wav")
(gk:define-sound :miss "sound/miss.wav")

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

(defparameter *battle-obj-w* (* *scale-obj-w* *origin-obj-w*))
(defparameter *battle-obj-h* (* *scale-obj-h* *origin-obj-h*))


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

(defparameter *white* (gk:vec4 1 1 1 1))
(defparameter *black* (gk:vec4 0 0 0 1))



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
   (font :initarg :font :initform nil :accessor button/font)
   (w :initarg :w :initform nil :accessor button/w)
   (h :initarg :h :initform nil :accessor button/h)
   (char-w :initarg :char-w :initform nil :accessor button/char-w)
   (char-h :initarg :char-h :initform nil :accessor button/char-h)
   (x1 :initarg :x1 :initform nil :accessor button/x1)
   (x2 :initarg :x2 :initform nil :accessor button/x2)
   (y1 :initarg :y1 :initform nil :accessor button/y1)
   (y2 :initarg :y2 :initform nil :accessor button/y2)
   (description :initarg :description :initform nil :accessor button/description)
   (box? :initarg :box? :initform nil :accessor button/box?)
   ))


(defclass status-up-btn (button)
  ((tag       :accessor tag        :initform 0      :initarg :tag)))

(defclass init-jon-btn (button)
  ())

(defclass create-init-party-end-btn (button)
  ())

(defclass temp-init-party-btn (button)
  ())

(defclass game-start-btn (button)
  ())

(defclass game-end-btn (button)
  ())

(defclass equip-item-btn (button)
  ((item       :accessor item        :initform 0      :initarg :item)
   (new        :accessor new        :initform nil      :initarg :new)
   (equiped-unit    :accessor equiped-unit        :initform nil      :initarg :equiped-unit)))

(defclass sale-item-btn (equip-item-btn)
  ((price       :accessor price        :initform 0      :initarg :price)))

(defclass next-item-page (button)
  ())
(defclass prev-item-page (button)
  ())
(defclass end-equip-menu-btn (button)
  ())

(Defclass shop-btn (button)
  ())

(defclass inn-btn (button)
  ())

(defclass cash-exchange-btn (button)
  ())

(defclass end-shop-btn (button)
  ())

(defclass shop-prev-item-page (button)
  ())

(defclass shop-next-item-page (button)
  ())

(defclass recruit-btn (button)
  ())

(defclass end-recruit-btn (button)
  ())

(defclass party-status-confirm-btn (button)
  ())

(defclass quest-btn (button)
  ())

(defclass town-exit-btn (button)
  ())

(defclass recruit-random-unit-btn (button)
  ((unit       :accessor unit        :initform nil      :initarg :unit)))

(defclass show-unit-status-btn (recruit-random-unit-btn)
  ())

(defclass race-btn (status-up-btn)
  ())
(defclass job-btn (button)
  ((job       :accessor job        :initform nil      :initarg :job)))

(defclass back-select-race-btn (button)
  ())

(defclass ability-dice-btn (button)
  ())

(defclass ability-dice-end-btn (button)
  ())

(defclass back-ability-dice-btn (button)
  ())

(defclass back-select-job-btn (button)
  ())

(defclass job-level-up-btn (job-btn)
  ())



(defclass back-ability-dice-btn (button)
  ())

(defclass job-level-up-end-btn (back-ability-dice-btn)
  ())

(defclass init-skill-btn (status-up-btn)
  ((skill-type       :accessor skill-type        :initform nil      :initarg :skill-type)))

(defclass back-to-select-job-level-up-btn (button)
  ())

(defclass back-to-select-init-skill-btn (button)
  ())

(defclass create-init-player-unit-end-btn (button)
  ())

(defclass back-job-level-up-btn  (button)
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

(defclass use-item-btn (command-btn)
  ((item :initarg :item :initform nil :accessor item)))

(defclass change-equip-btn (command-btn)
  ())

(defclass wait-cmd-btn (command-btn)
  ())

(defclass skill-btn (command-btn)
  ((tag :initarg :tag :initform (gk:vec2 0 0) :accessor tag)))



(defclass ability-dice ()
  ((dex-dice :initarg :dex-dice :initform 0 :accessor dex-dice)
   (vit-dice :initarg :vit-dice :initform 0 :accessor vit-dice)
   (agi-dice :initarg :agi-dice :initform 0 :accessor agi-dice)
   (str-dice :initarg :str-dice :initform 0 :accessor str-dice)
   (res-dice :initarg :res-dice :initform 0 :accessor res-dice)
   (int-dice :initarg :int-dice :initform 0 :accessor int-dice)
   (num       :accessor num        :initform 3      :initarg :num)))

;;能力値決定用
(defparameter *ability-dice* (make-instance 'ability-dice))

;;アイテムリスト表示マックス
(defparameter *item-show-max* 20)



(defconstant +action-end+ 2)
(defconstant +swoon+ 3)

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
   (monster-symbol           :accessor game/monster-symbol       :initform nil    :initarg :monster-symbol)
   (cursor          :accessor game/cursor      :initform 0      :initarg :cursor)
   (item            :accessor game/item        :initform nil    :initarg :item)
   (cash-exchange-item  :accessor game/cash-exchange-item        :initform nil    :initarg :cash-exchange-item)
   (bgm             :accessor game/bgm         :initform :on    :initarg :bgm)
   (endtime         :accessor game/endtime     :initform 0      :initarg :endtime)
   (starttime       :accessor game/starttime   :initform 0      :initarg :starttime)
   (save            :accessor game/save        :initform nil    :initarg :save)
   (item-page       :accessor game/item-page   :initform 0      :initarg :item-page)
   (getitem         :accessor game/getitem     :initform nil    :initarg :getitem)
   (selected-unit   :accessor game/selected-unit     :initform nil    :initarg :selected-unit)
   (selected-skill   :accessor game/selected-skill     :initform nil    :initarg :selected-skill)
   (selected-town   :accessor game/selected-town     :initform nil    :initarg :selected-town)
   (turn            :accessor game/turn        :initform :ally  :initarg :turn)
   (money            :accessor game/money        :initform :0  :initarg :money)
   (prestate        :accessor game/prestate    :initform nil    :initarg :prestate)
   (temp-dmg        :accessor game/temp-dmg    :initform nil    :initarg :temp-dmg)
   (dmg-font        :accessor game/dmg-font    :initform nil    :initarg :dmg-font)
   (temp-init-party        :accessor game/temp-init-party    :initform nil    :initarg :temp-init-party)
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
   (translate-x      :accessor translate-x      :initform 0      :initarg :translate-x)
   (translate-y      :accessor translate-y      :initform 0      :initarg :translate-y)
   (origin   :accessor origin      :initform 0      :initarg :origin)
   (img-id   :accessor img-id      :initform nil    :initarg :img-id)))

(defclass unit-btn (obj)
  ())

(defclass monster-symbol (obj)
  ((v      :accessor v      :initform 0     :initarg :v)
   (alive-time      :accessor alive-time      :initform 0     :initarg :alive-time)
   (change-dir-timing      :accessor change-dir-timing      :initform 0     :initarg :change-dir-timing)))

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
   (key6     :accessor key6     :initform nil :initarg :key6)
   (key7     :accessor key7     :initform nil :initarg :key7)
   (key8     :accessor key8     :initform nil :initarg :key8)
   (key9     :accessor key9     :initform nil :initarg :key9)
   (key0     :accessor key0     :initform nil :initarg :key0)
   (z     :accessor z     :initform nil :initarg :z)
   (v     :accessor v     :initform nil :initarg :v)
   (b     :accessor b     :initform nil :initarg :b)
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
   (color    :accessor color     :initform nil   :initarg :color)
   (font     :accessor font      :initform nil :initarg :font)
   ))

(defclass drop-item-font (dmg-font)
  ())

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
   (add-damage       :accessor add-damage       :initform 0    :initarg :add-damage)
   (agi-bonus       :accessor agi-bonus       :initform 0    :initarg :agi-bonus)
   (str-bonus       :accessor str-bonus       :initform 0    :initarg :str-bonus)
   (dex-bonus       :accessor dex-bonus       :initform 0    :initarg :dex-bonus)
   (vit-bonus       :accessor vit-bonus       :initform 0    :initarg :vit-bonus) ;;モンスターの場合生命抵抗力として使う
   (int-bonus       :accessor int-bonus       :initform 0    :initarg :int-bonus) ;;モンスターの場合精神抵抗力として使う
   (res-bonus       :accessor res-bonus       :initform 0    :initarg :res-bonus)
   (hit-value       :accessor hit-value       :initform 0    :initarg :hit-value) ;;命中基準値
   (avoid-value       :accessor avoid-value       :initform 0    :initarg :avoid-value) ;;回避基準値
   (magic-power       :accessor magic-power       :initform 0    :initarg :magic-power) ;;魔力
   (level        :accessor level        :initform 2    :initarg :level)
   (exp-point        :accessor exp-point        :initform 0    :initarg :exp-point)
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

(defparameter *job-level-list* '(:magitec 0 :sorcerer 0 :scout 0 :fencer 0 :fighter 0 :grappler 0 :priest 0
				   :conjurer 0 :sage 0 :ranger 0 :shooter 0))

;;プレイヤー用
(defclass unit (obj status anime)
  ((name        :accessor name        :initform nil :initarg :name)     ;;名前
   (weapon        :accessor weapon        :initform nil :initarg :weapon)
   (shield        :accessor shield        :initform nil :initarg :shield)
   (skill        :accessor skill        :initform nil :initarg :skill)
   (use-item        :accessor use-item        :initform nil :initarg :use-item)
   (selected-cmd        :accessor selected-cmd        :initform nil :initarg :selected-cmd)
   (atked?       :accessor atked?       :initform nil  :initarg :atked?)
   (atking-enemy       :accessor atking-enemy       :initform nil  :initarg :atking-enemy)
   (sight       :accessor sight       :initform 5   :initarg :sight)
   (temparea    :accessor temparea       :initform nil   :initarg :temparea)
   (movearea    :accessor movearea    :initform nil :initarg :movearea)
   (move        :accessor move        :initform nil :initarg :move)
   (movecost    :accessor movecost    :initform nil :initarg :movecost)
   (job         :accessor job         :initform nil :initarg :job) ;;技能
   (job-level-list :accessor job-level-list  :initform (copy-list *job-level-list*)  :initarg :job-level-list)
   (race         :accessor race         :initform nil :initarg :race) ;;種族
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
   (passive-skill  :accessor passive-skill  :initform nil :initarg :passive-skill)
   (declare-skill  :accessor declare-skill  :initform nil   :initarg :declare-skill)
   (action-skill  :accessor action-skill  :initform nil   :initarg :action-skill)
   ))

(defmethod initialize-instance :after ((unit unit) &rest initargs)
  (declare (ignore initargs))
  (with-slots (img-id) unit
    (setf img-id :job-img)))

(defclass e-unit (unit)
  ((drop         :accessor drop       :initform nil :initarg :drop)    ;;ドロップするアイテム
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


;;ユニットのランダム初期ステータス
(defun unit-random-status (unit)
  (with-slots (str dex tec agi con vit mnd res int) unit
    (setf dex (+ tec (dice 2 6)) agi (+ tec (dice 2 6)) str (+ con (dice 2 6)) vit (+ con (dice 2 6))
	  int (+ mnd (dice 2 6)) res (+ mnd (dice 2 6)))))

;;人間の初期技能
(defparameter *human-job-list* '(:human-magitec :human-sorcerer :human-scout&fencer :human-none :human-fighter
				 :human-grappler :human-priest :human-conjurer))
;;エルフの初期技能
(defparameter *elf-job-list* '(:elf-fencer :elf-sage&ranger :elf-priest :elf-conjurer :elf-sorcerer :elf-shooter))
;;ドワーフの初期技能
(defparameter *dwarf-job-list* '(:dwarf-shooter :dwarf-fighter :dwarf-grappler :dwarf-priest :dwarf-magitec))
;;タビットの初期技能
(defparameter *tabbit-job-list* '(:tabbit-conjurer :tabbit-sorcerer :tabbit-sage :tabbit-magitec))
;;ルーンフォークの初期技能
(defparameter *runefolk-job-list* '(:runefolk-sage :runefolk-shooter :runefolk-fighter :runefolk-grappler
				    :runefolk-magitec :runefolk-sorecerer))
;;ナイトメアの初期技能
(defparameter *nightmare-job-list* '(:nightmare-sorecerer :nightmare-fighter :nightmare-grappler :nightmare-fencer&scout
				     :nightmare-priest :nightmare-magitec))
;;リカントの初期技能
(defparameter *lycant-job-list* '(:lycant-scout :lycant-fighter :lycant-grappler :lycant-fencer :lycant-ranger))

;;種族タグリスト
(defparameter *all-race-tag-list* '(:human :elf :dwarf :tabbit :runefolk :nightmare :lycant))

;;種族データリスト movecost= (草原 壁 弱壁 森 低山 高山 水 砦 階段)
(defparameter *all-race-tag-and-data-list*
  `(:human (:name "人間" :description "" :init-job-list ,*human-job-list* :movecost #(1 -1 -1 2 2 2 2 1 1))
    :elf (:name "エルフ" :description "" :init-job-list ,*elf-job-list* :movecost #(1 -1 -1 1 3 3 1 1 1))
    :dwarf (:name "ドワーフ" :description "" :init-job-list ,*dwarf-job-list* :movecost #(1 -1 -1 2 1 1 3 1 1))
    :tabbit  (:name "タビット" :description "" :init-job-list ,*tabbit-job-list* :movecost #(1 -1 -1 1 2 3 3 1 1))
    :runefolk  (:name "ルーンフォーク" :description "" :init-job-list ,*runefolk-job-list* :movecost #(1 -1 -1 2 1 2 3 1 1))
    :nightmare (:name "ナイトメア" :description "" :init-job-list ,*nightmare-job-list* :movecost #(1 -1 -1 2 2 2 2 1 1))
    :lycant (:name "リカント" :description "" :init-job-list ,*lycant-job-list* :movecost #(1 -1 -1 1 2 2 2 1 1))))

;;回避判定に使うジョブ
(defparameter *avoid-job-list* '(:fighter :grappler :fencer))
;;戦士系ジョブ
(defparameter *worrior-job-list* '(:fighter :grappler :fencer :shooter))
;;魔法使い系
(defparameter *magic-job-list* '(:sorcerer :priest :conjurer :magitec))
;;全てのジョブ
(defparameter *all-job-list* '(:fighter :grappler :fencer :shooter :sorcerer :priest :conjurer :magitec
			       :sage :scout :ranger))


;;特定のジョブレベルの最大値値取得
(defun get-judgment-max-value-by-job (job-level-list job-list)
  (loop :for job :in job-list
	:maximize (getf job-level-list job)))

;;特定のジョブレベルの合計値取得
(defun get-judgment-sum-value-by-job (job-level-list job-list)
  (loop :for job :in job-list
	:sum (getf job-level-list job)))


;;ユニットステータスの調整 todo
(defun unit-status-adjust (unit)
  (with-slots (hp maxhp mp maxmp str-bonus dex-bonus int-bonus agi-bonus res-bonus level job-level-list
 	       vit-bonus hit-value avoid-value magic-power str dex agi int res vit id add-damage) unit
    (setf str-bonus (get-ability-bonus str) agi-bonus (get-ability-bonus agi) int-bonus (get-ability-bonus int)
	  res-bonus (get-ability-bonus res) vit-bonus (get-ability-bonus vit) dex-bonus (get-ability-bonus dex)
	  hit-value (+ (get-judgment-max-value-by-job job-level-list *worrior-job-list*) dex-bonus)
	  avoid-value (+ (get-judgment-max-value-by-job job-level-list *avoid-job-list*) agi-bonus)
	  magic-power (+ (get-judgment-max-value-by-job job-level-list *magic-job-list*) int-bonus)
	  add-damage (+ (get-judgment-max-value-by-job job-level-list *worrior-job-list*) str-bonus)
	  hp (+ vit (* 3 (get-judgment-max-value-by-job job-level-list *all-job-list*)))
	  mp (+ res (* 3 (get-judgment-sum-value-by-job job-level-list *magic-job-list*)))
	  maxhp hp maxmp mp)))


(defclass p-fighter (unit)
  ())

(defmethod initialize-instance :after ((e p-fighter) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "戦士"
	  tec 7 con 10 mnd 4
	  team :player
	  weapon (item-make +w_bastard-sword+)
	  armor (item-make +a_cloth_armor+)
	  move 4
	  movecost  #(1 -1 -1 2 2 3 3 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-warrior+))
	  translate-y (- (* *battle-obj-h* +img-p-warrior+))
	  id :fighter)
    (unit-random-status e)
    (unit-status-adjust e)
    ))

(defclass p-sorcerer (unit)
  ())

(defmethod initialize-instance :after ((e p-sorcerer) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "魔術師"
	  tec 7 con 4 mnd 10
	  move 3
	  team :player
	  weapon (item-make +w_mage_staff+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 -1 3 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-sorcerer+))
	  translate-y (- (* *battle-obj-h* +img-p-sorcerer+))
	  id :sorcerer)
    (unit-random-status e)
    (unit-status-adjust e)
    ))


(defclass p-priest (unit)
  ())

(defmethod initialize-instance :after ((e p-priest) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "僧侶"
	  tec 4 con 8 mnd 9
	  move 3
	  team :player
	  weapon (item-make +w_mage_staff+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 3 3 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-priest+))
	  translate-y (- (* *battle-obj-h* +img-p-priest+))
	  id :priest)
    (unit-random-status e)
    (unit-status-adjust e)))

(defclass p-ranger (unit)
  ())

(defmethod initialize-instance :after ((e p-ranger) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "射手"
	  tec 8 con 4 mnd 9
	  move 3
	  team :player
	  weapon (item-make +w_short_bow+)
	  armor (item-make +a_cloth_armor+)
	  movecost  #(1 -1 -1 2 2 3 3 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-archer+))
	  translate-y (- (* *battle-obj-h* +img-p-archer+))
	  id :ranger)
    (unit-random-status e)
    (unit-status-adjust e)))

(defclass p-s-knight (unit)
  ())

(defmethod initialize-instance :after ((e p-s-knight) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "騎士"
	  tec 8 con 9 mnd 5
	  move 5
	  team :player
	  weapon (item-make +w_javelin+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 2 2 3 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-s-knight+))
	  translate-y (- (* *battle-obj-h* +img-p-s-knight+))
	  id :s-knight)
    (unit-random-status e)
    (unit-status-adjust e)))

(defclass p-p-knight (unit)
  ())

(defmethod initialize-instance :after ((e p-p-knight) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "天馬騎士"
	  tec (dice 2 6) con (dice 2 6) mnd (dice 2 6)
	  move 5
	  team :player
	  weapon (item-make +w_javelin+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 1 1 1 1 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-p-knight+))
	  translate-y (- (* *battle-obj-h* +img-p-p-knight+))
	  id :p-knight)
    (unit-random-status e)
    (unit-status-adjust e)))

(Defclass p-scout (unit)
  ())

(defmethod initialize-instance :after ((e p-scout) &rest initargs)
  (declare (ignore initargs))
  (with-slots (job-name movecost id origin move weapon armor team con tec mnd translate-y) e
    (setf job-name "盗賊"
	  tec 10 con 7 mnd 4
	  move 4
	  team :player
	  weapon (item-make +w_knife+)
	  armor (item-make +a_cloth_armor+)
	  movecost   #(1 -1 -1 2 2 2 2 1 1)
	  origin (gk:vec2 0 (* 32 +img-p-thief+))
	  translate-y (- (* *battle-obj-h* +img-p-thief+))
	  id :scout)
    (unit-random-status e)
    (unit-status-adjust e)))

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

(defparameter *jon-name-list*
  '("戦士" "魔術師" "神官" "射手" "騎士" "天馬騎士" "盗賊"))

(defparameter *show-class*
  '(:fighter "戦士" :sorcerer "魔術師" :priest "僧侶" :ranger "射手" :s-knight "騎士" :scout "盗賊"
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

(defclass shielddesc (armordesc)
  ())

(defclass cash-exchange (itemdesc)
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



;;font----------------------------------------------------------
(defvar *font12* nil)
(defvar *font14* nil)
(defvar *font16* nil)
(defvar *font18* nil)
(defvar *font20* nil)
(defvar *font24* nil)
(defvar *font28* nil)
(defvar *font32* nil)
(defvar *font40* nil)
(defvar *font48* nil)
(defvar *font56* nil)
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
	*font40* (gk:make-font :mplus 40)
	*font48* (gk:make-font :mplus 48)
	*font56* (gk:make-font :mplus 56)
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
(my-enum +heal+ +fire+ +first-aid+ +skill-max+)

;;ボタン説明分
(defparameter *btn-description*
  '(:wait "待機:行動終了"
    :change-equip "装備変更:装備変更画面に切り替える"
    :normal-attack "通常攻撃:普通の攻撃"
    :skill "スキル:なんかスキル使う"
    :use-item "アイテム：なんかアイテム使う"
    :normal-move "通常移動:移動後に行動できる"
    :fast-move "全力移動:移動距離は長いが、移動後に行動できない"
    :fire "ファイア：ふぁいあ！"
    :heal "ヒール：回復魔法 気絶状態の味方には使えない"
    :first-aid "応急手当：気絶状態の味方をHP1で起こす、かもしれない"
    :healing-potion "回復薬：HPを回復する"
    :magic-perfume "魔香水：MPを回復する"
    ))

(defparameter *init-skill-tag-list*
  '(:slip-through :targeting :weapon-expert-a-sword :weapon-expert-a-axe :weapon-expert-a-spear :weapon-expert-a-staff
    :weapon-expert-a-bow :armor-expert-a-light-armor :armor-expert-a-heavy-armor :armor-expert-a-shield :ambidexterity
    :decoy-attack-1 :protect-1 :cut-back-1 :diversionary-attack-1 :all-out-attack-1 :provocation-attack-1
    :defence-stance :violent-cast-1 :special-attack-1 :me-power-certainty :me-certainty :me-number :me-distance
    :me-time :me-area :magic-convergence :magic-hit :armor-piercing-1 :snipe :word-break))

(defparameter *all-skill-tag-and-data*
  '(:slip-through (:name "かいくぐり" :skill-type :passive :description "盾を装備して近接攻撃を回避すると、次の攻撃がクリティカル発生しやすくなる")
    :targeting (:name "ターゲッティング" :skill-type :passive :description "誤射を防ぐ")
    :weapon-expert-a-sword (:name "武器習熟A/剣" :skill-type :passive :description "Aランクの剣を装備できるようになり、剣のダメージ＋１")
    :weapon-expert-a-axe (:name "武器習熟A/斧" :skill-type :passive :description "Aランクの斧を装備できるようになり、斧のダメージ＋１")
    :weapon-expert-a-spear (:name "武器習熟A/槍" :skill-type :passive :description "Aランクの槍を装備できるようになり、槍のダメージ＋１")
    :weapon-expert-a-staff (:name "武器習熟A/杖" :skill-type :passive :description "Aランクの杖を装備できるようになり、杖のダメージ＋１")
    :weapon-expert-a-bow (:name "武器習熟A/弓" :skill-type :passive :description  "Aランクの弓を装備できるようになり、弓のダメージ＋１")
    :armor-expert-a-light-armor (:name "防具習熟A/軽鎧" :skill-type :passive :description "Aランクの軽鎧を装備できるようになり、軽鎧の防護点＋１")
    :armor-expert-a-heavy-armor (:name "防具習熟A/重鎧" :skill-type :passive :description "Aランクの重鎧を装備できるようになり、重鎧の防護点＋１")
    :armor-expert-a-shield (:name "防具習熟A/盾" :skill-type :passive :description "Aランクの盾を装備できるようになり、盾の防護点＋１")
    :ambidexterity (:name "両手利き" :skill-type :passive :description "片手武器を2本、両腕に装備して使える。2階攻撃、命中判定-2")
    :decoy-attack-1 (:name "囮攻撃1" :skill-type :declare :description "近接攻撃時、命中判定-2 ダメージ+2 回避されると敵の回避力判定-1")
    :protect-1 (:name "かばう1" :skill-type :declare :description "1ターンに一度だけ周囲の味方の魔法以外の被ダメージを肩代わりする")
    :cut-back-1 (:name "斬り返し" :skill-type :declare :description  "攻撃が回避された場合、追加攻撃する")
    :diversionary-attack-1 (:name "牽制攻撃1" :skill-type :declare :description "近接、遠隔攻撃時、命中判定+1 クリティカル値+1")
    :all-out-attack-1 (:name "全力攻撃1" :skill-type :declare :description "近接攻撃時、一度だけダメージ+4 回避力判定-2")
    :provocation-attack-1 (:name "挑発攻撃1" :skill-type :declare :description "近接、遠隔攻撃時、一度だけダメージ-2 敵を挑発状態にする")
    :defence-stance (:name "ディフェンススタンス" :skill-type :declare :description "宣言時、回避、生命抵抗、精神抵抗判定に+4 そのほかのほとんどの判定-4")
    :violent-cast-1 (:name "バイオレントキャスト1" :skill-type :declare :description "ダメージ魔法使用時一度だけ魔法行使判定+2 魔法使い系技能が必要")
    :special-attack-1 (:name "必殺攻撃1" :skill-type :declare :description "近接攻撃時、威力表参照ダイス+1してダメージを計算する 回避判定-2")
    :me-power-certainty (:name "魔法拡大/威力確実化" :skill-type :declare :description "魔法攻撃時、威力表参照ダイスの出目が4以下だった場合、ダイスを振り直す 魔法使い系技能が必要")
    :me-certainty (:name "魔法拡大/確実化" :skill-type :declare :description "魔法攻撃時、魔法行使判定を2回行い、出目の高い方を使用する 魔法使い系技能が必要")
    :me-number (:name "魔法拡大/数" :skill-type :declare :description "単体魔法攻撃時に複数の敵を選択できる 魔法使い系技能が必要")
    :me-distance (:name "魔法拡大/距離" :skill-type :declare :description "魔法の射程距離を延ばす 魔法使い系技能が必要")
    :me-time (:name "魔法拡大/時間" :skill-type :declare :description  "魔法の効果時間を延ばす 魔法使い系技能が必要")
    :me-area (:name "魔法拡大/範囲" :skill-type :declare :description "魔法の効果範囲を広げる 魔法使い系技能が必要")
    :magic-convergence (:name "魔法収束" :skill-type :declare :description "範囲魔法を一体のみに行使する 魔法使い系技能が必要")
    :magic-hit (:name "魔力撃" :skill-type :declare :description "近接攻撃時、一度だけダメージに魔力を追加 生命、精神抵抗判定に-2")
    :armor-piercing-1 (:name "鎧貫き1" :skill-type :declare :description "近接攻撃時、一度だけ対象の防護点を半分として扱う クリティカル値+1 グラップラー技能が必要")
    :snipe (:name "狙撃" :skill-type :action :description "命中判定が対象の回避判定の出目より3以上高ければダメージ3倍 シューター技能、両手射撃武器が必要")
    :word-break (:name "ワードブレイク" :skill-type :action :description  "魔法などの効果をひとつ除去する")))


(defclass skill ()
  ((name       :accessor name      :initform nil :initarg :name)
   (rangemin   :accessor rangemin  :initform 0   :initarg :rangemin)
   (rangemax   :accessor rangemax  :initform 0   :initarg :rangemax)
   (atking-type    :accessor atking-type   :initform :magic :initarg :atking-type)
   (r   :accessor r  :initform 0   :initarg :r)
   (scope   :accessor scope  :initform nil   :initarg :scope) ;;スキルの発動範囲
   (temparea   :accessor temparea  :initform nil   :initarg :temparea)
   (range   :accessor range  :initform nil   :initarg :range) ;;スキルの射程距離
   (target   :accessor target  :initform 0   :initarg :target)
   (element   :accessor element  :initform 0   :initarg :element)
   (power   :accessor power  :initform 0   :initarg :power)
   (dmg-table   :accessor dmg-table  :initform 0   :initarg :dmg-table)
   (critical   :accessor critical  :initform 0   :initarg :critical)
   (mp   :accessor mp  :initform 0   :initarg :mp)
   (status   :accessor status  :initform 0   :initarg :status)
   (depend   :accessor depend  :initform 0   :initarg :depend)
   (origin   :accessor origin  :initform 0   :initarg :origin)
   (translate-x   :accessor translate-x  :initform 0   :initarg :translate-x)
   (translate-y   :accessor translate-y  :initform 0   :initarg :translate-y)
   (pos   :accessor pos  :initform 0   :initarg :pos)
   (img   :accessor img  :initform 0   :initarg :img)
   (frame   :accessor frame  :initform 0   :initarg :frame)
   (max-frame   :accessor max-frame  :initform 0   :initarg :max-frame)
   (interval   :accessor interval  :initform 0   :initarg :interval)
   (team   :accessor team  :initform 0   :initarg :team)
   (tag   :accessor tag  :initform 0   :initarg :tag)
   (sound   :accessor sound  :initform 0   :initarg :sound)
   ))

(defclass first-aid (skill)
  ())
(defclass heal (skill)
  ())
(defclass fire (skill)
  ())

(defclass use-item (skill itemdesc)
  ())
(defclass healing-potion (use-item)
  ())
(defclass magic-perfume (use-item)
  ())



(defparameter *skill-and-item-list*
  `(:heal ,(make-instance 'heal :name "ヒール" :target :ally :r 0 :mp 3 :rangemin 1 :rangemax 5
				:element :holy :power 10 :depend :int :img :skill-img
				:origin (gk:vec2 0 (* +heal+ 32)) :sound :heal
				:translate-y (- (* +heal+ *battle-obj-h*))
				:max-frame 100 :interval 20 :atking-type :magic-heal :critical 99
				:dmg-table (nth 10 *default-damage-table-list*))
    :fire ,(make-instance 'fire :name "ファイア" :target :enemy :r 1 :mp 3 :rangemin 1 :rangemax 5
				:element :fire :power 10 :depend :int :img :skill-img
				:origin (gk:vec2 0 (* +fire+ 32)) :sound :fire
				:translate-y (- (* +fire+ *battle-obj-h*))
				:max-frame 120 :interval 20 :atking-type :magic-atk
				:critical 10 :dmg-table (nth 10 *default-damage-table-list*))
    :first-aid ,(make-instance 'first-aid :name "応急手当" :target :ally :r 0 :mp 0 :rangemin 1 :rangemax 1
					  :depend :int :img :skill-img :atking-type :magic
					  :origin (gk:vec2 0 (* +heal+ 32)) :sound :heal
					  :translate-y (- (* +heal+ *battle-obj-h*))
					  :max-frame 120 :interval 20)
    :healing-potion ,(make-instance 'healing-potion :name "回復薬" :target :ally :r 0 :mp 0 :rangemin 0 :rangemax 0
						    :element :holy :power 20 :depend :int :img :skill-img :origin (gk:vec2 0 (* +heal+ 32))
						    :max-frame 100 :interval 20 :atking-type :magic-heal :critical 99 :category :item
						    :price 100
						    :dmg-table (nth 20 *default-damage-table-list*) :tag :healing-potion)
    :magic-perfume ,(make-instance 'magic-perfume :name "魔香水" :target :ally :r 0 :mp 0 :rangemin 1 :rangemax 1
						  :element :holy :power 0 :depend :int :img :skill-img :origin (gk:vec2 0 (* +heal+ 32))
						  :max-frame 100 :interval 20 :atking-type :magic-heal :critical 99 :category :item
						  :price 600
						  :dmg-table (nth 0 *default-damage-table-list*) :tag :magic-perfume)
    ))


;;使用アイテムのデータをゲット
(defun get-use-item-data (list)
  (loop :for tag :in list
	:collect (shallow-copy-object (getf *skill-and-item-list* tag))))

(defparameter *all-job-tag-and-data-list* '(:magitec (:name "マギテック" :description "")
					    :sorcerer (:name "ソーサラー" :description "")
					    :scout (:name "スカウト" :description "")
					    :fencer  (:name "フェンサー" :description "")
					    :fighter  (:name "ファイター" :description "")
					    :grappler (:name "グラップラー" :description "")
					    :priest (:name "プリースト" :description "")
					    :ranger (:name "レンジャー" :desciption "")
					    :conjurer (:name "コンジャラー" :description "")
					    :sage  (:name "セージ" :description "")
					    :shooter (:name "シューター" :description "")))


(defparameter *init-job-data-list*
  `(:human-magitec (:name "マギテック" :tec 8 :con 4 :mnd 9 :exp-point 2000 :tag :magitec :img ,+img-p-archer+)
    :human-sorcerer (:name "ソーサラー" :tec 6 :con 5 :mnd 10 :exp-point 2000 :tag :sorcerer :img ,+img-p-sorcerer+)
    :human-scout&fencer (:name "スカウト&フェンサー" :tec 10 :con 7 :mnd 4 :exp-point 2000 :tag (:scout :fencer)
			 :img ,+img-p-thief+)
    :human-none (:name "なし" :tec 7 :con 7 :mnd 7 :exp-point 3000 :tag :none :img ,+img-p-warrior+)
    :human-fighter (:name "ファイター" :tec 7  :con 10 :mnd 4 :exp-point 2000 :tag :fighter :img ,+img-p-warrior+)
    :human-grappler (:name "グラップラー" :tec 7  :con 10 :mnd 4 :exp-point 2000 :tag :grappler :img ,+img-p-warrior+)
    :human-priest (:name "プリースト" :tec 4  :con 8 :mnd 9 :exp-point 2000 :tag :priest :img ,+img-p-priest+)
    :human-conjurer (:name "コンジャラー" :tec 7 :con 4 :mnd 10 :exp-point 2000 :tag :conjurer :img ,+img-p-sorcerer+)
    :elf-fencer (:name "フェンサー" :tec 12 :con 5 :mnd 9 :exp-point 2500 :tag :fencer :img ,+img-p-thief+)
    :elf-sage&ranger (:name "セージ＆レンジャー" :tec 10 :con 5 :mnd 11 :exp-point 2000 :tag (:sage :ranger)
		      :img ,+img-p-thief+)
    :elf-priest (:name "プリースト" :tec 9  :con 5 :mnd 12 :exp-point 2000 :tag :priest :img ,+img-p-priest+)
    :elf-conjurer (:name "コンジャラー" :tec 9 :con 4 :mnd 13 :exp-point 2000 :tag :conjurer :img ,+img-p-sorcerer+)
    :elf-sorcerer (:name "ソーサラー" :tec 10 :con 3 :mnd 13 :exp-point 2000 :tag :sorcerer :img ,+img-p-sorcerer+)
    :elf-shooter (:name "シューター" :tec 13 :con 5 :mnd 8 :exp-point 2500 :tag :shooter :img ,+img-p-archer+)
    :dwarf-shooter (:name "シューター" :tec 6 :con 8 :mnd 6 :exp-point 2500 :tag :shooter :img ,+img-p-archer+)
    :dwarf-fighter (:name "ファイター" :tec 4  :con 11 :mnd 5 :exp-point 2000 :tag :fighter :img ,+img-p-warrior+)
    :dwarf-grappler (:name "グラップラー" :tec 5  :con 10 :mnd 5 :exp-point 2000 :tag :grappler :img ,+img-p-warrior+)
    :dwarf-priest (:name "プリースト" :tec 4  :con 7 :mnd 9 :exp-point 2000 :tag :priest :img ,+img-p-priest+)
    :dwarf-magitec (:name "マギテック" :tec 6 :con 7 :mnd 7 :exp-point 2000 :tag :magitec :img ,+img-p-archer+)
    :tabbit-conjurer (:name "コンジャラー" :tec 6 :con 6 :mnd 10 :exp-point 2000 :tag :conjurer :img ,+img-p-sorcerer+)
    :tabbit-sorcerer (:name "ソーサラー" :tec 5 :con 7 :mnd 10 :exp-point 2000 :tag :sorcerer :img ,+img-p-sorcerer+)
    :tabbit-sage (:name "セージ" :tec 5 :con 8 :mnd 9 :exp-point 2500 :tag :sage :img ,+img-p-sorcerer+)
    :tabbit-magitec (:name "マギテック" :tec 8 :con 5 :mnd 9 :exp-point 2000 :tag :magitec :img ,+img-p-archer+)
    :runefolk-sage (:name "セージ" :tec 8 :con 10 :mnd 8 :exp-point 2500 :tag :sage :img ,+img-p-sorcerer+)
    :runefolk-shooter (:name "シューター" :tec 12 :con 8 :mnd 8 :exp-point 2500 :tag :shooter :img ,+img-p-archer+)
    :runefolk-fighter (:name "ファイター" :tec 9  :con 12 :mnd 5 :exp-point 2000 :tag :fighter :img ,+img-p-warrior+)
    :runefolk-grappler (:name "グラップラー" :tec 9 :con 12 :mnd 5 :exp-point 2000 :tag :grappler :img ,+img-p-warrior+)
    :runefolk-magitec (:name "マギテック" :tec 12 :con 8 :mnd 6 :exp-point 2000 :tag :magitec :img ,+img-p-archer+)
    :runefolk-sorecerer (:name "ソーサラー" :tec 9 :con 8 :mnd 9 :exp-point 2000 :tag :sorcerer :img ,+img-p-sorcerer+)
    :nightmare-sorecerer (:name "ソーサラー" :tec 5 :con 13 :mnd 12 :exp-point 2000 :tag :sorcerer :img ,+img-p-sorcerer+)
    :nightmare-fighter (:name "ファイター" :tec 7 :con 15 :mnd 8 :exp-point 2000 :tag :fighter :img ,+img-p-warrior+)
    :nightmare-grappler (:name "グラップラー" :tec 7  :con 15 :mnd 8 :exp-point 2000 :tag :grappler :img ,+img-p-warrior+)
    :nightmare-fencer&scout (:name "フェンサー＆スカウト" :tec 11 :con 13 :mnd 6 :exp-point 2000 :tag (:fencer :scout) :img ,+img-p-thief+)
    :nightmare-priest (:name "プリースト" :tec 6 :con 14 :mnd 10 :exp-point 2000 :tag :priest :img ,+img-p-priest+)
    :nightmare-magitec (:name "マギテック" :tec 9 :con 9 :mnd 12 :exp-point 2000 :tag :magitec :img ,+img-p-archer+)
    :lycant-scout (:name "スカウト" :tec 13 :con 5 :mnd 7 :exp-point 2500 :tag :scout :img ,+img-p-thief+)
    :lycant-fighter (:name "ファイター" :tec 10 :con 9 :mnd 6 :exp-point 2000 :tag :fighter :img ,+img-p-warrior+)
    :lycant-grappler (:name "グラップラー" :tec 11  :con 7 :mnd 7 :exp-point 2000 :tag :grappler :img ,+img-p-warrior+)
    :lycant-fencer (:name "フェンサー" :tec 12  :con 6 :mnd 7 :exp-point 2500 :tag :fencer :img ,+img-p-warrior+)
    :lycant-ranger (:name "レンジャー" :tec 9  :con 8 :mnd 8 :exp-point 2500 :tag :ranger :img ,+img-p-thief+)
    ))


;;経験点テーブル
(defparameter *exp-point-table-A*
  '(1 1000
    2 1000
    3 1500
    4 1500
    5 2000
    6 2000))

(defparameter *exp-point-table-B*
  '(1 500
    2 1000
    3 1000
    4 1500
    5 1500
    6 2000))

(defparameter *all-job-exp-point-table* `(:fighter ,*exp-point-table-a*
					  :grappler ,*exp-point-table-a*
					  :fencer ,*exp-point-table-b*
					  :shooter ,*exp-point-table-b*
					  :sorcerer ,*exp-point-table-a*
					  :conjurer ,*exp-point-table-a*
					  :priest ,*exp-point-table-a*
					  :magitec ,*exp-point-table-a*
					  :scout ,*exp-point-table-b*
					  :ranger ,*exp-point-table-b*
					  :sage ,*exp-point-table-b*))
