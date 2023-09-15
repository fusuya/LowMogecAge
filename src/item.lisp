(in-package :lowmogecage)
;;武器 heal:傷薬
(my-enum ;;剣
	 +w_knife+ +w_stiletto+ +w_dagger+ +w_Kukri+ +short_sword+ +w_epee+
	 +w_Katzbalger+ +w_rapier+  +w_sabel+ +w_estoc+ +w_long_sword+ +w_broad_sword+
	 +w_bastard-sword+ +w_falchion+ +w_two_hand_sword+ +w_shamshier+ +w_graet_sword+
	 +w_fast_spike+  +w_flissa+ +w_schiavona+
	 +w_defender+ +w_talwar+ +w_steal_blade+ +w_flamberge+ +w_dragon_slayer+
	 ;;槍
	 +w_javelin+ +w_short_spear+ +w_spear+ +w_long_spear+ +w_pillar+
	 +w_trident+  +w_Ahlspiess+ +w_pike+ 
	 ;;弓
	 +w_short_bow+  +w_normal_bow+ +w_long_bow+ +w_heavy_bow+ +w_light_bow+
	 +w_wrapped_bow+ +w_fast_bow+
	 
	 ;;斧
	 +w_sickle+ +w_hand_ax+ +w_Bhuj+ +w_battle_ax+ +w_great_ax+ +w_heavy_ax+
	 +w_Bulloova+ +w_Minotaur_ax+
	 +w_Tomahawk+ +w_abarzin+ +w_long_ax+ +w_Halberd+ +w_glaive+
	 ;;杖
	 +w_mage_staff+ +w_quater_staff+ +w_ifrit_beard+ +w_mana_staff+ +w_reach_staff+
	 ;;鎧
	 +a_cloth_Armor+ +a_point_guard+ +a_soft_Leather+ +a_hard_leather+ +a_aramid_coat+
	 +a_Breast_armor+ +a_bone_best+ +a_sprint_armor+ +a_chain_mail+
	 +a_plate_armor+ +a_suit_armor+ +a_ateel_guard+ +a_Lamellar_armor+
	 +a_Brigandine+ +a_coat_of_plate+ +a_fotress+
	 
	 ;;盾
	 +a_buckler+ +a_round_shield+ +a_kite_shield+ +a_tower_shield+
	 +a_target_shield+ +a_heater_shield+  +a_spike_shield+ +a_great_Wall+
	 +a_great_parry+
	 ;;アイテム
	 +i_healing_potion+ +i_magic_perfume+
	 +w_max+
	 )

(defparameter *sword-list*
  (list +w_knife+ +w_stiletto+ +w_dagger+ +w_Kukri+ +short_sword+ +w_epee+
	 +w_Katzbalger+ +w_rapier+  +w_sabel+ +w_estoc+ +w_long_sword+ +w_broad_sword+
	 +w_bastard-sword+ +w_falchion+ +w_two_hand_sword+ +w_shamshier+ +w_graet_sword+
	 +w_fast_spike+  +w_flissa+ +w_schiavona+
	 +w_defender+ +w_talwar+ +w_steal_blade+ +w_flamberge+ +w_dragon_slayer+))

(defparameter *spear-list* 
  (list +w_javelin+ +w_short_spear+ +w_spear+ +w_long_spear+ +w_pillar+
	 +w_trident+  +w_Ahlspiess+ +w_pike+))

(defparameter *bow-list*
  (list +w_short_bow+  +w_normal_bow+ +w_long_bow+ +w_heavy_bow+ +w_light_bow+
	 +w_wrapped_bow+ +w_fast_bow+))

(defparameter *ax-list*
  (list +w_sickle+ +w_hand_ax+ +w_Bhuj+ +w_battle_ax+ +w_great_ax+ +w_heavy_ax+
	 +w_Bulloova+ +w_Minotaur_ax+
	 +w_Tomahawk+ +w_abarzin+ +w_long_ax+ +w_Halberd+ +w_glaive+))

(defparameter *staff-list*
  (list +w_mage_staff+ +w_quater_staff+ +w_ifrit_beard+ +w_mana_staff+ +w_reach_staff+))



	 
(defparameter *armor-list*
  (list +a_cloth_Armor+ +a_point_guard+ +a_soft_Leather+ +a_hard_leather+ +a_aramid_coat+
	 +a_Breast_armor+ +a_bone_best+ +a_sprint_armor+ +a_chain_mail+
	 +a_plate_armor+ +a_suit_armor+ +a_ateel_guard+ +a_Lamellar_armor+
	 +a_Brigandine+ +a_coat_of_plate+ +a_fotress+))

(defparameter *shield-list*
  (list  +a_buckler+ +a_round_shield+ +a_kite_shield+ +a_tower_shield+
	 +a_target_shield+ +a_heater_shield+  +a_spike_shield+ +a_great_Wall+
	 +a_great_parry+))
	 



(defun drop-list (lst)
  (loop :for i :in (reverse lst)
     :for rate :from 1
     :collect (cons i (expt 1.27 rate))))


(defun create-drop-item-list (lst)
  (sort (loop :for i :in lst
	   :append (drop-list i))
	#'>= :key #'cdr))


(defparameter *warrior-weapon-drop-rate*  nil)
(defparameter *sorcerar-weapon-drop-rate* nil)
(defparameter *priest-weapon-drop-rate*   nil)
(defparameter *thief-weapon-drop-rate*    nil)
(defparameter *archer-weapon-drop-rate*   nil)
(defparameter *knight-weapon-drop-rate*   nil)
(defparameter *armor-drop-rate*      nil)
(defparameter *shield-drop-rate* nil)

(defun init-weapon-list ()
  (setf *warrior-weapon-drop-rate* (create-drop-item-list (list *sword-list* *spear-list* *ax-list*))
	*sorcerar-weapon-drop-rate* (create-drop-item-list (list *rod-list*))
	*priest-weapon-drop-rate* (create-drop-item-list (list *staff-list*))
	*thief-weapon-drop-rate* (create-drop-item-list (list *sword-list*))
	*archer-weapon-drop-rate* (create-drop-item-list (list *bow-list*))
	*knight-weapon-drop-rate* (create-drop-item-list (list *spear-list*))
	*armor-drop-rate* (create-drop-item-list (list *clothes-list*))
	*shield-drop-rate* (create-drop-item-list (list *shield-list*))))

;武器データ配列
(defparameter *weapon-data*
  ;;剣
  (list
   (make-instance 'weapondesc :category :sword :name "ナイフ" :damage 1 :atktype :short :required-str 1
			      :critical 10 :rangemin 1 :rangemax 1 :price 30 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "スティレット" :damage 2 :atktype :short :required-str 2
			      :critical 10 :rangemin 1 :rangemax 1 :price 40 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "ダガー" :damage 3 :atktype :short :required-str 3
			      :critical 10 :rangemin 1 :rangemax 1 :price 50 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "ククリ" :damage 4 :atktype :short :required-str 4
			      :critical 10 :rangemin 1 :rangemax 1 :price 60 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "ショートソード" :damage 5 :atktype :short :required-str 5
			      :critical 10 :rangemin 1 :rangemax 1 :price 80 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "エペ" :damage 6 :atktype :short :required-str 6
			      :critical 10 :rangemin 1 :rangemax 1 :price 90 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "カッツバルゲル" :damage 7 :atktype :short :required-str 7
			      :critical 10 :rangemin 1 :rangemax 1 :price 100 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "レイピア" :damage 8 :atktype :short :required-str 8
			      :critical 10 :rangemin 1 :rangemax 1 :price 110 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "サーベル" :damage 10 :atktype :short :required-str 10
			      :critical 10 :rangemin 1 :rangemax 1 :price 190 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "エストック" :damage 21 :atktype :short :required-str 11
			      :critical 10 :rangemin 1 :rangemax 1 :price 370 :hand :2h)
   (make-instance 'weapondesc :category :sword :name "ロングソード" :damage 13 :atktype :short :required-str 13
			      :critical 10 :rangemin 1 :rangemax 1 :price 440 :hand :1hor2h)
   (make-instance 'weapondesc :category :sword :name "ブロードソード" :damage 15 :atktype :short :required-str 15
			      :critical 10 :rangemin 1 :rangemax 1 :price 340 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "バスタードソード" :damage 17 :atktype :short :required-str 17
			      :critical 10 :rangemin 1 :rangemax 1 :price 560 :hand :1hor2h)
   (make-instance 'weapondesc :category :sword :name "ファルシオン" :damage 28 :atktype :short :required-str 18
			      :critical 10 :rangemin 1 :rangemax 1 :price 790 :hand :2h)
   (make-instance 'weapondesc :category :sword :name "ツーハンドソード" :damage 30 :atktype :short :required-str 20
			      :critical 10 :rangemin 1 :rangemax 1 :price 860 :hand :2h)
   (make-instance 'weapondesc :category :sword :name "シャムシール" :damage 32 :atktype :short :required-str 22
			      :critical 10 :rangemin 1 :rangemax 1 :price 960 :hand :2h)
   (make-instance 'weapondesc :category :sword :name "グレートソード" :damage 34 :atktype :short :required-str 24
			      :critical 10 :rangemin 1 :rangemax 1 :price 1020 :hand :2h)
   (make-instance 'weapondesc :category :sword :name "ファストスパイク" :damage 6 :atktype :short :required-str 1
			      :critical 10 :rangemin 1 :rangemax 1 :price 820 :hand :1h :hit 1)
   (make-instance 'weapondesc :category :sword :name "フリッサ" :damage 13 :atktype :short :required-str 8
			      :critical 10 :rangemin 1 :rangemax 1 :price 860 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "スキアヴォーナ" :damage 16 :atktype :short :required-str 11
			      :critical 10 :rangemin 1 :rangemax 1 :price 990 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "ディフェンダー" :damage 17 :atktype :short :required-str 12
			      :critical 10 :rangemin 1 :rangemax 1 :price 1770 :hand :1h :def 1)
   (make-instance 'weapondesc :category :sword :name "タルワール" :damage 20 :atktype :short :required-str 15
			      :critical 10 :rangemin 1 :rangemax 1 :price 1210 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "スティールブレイド" :damage 24 :atktype :short :required-str 19
			      :critical 10 :rangemin 1 :rangemax 1 :price 1450 :hand :1h)
   (make-instance 'weapondesc :category :sword :name "フランベルジュ" :damage 28 :atktype :short :required-str 23
			      :critical 10 :rangemin 1 :rangemax 1 :price 1580 :hand :1hor2h)
   (make-instance 'weapondesc :category :sword :name "ドラゴンスレイヤー" :damage 43 :atktype :short :required-str 28
			      :critical 10 :rangemin 1 :rangemax 1 :price 2760 :hand :2h)


   ;;やり
   (make-instance 'weapondesc :category :spear :name "ジャベリン" :damage 10 :atktype :short :required-str 5
			      :critical 10 :rangemin 1 :rangemax 1 :price 65 :hand :1h :hit -1)
   (make-instance 'weapondesc :category :spear :name "ショートスピア" :damage 15 :atktype :short :required-str 10
			      :critical 10 :rangemin 1 :rangemax 1 :price 110 :hand :1h :hit -1)
   (make-instance 'weapondesc :category :spear :name "スピア" :damage 20 :atktype :short :required-str 15
			      :critical 10 :rangemin 1 :rangemax 1 :price 170 :hand :1hor2h :hit -1)
   (make-instance 'weapondesc :category :spear :name "ロングスピア" :damage 25 :atktype :short :required-str 20
			      :critical 10 :rangemin 1 :rangemax 1 :price 220 :hand :1hor2h :hit -1)
   (make-instance 'weapondesc :category :spear :name "ピラー" :damage 13 :atktype :short :required-str 8
			      :critical 9 :rangemin 1 :rangemax 1 :price 880 :hand :1h :hit -1)
   (make-instance 'weapondesc :category :spear :name "トライデント" :damage 22 :atktype :short :required-str 12
			      :critical 10 :rangemin 1 :rangemax 1 :price 460 :hand :1h :hit -1)
   (make-instance 'weapondesc :category :spear :name "アールシェピース" :damage 25 :atktype :short :required-str 15
			      :critical 10 :rangemin 1 :rangemax 1 :price 480 :hand :1hor2h :hit -1)
   (make-instance 'weapondesc :category :spear :name "パイク" :damage 45 :atktype :short :required-str 25
			      :critical 10 :rangemin 1 :rangemax 1 :price 1750 :hand :1h :hit -1)
	      
	       
   ;;弓
   (make-instance 'weapondesc :category :bow :name "ショートボウ" :damage 12 :atktype :long :required-str 2
			      :critical 10 :rangemin 2 :rangemax 5 :price 60 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ノーマルボウ" :damage 17 :atktype :long :required-str 7
			      :critical 10 :rangemin 2 :rangemax 5 :price 120 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ロングボウ" :damage 22 :atktype :long :required-str 12
			      :critical 10 :rangemin 2 :rangemax 5 :price 170 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ヘビーボウ" :damage 27 :atktype :long :required-str 17
			      :critical 10 :rangemin 2 :rangemax 5 :price 220 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ライトボウ" :damage 20 :atktype :long :required-str 5
			      :critical 10 :rangemin 2 :rangemax 5 :price 610 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ラップドボウ" :damage 28 :atktype :long :required-str 13
			      :critical 10 :rangemin 2 :rangemax 5 :price 780 :hand :2h)
   (make-instance 'weapondesc :category :bow :name "ファストボウ" :damage 35 :atktype :long :required-str 20
			      :critical 10 :rangemin 2 :rangemax 5 :price 1200 :hand :2h)
   ;;斧
   (make-instance 'weapondesc :category :axe :name "シックル" :damage 9 :atktype :short :required-str 4
			      :critical 11 :rangemin 1 :rangemax 1 :price 40 :hand :1h)
   (make-instance 'weapondesc :category :axe :name "ハンドアックス" :damage 12 :atktype :short :required-str 7
			      :critical 11 :rangemin 1 :rangemax 1 :price 90 :hand :1h)
   (make-instance 'weapondesc :category :axe :name "ブージ" :damage 16 :atktype :short :required-str 11
			      :critical 11 :rangemin 1 :rangemax 1 :price 240 :hand :1h)
   (make-instance 'weapondesc :category :axe :name "バトルアックス" :damage 21 :atktype :short :required-str 16
			      :critical 11 :rangemin 1 :rangemax 1 :price 360 :hand :1hor2h)
   (make-instance 'weapondesc :category :axe :name "グレートアックス" :damage 33 :atktype :short :required-str 18
			      :critical 11 :rangemin 1 :rangemax 1 :price 410 :hand :2h)
   (make-instance 'weapondesc :category :axe :name "ヘビーアックス" :damage 25 :atktype :short :required-str 20
			      :critical 11 :rangemin 1 :rangemax 1 :price 440 :hand :1hor2h)
   (make-instance 'weapondesc :category :axe :name "ブローバ" :damage 37 :atktype :short :required-str 22
			      :critical 11 :rangemin 1 :rangemax 1 :price 490 :hand :2h)
   (make-instance 'weapondesc :category :axe :name "ミノタウロスアックス" :damage 45 :atktype :short :required-str 30
			      :critical 11 :rangemin 1 :rangemax 1 :price 950 :hand :2h)
   (make-instance 'weapondesc :category :axe :name "トマホーク" :damage 17 :atktype :short :required-str 7
			      :critical 11 :rangemin 1 :rangemax 1 :price 630 :hand :1h)
   (make-instance 'weapondesc :category :axe :name "タバルジン" :damage 23 :atktype :short :required-str 13
			      :critical 11 :rangemin 1 :rangemax 1 :price 840 :hand :1h)
   (make-instance 'weapondesc :category :axe :name "ロングアックス" :damage 36 :atktype :short :required-str 16
			      :critical 11 :rangemin 1 :rangemax 1 :price 990 :hand :2h)
   (make-instance 'weapondesc :category :axe :name "ハルバード" :damage 40 :atktype :short :required-str 20
			      :critical 11 :rangemin 1 :rangemax 1 :price 1080 :hand :2h)
   (make-instance 'weapondesc :category :axe :name "グレイブ" :damage 48 :atktype :short :required-str 28
			      :critical 11 :rangemin 1 :rangemax 1 :price 1350 :hand :2h)
	      
   ;;杖
   (make-instance 'weapondesc :category :staff :name "メイジスタッフ" :damage 11 :atktype :short :required-str 1
			      :critical 10 :rangemin 1 :rangemax 1 :price 110 :hand :2h :hit 1 :def 1)
   (make-instance 'weapondesc :category :staff :name "クォータースタッフ" :damage 14 :atktype :short :required-str 4
			      :critical 10 :rangemin 1 :rangemax 1 :price 140 :hand :2h :hit 1 :def 1)
   (make-instance 'weapondesc :category :staff :name "イフリートの髭" :damage 14 :atktype :short :required-str 4
			      :critical 10 :rangemin 1 :rangemax 1 :price 500 :hand :2h :hit 1 :def 1)
   (make-instance 'weapondesc :category :staff :name "マナスタッフ" :damage 13 :atktype :short :required-str 3
			      :critical 10 :rangemin 1 :rangemax 1 :price 6800 :hand :2h :hit 1)
   (make-instance 'weapondesc :category :staff :name "リーチスタッフ" :damage 15 :atktype :short :required-str 5
			      :critical 10 :rangemin 1 :rangemax 1 :price 7000 :hand :2h :hit 1)

	      
	      
   ;;鎧
   (make-instance 'armordesc :category :armor :name "クロースアーマー" :required-str 1 :price 15 :def 2)
   (make-instance 'armordesc :category :armor :name "ポイントガード" :required-str 1 :price 100 :def 0 :avoid 1)
   (make-instance 'armordesc :category :armor :name "ソフトレザー" :required-str 7 :price 150 :def 3)
   (make-instance 'armordesc :category :armor :name "ハードレザー" :required-str 13 :price 340 :def 4)
   (make-instance 'armordesc :category :armor :name "アラミドコード" :required-str 5 :price 750 :def 2 :avoid 1)
   (make-instance 'armordesc :category :armor :name "ブレストアーマー" :required-str 10 :price 1000 :def 5)
   (make-instance 'armordesc :category :armor :name "ボーンベスト" :required-str 16 :price 2100 :def 6)
   (make-instance 'armordesc :category :armor :name "スプリントアーマー" :required-str 15 :price 520 :def 5)
   (make-instance 'armordesc :category :armor :name "チェインメイル" :required-str 18 :price 760 :def 6 :avoid -1)
   (make-instance 'armordesc :category :armor :name "プレートアーマー" :required-str 21 :price 15 :def 7 :avoid -2)
   (make-instance 'armordesc :category :armor :name "スーツアーマー" :required-str 24 :price 15 :def 8 :avoid -3)
   (make-instance 'armordesc :category :armor :name "スティールガード" :required-str 12 :price 1600 :def 5)
   (make-instance 'armordesc :category :armor :name "ラメラーアーマー" :required-str 15 :price 2400 :def 6)
   (make-instance 'armordesc :category :armor :name "ブリガンディ" :required-str 18 :price 15 :def 7 :avoid -1)
   (make-instance 'armordesc :category :armor :name "コートオブプレート" :required-str 24 :price 6100 :def 8 :avoid -2)
   (make-instance 'armordesc :category :armor :name "フォートレス" :required-str 27 :price 8200 :def 9 :avoid -3)
	      
   ;;盾
   (make-instance 'shielddesc :category :shield :name "バックラー" :required-str 1 :price 60 :def 0 :avoid 1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "ラウンドシールド" :required-str 8 :price 100 :def 1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "カイトシールド" :required-str 13 :price 500 :def 1 :avoid 1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "タワーシールド" :required-str 17 :price 600 :def 2 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "ターゲットシールド" :required-str 7 :price 680 :def 1 :avoid 1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "ヒーターシールド" :required-str 10 :price 1000 :def 2 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "スパイクシールド" :required-str 13 :price 1800 :def 2 :avoid 1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "グレートウォール" :required-str 20 :price 1800 :def 3 :avoid -1 :hand :1h)
   (make-instance 'shielddesc :category :shield :name "グレートパリィ" :required-str 30 :price 3300 :def 3 :avoid 1 :hand :2h)
   ;;使用アイテム
   (make-instance 'healing-potion :name "回復薬" :target :ally :r 0 :mp 0 :rangemin 0 :rangemax 0 :price 100
				  :element :holy :power 20 :depend :int :img :skill-img :origin (gk:vec2 0 (* +heal+ 32))
				  :max-frame 100 :interval 20 :atking-type :magic-heal :critical 99 :category :item
				  :dmg-table (nth 20 *default-damage-table-list*) :tag :healing-potion)
   (make-instance 'magic-perfume :name "魔香水" :target :ally :r 0 :mp 0 :rangemin 1 :rangemax 1 :price 600
				 :element :holy :power 0 :depend :int :img :skill-img :origin (gk:vec2 0 (* +heal+ 32))
				 :max-frame 100 :interval 20 :atking-type :magic-heal :critical 99 :category :item
				 :dmg-table (nth 0 *default-damage-table-list*) :tag :magic-perfume)
   ))

(defparameter *weapondescs*
  (make-array (length *weapon-data*)
	      :initial-contents *weapon-data*))

;;アイテムのデータ持ってくる　武器なら威力表つける
(defun item-make (itemnum)
  (let ((item-data (shallow-copy-object (aref *weapondescs* itemnum))))
    (when (eq 'weapondesc (type-of item-data))
      (setf (dmg-table item-data) (copy-list (nth (damage item-data) *default-damage-table-list*))))
    item-data))

(defun get-item-data-from-list (list)
  (loop :for num :in list
	:collect (item-make num)))


(defparameter *test-buki-item* nil)

(defun set-test-item-list ()
  (setf *test-buki-item*
	(loop :for n :from 0 :below (length *weapondescs*)
	   :collect (item-make n))))

(defun create-item-n (n)
  (loop :for a :from 0 :below n
	:collect (item-make a)))

(my-enum +c_crude_weapon+ +c_dirty_hood+ +c_well_worn_hood+ +c_crude_shield+
	 +c_copper_bag+ +c_silver_bag+ +c_gems+ +c_fragment_sword+ +c_sturdy_bone+
	 +c_magic_bone+ +c_sullied_bone+ +c_unholy_skull+ +c_fruits_resentment_tears+
	 +c_zombie_eyeball+ +c_old_cloak+ +c_year_old_cape+ +c_magic_stone+
	 +c_sharp_stone+ +c_high_density_magic_stone+)

(defparameter *cash-exchange-item-list*
  (list
   (make-instance 'cash-exchange :name "粗末な武器" :price 10)
   (make-instance 'cash-exchange :name "汚いフード" :price 20)
   (make-instance 'cash-exchange :name "着慣れたフード" :price 80)
   (make-instance 'cash-exchange :name "粗雑な盾" :price 50)
   (make-instance 'cash-exchange :name "銅貨袋" :price 30)
   (make-instance 'cash-exchange :name "銀貨袋" :price 100)
   (make-instance 'cash-exchange :name "宝石" :price 150)
   (make-instance 'cash-exchange :name "剣のかけら" :price 200)
   (make-instance 'cash-exchange :name "頑丈な骨" :price 30)
   (make-instance 'cash-exchange :name "魔力を帯びた骨" :price 250)
   (make-instance 'cash-exchange :name "穢れた骨" :price 50)
   (make-instance 'cash-exchange :name "穢れた頭蓋骨" :price 300)
   (make-instance 'cash-exchange :name "恨みの涙の結晶" :price 500)
   (make-instance 'cash-exchange :name "ゾンビの眼球" :price 30)
   (make-instance 'cash-exchange :name "古びた外套" :price 200)
   (make-instance 'cash-exchange :name "年経たマント" :price 800)
   (make-instance 'cash-exchange :name "魔力を帯びた石" :price 250)
   (make-instance 'cash-exchange :name "鋭利な石" :price 20)
   (make-instance 'cash-exchange :name "高密度の魔石" :price 700)
   ))







;;ジョブ別初期武器
(defun job-init-weapon (job)
  (case job
    (p-fighter  (item-make +w_knife+))
    (p-sorcerer  (item-make +w_mage_staff+))
    (p-s-knight  (item-make +w_javelin+))
    (p-priest  (item-make +w_mage_staff+))
    (p-p-knight  (item-make +w_javelin+))
    (p-scout  (item-make +w_knife+))
    (p-ranger  (item-make +w_short_bow+))))
    ;; ((= job +job_sorcerer+) (item-make +w_rod+))
    ;; ((= job +job_priest+)   (item-make +w_staff+))
    ;; ((= job +job_archer+)   (item-make +w_bow+))
    ;; ((= job +job_s_knight+) (item-make +w_spear+))
    ;; ((= job +job_thief+)    (item-make +w_knife+))
    ;; ((= job +job_p_knight+) (item-make +w_spear+))
    ;; (brigand  (item-make +w_bow+))
    ;; (dragon   (item-make +w_dragon_crow+))
    ;; (hydra    (item-make +w_hydra_fang+))
    ;; (orc      (item-make +w_ax+))
    ;; (slime   (item-make +w_numenume+))
    ;; (yote1    (item-make +w_numenume+))
    ;; (goron    (item-make +w_numenume+))))

;;武器のステータス取得
(defun get-item-abi (weapon ablility)
  (cond
    ((null weapon)
     "なし")
    (t
     (case ablility
       (:name     (name     (aref *weapondescs* weapon)))
       (:rangemin (rangemin (aref *weapondescs* weapon)))
       (:rangemax (rangemax (aref *weapondescs* weapon)))
       (:def      (def      (aref *weapondescs* weapon)))
       (:hit      (hit      (aref *weapondescs* weapon)))
       (:critical (critical (aref *weapondescs* weapon)))
       (:damage   (damage   (aref *weapondescs* weapon)))
       (:blk      (blk      (aref *weapondescs* weapon)))))))















    
