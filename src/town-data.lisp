(in-package :lowmogecage)


(defparameter *sale-item-show-max* 15)

(defparameter *datehako-item* (list  +w_knife+ +w_stiletto+ +w_dagger+ +w_Kukri+ +short_sword+ +w_epee+
				     +w_javelin+ +w_short_spear+ +w_spear+ +w_long_spear+ +w_pillar+
				     +w_short_bow+  +w_normal_bow+ +w_long_bow+ +w_heavy_bow+ +w_light_bow+
				     +a_cloth_Armor+ +a_point_guard+ +a_soft_Leather+ +a_hard_leather+ +a_aramid_coat+
				     +a_buckler+ +a_round_shield+ +a_kite_shield+ +a_tower_shield+ +i_healing_potion+
				     +i_magic_perfume+))

(defparameter *sapo-ro-item* (list +w_knife+ +w_stiletto+ +w_dagger+ +w_Kukri+ +short_sword+ +w_epee+
				   +w_Katzbalger+ +w_rapier+  +w_sabel+ +w_estoc+ +w_long_sword+ +w_broad_sword+
				   +w_bastard-sword+ +w_falchion+ +w_two_hand_sword+ +w_shamshier+ +w_graet_sword+
				   +w_fast_spike+  +w_flissa+ +w_schiavona+
				   +w_defender+ +w_talwar+ +w_steal_blade+ +w_flamberge+ +w_dragon_slayer+))

(defparameter *akawasahi-item* (list +w_javelin+ +w_short_spear+ +w_spear+ +w_long_spear+ +w_pillar+
				     +w_trident+  +w_Ahlspiess+ +w_pike+ +i_healing_potion+))

(defparameter *biohiro-item* (list +w_short_bow+  +w_normal_bow+ +w_long_bow+ +w_heavy_bow+ +w_light_bow+
				   +w_wrapped_bow+ +w_fast_bow+))

(defparameter *mitaki-item* (list +w_sickle+ +w_hand_ax+ +w_Bhuj+ +w_battle_ax+ +w_great_ax+ +w_heavy_ax+
				  +w_Bulloova+ +w_Minotaur_ax+
				  +w_Tomahawk+ +w_abarzin+ +w_long_ax+ +w_Halberd+ +w_glaive+
				  +i_magic_perfume+))

(defparameter *oita-item* (list +w_mage_staff+ +w_quater_staff+ +w_ifrit_beard+ +w_mana_staff+ +w_reach_staff+))

(defparameter *wakannai-item* (list +a_cloth_Armor+ +a_point_guard+ +a_soft_Leather+ +a_hard_leather+ +a_aramid_coat+
	 +a_Breast_armor+ +a_bone_best+ +a_sprint_armor+ +a_chain_mail+
	 +a_plate_armor+ +a_suit_armor+ +a_ateel_guard+ +a_Lamellar_armor+
	 +a_Brigandine+ +a_coat_of_plate+ +a_fotress+))

(defparameter *gasakina-item* (list +a_buckler+ +a_round_shield+ +a_kite_shield+ +a_tower_shield+
	 +a_target_shield+ +a_heater_shield+  +a_spike_shield+ +a_great_Wall+
	 +a_great_parry+))

(defparameter *magokashi-item* (list +i_healing_potion+ +i_magic_perfume+))

(defclass town ()
  ((name       :accessor name      :initform nil :initarg :name)
   (sale-item       :accessor sale-item      :initform nil :initarg :sale-item)
   (sale-item-page       :accessor sale-item-page      :initform 0 :initarg :sale-item-page)))



(defparameter *town-list*
  `(5 ,(make-instance 'town :name "ダテハコ" :sale-item (get-item-data-from-list *datehako-item*))
    6 ,(make-instance 'town :name "サポーロ" :sale-item (get-item-data-from-list *sapo-ro-item*))
    7 ,(make-instance 'town :name "アカワサヒ" :sale-item (get-item-data-from-list *akawasahi-item*))
    8 ,(make-instance 'town :name "ビオヒロ" :sale-item (get-item-data-from-list *biohiro-item*))
    9 ,(make-instance 'town :name "ミタキ" :sale-item (get-item-data-from-list *mitaki-item*))
    z ,(make-instance 'town :name "オイター" :sale-item (get-item-data-from-list *oita-item*))
    c ,(make-instance 'town :name "ワカンナイ" :sale-item (get-item-data-from-list *wakannai-item*))
    v ,(make-instance 'town :name "ガサキナ" :sale-item (get-item-data-from-list *gasakina-item*))
    b ,(make-instance 'town :name "マゴカシ" :sale-item (get-item-data-from-list *magokashi-item*))))
