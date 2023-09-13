(in-package :lowmogecage)


(defparameter *sale-item-show-max* 15)

(defparameter *datehako-item* (list  +w_knife+ +w_stiletto+ +w_dagger+ +w_Kukri+ +short_sword+ +w_epee+
				     +w_javelin+ +w_short_spear+ +w_spear+ +w_long_spear+ +w_pillar+
				     +w_short_bow+  +w_normal_bow+ +w_long_bow+ +w_heavy_bow+ +w_light_bow+
				     +a_cloth_Armor+ +a_point_guard+ +a_soft_Leather+ +a_hard_leather+ +a_aramid_coat+
				     +a_buckler+ +a_round_shield+ +a_kite_shield+ +a_tower_shield+ +i_healing_potion+))

(defclass town ()
  ((name       :accessor name      :initform nil :initarg :name)
   (sale-item       :accessor sale-item      :initform nil :initarg :sale-item)
   (sale-item-page       :accessor sale-item-page      :initform 0 :initarg :sale-item-page)))



(defparameter *town-list*
  `(5 ,(make-instance 'town :name "ダテハコ" :sale-item (get-item-data-from-list *datehako-item*))
    6 ,(make-instance 'town :name "サポーロ")
    7 ,(make-instance 'town :name "アカワサヒ")
    8 ,(make-instance 'town :name "ビオヒロ")
    9 ,(make-instance 'town :name "ミタキ")
    z ,(make-instance 'town :name "オイター")
    c ,(make-instance 'town :name "ワカンナイ")
    v ,(make-instance 'town :name "ガサキナ")
    b ,(make-instance 'town :name "マゴカシ")))
