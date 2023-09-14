(defsystem #:lowmogecage
  :version "0.0.1"
  :author "mogezou"
  :license ""
  :depends-on (:trivial-gamekit)
  :components ((:module "src"
                :components
               	 ((:file "package")
		  (:file "world-map-data")
                  (:file "define")
		  (:file "keybind")
		  (:file "item")
		  (:file "town-data")
		  (:file "monster-data")
		  (:file "name")
		  (:file "astar")
		  (:file "util")
		  (:file "render")
		  (:file "stage-data")
		  (:file "game")

                  
		 )))
  :description "")
