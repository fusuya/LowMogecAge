(ql:quickload :mogetical)

(sb-ext:save-lisp-and-die "Mogetical.exe" :toplevel #'mogetical:moge ;;:application-type :gui
			  :executable t :save-runtime-options t)
