 (ql:quickload :ftw)

 (defpackage moge
   (:use #:cl #:ftw #:cffi))

 (in-package moge)

(load "define.lisp" :external-format :utf-8)
(load "name.lisp" :external-format :utf-8)


(load "mci.lisp" :external-format :utf-8)
(load "item.lisp" :external-format :utf-8)
(load "stage.lisp" :external-format :utf-8)
(load "stage-data.lisp" :external-format :utf-8)
(load "astar.lisp" :external-format :utf-8)
(load "render.lisp" :external-format :utf-8)
(load "save.lisp" :external-format :utf-8)
(load "mogetical.lisp" :external-format :utf-8)


(sb-ext:save-lisp-and-die "Mogetical.exe" :toplevel #'moge::moge ;;:application-type :gui
			  :executable t :save-runtime-options t)
