(in-package :mogetical)


(cffi:defctype mcierror :int)
(cffi:defcfun "mciSendStringA" mcierror 
  (msg :string) (ret :string) 
  (a :int) (b :int))



; Helper function that calls mcisendstringa
(defun mci-send-string (command) 
  (mcisendstringa command "" 0 0))

(defun bgm-play (alias &optional (from 0))
  (mci-send-string
   (format nil "play ~a from ~d" alias from)))


(defun bgm-open (path alias)
  (mci-send-string
   (format nil "open ~a alias ~a type mpegvideo" path alias)))
  


(defun bgm-set-volume (vol alias)
  (mci-send-string
   (format nil "setaudio ~a volume to ~d" alias vol)))

(defun mci-send-status (alias)
  (with-foreign-strings ((str (make-string 255)))
    (mcisendstringa
     (concatenate 'string "status " alias " mode") str 255 0)
    (foreign-string-to-lisp str)))
;; 
;;(defun set-song (song) 
;;  (setf *song* song))

(defun bgm-status (alias)
  (multiple-value-bind (str num)
      (mci-send-status alias)
    (declare (ignore num))
    str))

; play-song is a helper function. 
;; (defun bgm-play (path alias vol) 
;;   (let ((status 
;; 	 (mci-send-open path alias)))
;;     (if (zerop status) 
;; 	1
;; 	(progn (mci-send-volume vol alias)
;; 	       (mci-send-play alias)))))

; Plays the current song. 
; It will keep trying to play it if there is an error, 
; because frequently it will fail for some reason :/ 
; This failure appears to be nondeterministic... 
;; (defun play-bgm (path vol) 
;;   (if *song* 
;;       (if (zerop (play-song path vol))
;; 	  0 
;; 	  (play path vol)) 
;;       "Error: You must first specify a song with SET-SONG."))

; Pauses the current song. 
(defun bgm-pause (alias) 
  (mci-send-string
   (format nil "pause ~a" alias)))

; Stops the current song. 
(defun bgm-stop (alias) 
  (mci-send-string
   (format nil "stop ~a" alias)))

; Please remember to call this when you're done with a song! 
(defun bgm-close (alias) 
  (mci-send-string
   (format nil "close ~a" alias)))



(defun init-bgm ()
  (loop :for path :in (list *title-bgm-path* *edit-bgm-path* *prebattle-bgm-path*
			    *battle1-bgm-path* *battle2-bgm-path*
			    *battle3-bgm-path* *battle4-bgm-path* *battle5-bgm-path*)
     :for alias :in (list *titlebgm* *editbgm* *prebattle* *battle1bgm* *battle2bgm* *battle3bgm* *battle4bgm*
			  *battle5bgm*)
     :do (bgm-open path alias)
       (bgm-set-volume 350 alias)))

(defun close-bgms ()
  (loop  :for alias :in (list *titlebgm* *editbgm* *prebattle* *battle1bgm*
			      *battle2bgm* *battle3bgm* *battle4bgm* *battle5bgm*)
     :do (bgm-close  alias)))

