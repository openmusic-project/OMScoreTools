;;;===================================================
;;; omscholar
;;; openmusic file interchange
;;;
;;; Preferences
;;; K . Haddad , IRCAM 2021
;;;===================================================

(in-package :om)


(defvar *GNUPLOT-PATH* "path to GNUPLOT binary")

(pushr 'gnuplot *external-prefs*)



(defmethod get-external-name ((module (eql 'gnuplot))) "gnuplot")
(defmethod get-external-icon ((module (eql 'gnuplot))) (and (find-library "omscholar") (list 275 (find-library "omscholar"))))


(defmethod get-external-module-path ((module (eql 'gnuplot)) modulepref) (get-pref modulepref :gnuplot))

(defmethod set-external-module-path ((module (eql 'gnuplot)) modulepref path) 
  (set-pref modulepref :gnuplot path))


(defun find-gnuplot-path ()
  "outputs gnuplot binary unix path"
  (multiple-value-bind (out pid)
      #+linux(sys:run-shell-command "sh -c 'which gnuplot'"
                                    :wait nil
                                    :output :stream
                                    :error-output nil)
    #+macosx(sys:run-shell-command "bash -l -c 'which gnuplot'"
                                   :wait nil
                                   :output :stream
                                   :error-output nil)
    #+win32(sys:run-shell-command "where gnuplot.exe"
                                   :wait nil
                                   :output :stream
                                   :error-output nil)
    (with-open-stream (out out)
      (values (read-line out) ))))

(defun test-gnuplot-bin ()
  "returns 0 if found, 1 if not"
  #+macosx(sys:run-shell-command "bash -l -c 'which gnuplot'")
  #+linux(sys:run-shell-command "sh -c 'which gnuplot'")
  #+win32(sys:run-shell-command "where gnuplot.exe")		
  )

(defun gnuplot? ()
  "tests if gnuplot is installed"
  (let ((test (test-gnuplot-bin)))
    (if (not (= 1 test))
      (find-gnuplot-path)
      "NOT FOUND"
)))


(defmethod get-external-def-vals ((module (eql 'gnuplot)))
    (cond
     ((equal *om-os* :linux) 
      (list :gnuplot (pathname (gnuplot?))) 
      )
     ((equal *om-os* :mac) 
      (list :gnuplot (pathname (gnuplot?))
               )) 
    ; ((equal *om-os* :win) 
    ;  (list :gnuplot-path (pathname (gnuplot?))
    ;        ))
     (t (list :gnuplot (pathname "~/bin/gnuplot")))))


(defun set-gnuplot-path ()
      (set-pref (find-pref-module :externals) :gnuplot-path (pathname (find-gnuplot-path)))
    )


(defmethod save-external-prefs ((module (eql 'gnuplot))) 
  `(:gnuplot ,(om-save-pathname *gnuplot-path*)))


(defmethod put-external-preferences ((module (eql 'gnuplot)) moduleprefs)
    (when (get-pref moduleprefs :gnuplot)
      (setf *GNUPLOT-PATH* (find-true-external (get-pref moduleprefs :gnuplot))))
    t)
      


;;put path in preferences external:
 
(if (not (= 1 (test-gnuplot-bin)))
    (progn
      (set-gnuplot-path)
      (put-external-preferences 'gnuplot-path (find-pref-module :externals))
      )
  )



;;;===========================
;;; PDF



(defvar *PDF-READER-PATH* "path to pdf reader")


(if (equal *om-os* :linux)
(pushr 'xpdf *external-prefs*))



(defmethod get-external-name ((module (eql 'xpdf))) "Pdf Reader")
(defmethod get-external-icon ((module (eql 'xpdf))) (and (find-library "omscholar") (list 606 (find-library "omscholar"))))


(defmethod get-external-module-path ((module (eql 'xpdf)) modulepref) (get-pref modulepref :pdf-path))

(defmethod set-external-module-path ((module (eql 'xpdf)) modulepref path) 
  (set-pref modulepref :pdf-path path))



(defmethod get-external-def-vals ((module (eql 'xpdf)))
  (cond
   ((equal *om-os* :linux) 
    (list :pdf-path (pathname "/usr/bin/xpdf"))
    )
   ((equal *om-os* :mac) 
    (list :pdf-path nil)
    ) 
   (t (list :pdf-path (pathname "/usr/bin/xpdf")))))


(defmethod put-external-preferences ((module (eql 'xpdf)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :pdf-options)))

    (when (get-pref moduleprefs :pdf-path)
      (setf *PDF-READER-PATH* (find-true-external (get-pref moduleprefs :pdf-path))))
    t))
      

(put-external-preferences 'xpdf (find-pref-module :externals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;========================================================================================================================
;         PREFERENCES PANEL MODULE
;=========================================================================================================================


(defvar *gp-composer-name* nil)
(setf *gp-composer-name* "El Guapo")

(defvar *gp-name-mode* nil)

(defvar *gp-name* nil)
(setf *gp-name* "BPF")


(defvar *lim-mode* nil)

(defvar *x-limits* "(0 100)")
(setf *x-limits* "(0 100)")

(defvar *y-limits* "(0 100)")
(setf *y-limits* "(0 100)")


(defvar *output-format* nil)
(setf *output-format* "pdf")


(defvar *font-style* nil)
(setf *font-style* *om-default-font2*)


(defmethod get-def-vals ((iconID (eql :gnuplot)))
   (list 
    :gp-name-mode nil
    :gp-name "BPF"
    :lim-mode nil
    :x-limits "(0 100)"
    :y-limits "(0 100)"
    :output-format "pdf"
    :font-style *om-default-font2* 
    ))


; (get-pref (find-pref-module :gnuplot) :font-style)

(defmethod put-preferences ((iconID (eql :gnuplot)))

  (let* ((modulepref (find-pref-module iconID)))
    (setf *gp-name-mode* (get-pref modulepref :gp-name-mode))
    (setf *gp-name* (get-pref modulepref :gp-name))
    (setf *lim-mode* (get-pref modulepref :lim-mode))
    (setf *x-limits* (get-pref modulepref :x-limits))
    (setf *y-limits* (get-pref modulepref :y-limits))
    (setf *output-format* (get-pref modulepref :output-format))
    (setf *font-style* (eval (get-pref modulepref :font-style))) 
    ))

(defmethod save-pref-module ((iconID (eql :gnuplot)) item)
   (list iconID `(list 
                  :gp-name-mode ,*gp-name-mode*
                  :gp-name ,*gp-name*
                  :lim-mode ,*lim-mode*
                  :x-limits ,*x-limits*
                  :y-limits ,*y-limits*
                  :output-format ,*output-format*
                  :font-style ,*font-style*
                  ) *om-version*))



(defmethod make-new-pref-scroll  ((num (eql :gnuplot)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "Gnuplot"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 0 0)
                                 :font *controls-font* 
                                ;:scrollbars :v 
                                ;:retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        (l1 50)
	(l2 (round (om-point-h (get-pref-scroll-size)) 2))
	(l3 (- (om-point-h (get-pref-scroll-size)) 60))
        (i 40)
        (posy 0)
	(dy 40)
        outtxt tmptxt)
    
    (om-add-subviews thescroll 
                     ;(om-make-dialog-item 'om-static-text (om-make-point l1 (incf posy 5)) (om-make-point 200 30) "General"
                     ;                      :font *om-default-font4b*)
                     

                      (om-make-dialog-item 'om-static-text 
                                           (om-make-point l1 (incf posy dy)) (om-make-point 90 24) "Output"
                                          :font *controls-font*)

                      (om-make-dialog-item 'om-pop-up-dialog-item 
                                           (om-make-point 160 (- i 3)) (om-make-point 90 10) ""
                                           :range '("pdf" "svg" "png" "jpeg")
                                           :value (cond ((string-equal *output-format* "pdf") "pdf")
                                                        ((string-equal *output-format* "svg") "svg")
                                                        ((string-equal *output-format* "png") "png")
                                                        ((string-equal *output-format* "jpeg") "jpeg")
                                                        (t nil))
                                           :di-action (om-dialog-item-act item 
                                                        (let ((choice (om-get-selected-item item)))
                                                          (set-pref modulepref :output-format
                                                                    (cond ((string-equal choice "pdf") "pdf")
                                                                          ((string-equal choice "svg") "svg")
                                                                          ((string-equal choice "png") "png")
                                                                          ((string-equal choice "jpeg") "jpeg")
                                                                          (t nil))
                                                                    )))
                                           :font *controls-font*)
                     

                    (om-make-dialog-item 'om-static-text  (om-make-point l1 (incf i 40)) (om-make-point 120 20) "Boundaries"
                                          :font *controls-font*)
                    
                    (om-make-dialog-item 'om-check-box (om-make-point 160 i) (om-make-point 20 10) ""
                                         :font *controls-font*
                                         :checked-p (get-pref modulepref :lim-mode)
                                         :di-action (om-dialog-item-act item 
                                                      (set-pref modulepref :lim-mode (om-checked-p item))))

                    (om-make-dialog-item 'om-static-text  (om-make-point 185 i) (om-make-point 30 10) "X:"
                                          :font *controls-font*)
                     
                    (om-make-dialog-item 'om-editable-text (om-make-point 210 (+ i 3))  (om-make-point 170 13)
                                         (format nil "~A" (get-pref modulepref :x-limits))
                                         :modify-action (om-dialog-item-act item (set-pref modulepref :x-limits (om-dialog-item-text item)))
                                         :font *om-default-font2*)


                    (om-make-dialog-item 'om-static-text  (om-make-point 185 (incf i 40)) (om-make-point 30 10) "Y:"
                                          :font *controls-font*)
                     
                    (om-make-dialog-item 'om-editable-text (om-make-point 210 (+ i 3))  (om-make-point 170 13)
                                         (format nil "~A" (get-pref modulepref :y-limits))
                                         :modify-action (om-dialog-item-act item (set-pref modulepref :y-limits (om-dialog-item-text item)))
                                         :font *om-default-font2*)
                                    



                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf i 40)) (om-make-point 90 24) "Name"
                                          :font *controls-font*) 

                     (om-make-dialog-item 'om-check-box (om-make-point 160 i) (om-make-point 30 10) ""
                                          :font *controls-font*
                                          :checked-p (get-pref modulepref :gp-name-mode)
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :gp-name-mode (om-checked-p item))))
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point 200 (+ i 3))  (om-make-point 170 13)
                                          (format nil "~A" (get-pref modulepref :gp-name))
                                          :modify-action (om-dialog-item-act item (set-pref modulepref :gp-name (om-dialog-item-text item)))
                                          :font *om-default-font2*)
                     

                     (om-make-dialog-item 'om-button (om-make-point (- l1 5)  (incf i 40)) (om-make-point 70 24) "Font"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore button))
                                                       (let* ((font (om-choose-font-dialog :font (get-pref modulepref :font-style))))
                                                         (when font
                                                           (set-pref modulepref :font-style font)
                                                           (om-set-font test-font font)
                                                           (om-invalidate-view thescroll))))
					  :font *controls-font*)
                    
                     (setf test-font (om-make-dialog-item 'om-static-text (om-make-point 160 i) 
                                                          (om-make-point 150 80) "FONT"
                                                          :font (get-pref modulepref :font-style)
                                                          :bg-color *om-white-color*
                                           ;:fg-color (get-pref modulepref :comment-color)
                                                          ))
                    

                     )

    #|
    (setf posy 0)
   
    
    (om-add-subviews thescroll
                     
                    (om-make-dialog-item 'om-button (om-make-point (+ l1 60) (- posy 5)) (om-make-point 70 24) "Font"
                                         :di-action (om-dialog-item-act item
                                                      (declare (ignore button))
                                                      (let* ((font (om-choose-font-dialog :font (get-pref modulepref :font))))
                                                        (when font
                                                          (set-pref modulepref :font font)
                                                          (om-set-font test-font font)
                                                          (setf *gnuplotfont* (get-gnuplot-font font))
                                                          (om-invalidate-view thescroll))))
					  :font *controls-font*)
                    
                    (setf test-font (om-make-dialog-item 'om-static-text (om-make-point (+ l1 150) (incf posy 25)) 
                                                             (om-make-point 100 20) "FONT"
                                           :font (get-pref modulepref :comment-font)
                                           :bg-color *om-white-color*
                                           ;:fg-color (get-pref modulepref :comment-color)
                                                               ))
                    
                    
                    )
          |#

    thescroll))




;set and load tab in om preferences panel 
(pushr :gnuplot *pref-order*)

(defun add-gnuplot-preferences ()
(push-pref-module (list :gnuplot (get-def-vals :gnuplot))))

(add-gnuplot-preferences)






