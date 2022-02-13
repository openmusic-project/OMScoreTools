(in-package :om)

;(defvar *numData* 0)
(defvar *gpformat* nil)
(setf *gpformat* ".pdf")


;;;;;;;;;;;;;;;;;;;;;;;;tools
;color conversion
(defun scalecolor (n)
  (round (* n 255)))

;(scalecolor 0.5) 

(defun dec2hex (n)
  (let ((hex (write-to-string n :base 16)))
    (if (= 1 (length hex))
        (format nil "0~A" hex)
      hex)))

(defun conv-color (n)
  (dec2hex (scalecolor n)))


(defmethod omcolor->hex ((self oa::omcolor))
  (let ((r (conv-color (om-color-r self)))
        (g (conv-color (om-color-g self)))
        (b (conv-color (om-color-b self))))
    (format nil "#~A~A~A" r g b)))


(defun get-gnuplot-font (font)
  (let ((face (om-font-face font))
        (size (round 
               (* #+linux 3/4 #-linux 1 (om-font-size font)))))
    (format nil "~A,~A" face size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun run-gnuplot (path)
  (let* ((gnuplot-path (namestring *GNUPLOT-PATH*))
         (folder (pathname-directory path))
         (folderpath (make-pathname :directory folder))
         ;(outfile (make-pathname :directory folder :name (pathname-name path)))
         )

    (om-cmd-line (format nil "cd ~s; sh -c '~s ~s'" 
                         (om-path2cmdpath folderpath) 
                         gnuplot-path
                         (om-path2cmdpath path)))
    ))
   
    ;(t outfile)))



(defun write-gnuplot-bpffile (list path name color format)
  (let ((pathname (or path (om-choose-new-file-dialog)))
        )
    ;(print (list "toto" pathname))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      ;;;HERE COMES THE HEADER and GENERAL SETTINGS

      (format out "set terminal ~A #outputs but doesn't display~%" (if format format *output-format*))
      (format out "set output  \"~A.~A\"~%" (pathname-name pathname) (if format format *output-format*))
      
      (format out "set term ~A font ~S~%"  (if format format *output-format*) (get-gnuplot-font *font-style*))
      
      (if *lim-mode* 
          (let ((x (read-from-string *x-limits*))
                (y (read-from-string *y-limits*)))
          (format out "set xrange [~D : ~D]     #limites de l'axe x~%" (car x) (second x)) 
          (format out "set yrange [~D : ~D]     #limites de l'axe y" (car y) (second y)))) 
     
 ;(format out "set title \"Dynamics from midicents\"~%")
      ;(format out "set xlabel \"Time\"~%")
      ;(format out "set ylabel \"Energy\"~%")
      (format out "~%")
     ; (format out "#set border 3~%")
     ; (format out "set yrange [-2850:4050]~%")
     ; (format out "set ytics nomirror rangelimited~%")
      (format out "~%")    
      (format out "set grid~%")
      (format out "show grid~%")
      (format out "~%")  
      (format out "$data << EOD~%")

      ;;;;The data (x y)
   
      ;;;;;
      (loop for elt in list do
            (format out "~A ~A~%" (car elt) (second elt)))
      (format out "EOD ~%~%")
      (format out "plot $data title ~S with lines lt rgb ~S ~%" name color)
      (format out "#pause -1 \"Hit any key to continue\"")
      )
    pathname))



(defun write-gnuplot-multifile (list path name color format)
  (let ((pathname (or path (om-choose-new-file-dialog)))
        )
    ;(print (list "toto" pathname))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      
      (format out "set terminal ~A #outputs but doesn't display~%" (if format format *output-format*))
      (format out "set output  \"~A.~A\"~%" (pathname-name pathname) (if format format *output-format*))

      
       (format out "set term ~A font ~S~%"  (if format format *output-format*) (get-gnuplot-font *font-style*))
      
      (if *lim-mode* 
          (let ((x (read-from-string *x-limits*))
                (y (read-from-string *y-limits*)))
          (format out "set xrange [~D : ~D]     #limites de l'axe x~%" (car x) (second x)) 
          (format out "set yrange [~D : ~D]     #limites de l'axe y" (car y) (second y)))) 

      (format out "~%")
      (format out "~%")    
      (format out "set grid~%")
      (format out "show grid~%")
      (format out "~%")  
      
      (print list)
      (let* ((index 1))
        (loop for i in list
              for n in name
             do (let* ((xpoints (x-points i))
                       (ypoints (y-points i))
                       (data (mat-trans (list xpoints ypoints))))
                  (format out "$data~D << EOD~%" index)
                  ;;;;The data (x y)
                  (loop for elt in data
                        do (format out "~A ~A~%" (car elt) (second elt)))
                  (format out "EOD ~%~%")
                  (incf index)
                  ))
        (format out "plot \\~%")
        (setf index 1)
        (loop for j in list
              for n in name
              for c in color
              do (progn
                   (format out "$data~D title ~S with lines lt rgb ~S, \\~%" index (if n n (write-to-string index)) c)
                   (incf index))
                   )
        
        (format out "#pause -1 \"Hit any key to continue\"")
        )
             pathname)))

(defun write-gnuplot-3dfile (list path name color format)
  (let ((pathname (or path (om-choose-new-file-dialog)))
        )
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      ;;;HERE COMES THE HEADER and GENERAL SETTINGS
      (format out "#set terminal ~A #outputs svg but doesn't display~%" format) 
      (format out "#set output  \"d1_plot.~A\"~%" format) 
      ;(format out "set title \"Dynamics from midicents\"~%")
      ;(format out "set xlabel \"Time\"~%")
      ;(format out "set ylabel \"Energy\"~%")
      (format out "~%")
     ; (format out "#set border 3~%")
     ; (format out "set yrange [-2850:4050]~%")
     ; (format out "set ytics nomirror rangelimited~%")
      (format out "~%")    
      ;(format out "set grid~%")
      ;(format out "show grid~%")
      (format out "set ticslevel 0 ~%")
      (format out "~%")  
      (format out "$data << EOD~%")

      ;;;;The data (x y)
   
      ;;;;;
      (loop for elt in list do
            (format out "~A ~A ~A~%" (car elt) (second elt) (third elt)))
      (format out "EOD ~%~%")
      (format out "splot $data u 1:2:3 title ~S with lines lt rgb ~S ~%" name color)
      (format out "pause -1 \"Hit any key to continue\"")
      )
    pathname))

(defmethod! om->gnuplot ((self bpf) 
                         &optional 
                         (format nil)
                         (path nil))
                         
  :icon 275
  :indoc '("self" "format" "path")
  :initvals '(t nil nil)
  :menuins '((1 (
                 ("pdf" "pdf" )
                 ("svg" "svg" )
                 ("png" "png" )
                 ("jpeg" "jpeg")
                 )))
  :doc "Exports bpf to gnuplot graph."
  (setf *numData* 1)
  (let* ((pathname (or path (om-choose-new-file-dialog)))
         (xpoints (x-points self))
         (ypoints (y-points self))
         (data (mat-trans (list xpoints ypoints)))
         (name (if *gp-name-mode* *gp-name* (get-name self)))
         (color (omcolor->hex (bpfcolor self)))
         (gnuplotfile (write-gnuplot-bpffile data pathname name color (if format format *output-format*))))
                   
    (run-gnuplot gnuplotfile)))

;faire pour le reste (ainsi que dans le write!

(defmethod! om->gnuplot ((self list) 
                         &optional 
                         (format "pdf")
                         (path nil))

  (let* ((name (om-choose-new-file-dialog)))
         (loop for i in self
               for n from 1 to (length self)
               do (om->gnuplot i format (format nil "~D-~D.gp" name n)))))
         

(defmethod! om->gnuplot ((self bpf-lib) 
                         &optional 
                         (format nil)
                         (path nil))
  (setf *numData* 1)
  (let* ((pathname (or path (om-choose-new-file-dialog)))
                   ;(xpoints (x-points self))
                   ;(ypoints (y-points self))
                   ;(data (mat-trans (list xpoints ypoints)))
         (data (bpf-list self))
         (name (mapcar #'get-name data))
         (color (loop for i in data
                      collect (omcolor->hex (bpfcolor i))))
         (gnuplotfile (write-gnuplot-multifile data pathname name color format)))
                   
    (run-gnuplot gnuplotfile)))

(defmethod! om->gnuplot ((self 3dc) 
                         &optional 
                         (format "pdf")
                         (path nil))
  (setf *numData* 1)
  (let* ((pathname (or path (om-choose-new-file-dialog)))
         (xpoints (x-points self))
         (ypoints (y-points self))
         (zpoints (z-points self))
         (data (mat-trans (list xpoints ypoints zpoints)))
         (name (get-name self))
         (color (omcolor->hex (bpfcolor self)))
         (gnuplotfile (write-gnuplot-3dfile data pathname name color format)))
                   
    (run-gnuplot gnuplotfile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;VOICE-BPF-GNUPLOT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;special for gnuplot examples.

(defun write-gnuplot-vbpffile (list path)
  (let ((pathname (or path (om-choose-new-file-dialog)))
        )
    ;(print (list "toto" pathname))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (format out "set terminal svg #outputs svg but doesn't display~%")
      (format out "set output  \"~A.svg\"~%" (pathname-name pathname)); (pathname-type pathname)) 
      (format out "~%")
      (format out "~%")
      (format out "set key off ~%")
      (format out "set grid~%")
      (format out "show grid~%")
      (format out "~%")  
      
      (print list)
      ;;;;DATA
      (let* ((index 1))
        (loop for i in list
              do (let* ((xpoints (x-points i))
                        (ypoints (y-points i))
                        (data (mat-trans (list xpoints ypoints))))
                   (format out "$data~D << EOD~%" index)
                   ;;;;The data (x y)
                   (loop for elt in data
                         do (format out "~A ~A~%" (car elt) (second elt)))
                   (format out "EOD ~%~%")
                   (incf index)
                   ))
        ;;;;labels
        (loop for i in (cdr list)
              do (let* ((xpoints (x-points i))
                        (ypoints (y-points i))
                        (data (mat-trans (list xpoints ypoints))))
                   (let ((xy (car data)))
                     (format out "set label \"(~D ~D)\" at ~D,~D~%" 
                             (round (car xy)) 
                             (om-round (second xy) 2)
                             (round (car xy)) 
                             (om-round (second xy) 2)))
                   ))

        ;;;;commands
        (format out "plot \\~%")
        (format out "$data1 with lines lt rgb \"red\", \\~%")
        (setf index 2)
        (loop for j in (cdr list)
              do (progn
                   (format out "$data~D with linespoint pointtype 7 lt rgb \"blue\", \\~%" index)
                   (incf index))
              )
        
        (format out "#pause -1 \"Hit any key to continue\"")
        )
      pathname)))


(defmethod! voice-bpf->gnuplot ((self bpf-lib) 
                         &optional 
                         (path nil))
            :icon 275
            :indoc '("self" "path")
            :initvals '(t nil)
            :doc "Exports bpf to gnuplot graph."
            (setf *numData* 1)
            (let* ((pathname (or path (om-choose-new-file-dialog)))
                   (data (bpf-list self))
                   (gnuplotfile (write-gnuplot-vbpffile data pathname)))
                   
              (run-gnuplot gnuplotfile)))