(in-package :om)

;;functions to add
;;from karim/sources/bpf.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;TOOLS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;supposed to return the REAL length of voice!
;when voice finishes with rest!

(defmethod! voice-onsets ((self voice))
  :icon 134
  :doc "returns the onsets of expressed pulses of a voice or poly" 
  (let ((dur (get-obj-dur self))
        (chrdseq (objfromobjs self (make-instance 'chord-seq))))
    (if (lmidic chrdseq)
        (lonset chrdseq))))

(defmethod! voice-onsets ((self poly))
            (mapcar #'voice-onsets (inside self)))
         


;(measure->voice (make-instance 'measure :tree '((5 4) (1 2 1))))
;This doesn't work !


;This messes up parent of self!
(defmethod measure2voice ((self measure))
  (let* ((trans (objfromobjs
                (clone self); must clone it because it will garble the voice containing the measure! 
                (make-instance 'voice))))
    (setf (tree trans) (resolve-? (tree trans)))
    trans))



;-------------------------------------------------------------------------------
;------------------------------setf-vel-----------------------------------------
;-------------------------------------------------------------------------------

(defmethod get-chord-objs ((self measure))
  (let ((chords (flat (get-all-chords self))))
  (remove nil (loop for i in chords
                    collect (if (and (not (rest-p i)) (not (cont-chord-p i))) i)))))

(defmethod get-chord-objs ((self voice))
  "Get all REAL chords objects in <self> omitting rests and continuation chords. The difference with get-all-chords, is the fact that these are NOT cloned objects but the REAL inside objects."
  (let ((chords (flat (get-all-chords self))))
  (remove nil (loop for i in chords
                    collect (if (and (not (rest-p i)) (not (cont-chord-p i))) i)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod setf-vel ((self voice) (newvel list))   
(mapcar #'(lambda (x y) (setf-vel x y))
          (get-chord-objs self) newvel))

(defmethod setf-vel ((self measure) (newvel list))                               
(mapcar #'(lambda (x y) (setf-vel x y))
          (get-chord-objs self) newvel))

(defmethod setf-vel ((self chord) (newvel list))
(setf (lvel self) newvel))

(defmethod setf-vel ((self chord) (newvel number))
(setf (lvel self) (list newvel)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;scale dyn



(defmethod! scale-dyn ((self list)
                       (minout number)
                       (maxout number))
  :initvals (list (list 64 64) 0 127) 
  :indoc '("dynamics" "min" "max")
  :icon 235
  :doc "Scales the given dynamics from a list, chord, chord-seq or voice" 
            (om-round (om-scale self minout maxout
                                0 127)))

(defmethod! scale-dyn ((self chord)
                       (minout number)
                       (maxout number))

            (let* ((clone (clone self))
                   (dyn (lvel clone)))
              (setf (lvel clone)
                    (scale-dyn dyn minout maxout))
              clone
              ))

(defmethod! scale-dyn ((self chord-seq)
                       (minout number)
                       (maxout number))

            (let* ((chords (loop for i in (inside self)
                                collect (scale-dyn i minout maxout))))
             (make-instance 'chord-seq
                            :lmidic chords
                            :lonset (lonset self))
              ))

(defmethod! scale-dyn ((self voice)
                       (minout number)
                       (maxout number))
                               
            (let ((chords (loop for i in (chords self)
                                collect (scale-dyn i minout maxout))))
              (make-instance 'voice
                             :tree (tree self)
                             :chords chords
                             :tempo (tempo self))))
                  
(defmethod! scale-dyn ((self poly)
                       (minout number)
                       (maxout number))
                               
            (let ((voices (loop for i in (inside self)
                                collect (scale-dyn i minout maxout))))
              (make-instance 'poly :voices voices)))

                             
;================================================================================================
;=========================================BPF STUFF==============================================
;================================================================================================



;;;;;just the dynamics
(defmethod! dyn-to-score ((self voice) 
                          (bpf bpf) 
                          &optional 
                          (min nil) 
                          (max nil))
  :initvals (list t t nil nil) 
  :indoc '("voice" "bpf" "min" "max")
  :icon 277
  :doc "Outputs midi dynamic values starting from <bpf> profile and expressed note in <self>.
<bpf> ideally supposed to be between y=0-100 (included) most particulary if they are staight lines. 
If boundaries are transgressed, it's fine but not for constant bpfs."
  (let* ((chords (clone (chords self)))
         (real-obj-dur (get-obj-dur (objfromobjs self (make-instance 'chord-seq))))
         (tempo (tempo self))
         (tree (tree self))
         (onsets (voice-onsets self));prob cf*
         (ys (y-points bpf))
         (miny (if (< (list-min ys) 0) (list-min ys) 0))
         (maxy (if (> (list-max ys) 100) (list-max ys) 100))
                   
         (bpfscale (bpf-scale bpf :x1 0 :x2 real-obj-dur))
         (newys (if min (om-scale ys min max miny maxy) ys))
         (newbpf (om-make-bpf 'bpf (x-points bpfscale) newys 10)))
                   
    (om-round (butlast (x-transfer newbpf (om+ onsets 1)))));+1 is for bpf-step cases
  )

(defmethod! dyn-to-score ((self measure) 
                          (bpf bpf) 
                          &optional 
                          (min nil) 
                          (max nil))
  (let ((voice (measure2voice self)))
    (dyn-to-score voice bpf min max)))



;;;bfp-dyn-to-score


(defmethod! bpf-dyn-to-score ((self voice) 
                              (bpf bpf) 
                              &key 
                              (mode 'clone)
                              (min nil) 
                              (max nil)
                              )
  :initvals (list t t 'clone nil nil ) 
  :indoc '("voice" "bpf" "mode" "min" "max" )
  :menuins '((2 (("clone" clone)
                 ("destructive" destructive))))
  :icon 277
  :doc "Applies bpf as dynamic enveloppe on a voice. Bpfs are supposed to be between y=0-100 (included) most particulary if they are staight lines. If boundaries are transgressed, it's fine but not for constant bpfs. "
  (let* ((chords (if (equal mode 'clone) (clone (chords self)) (chords self)))
         (tempo (tempo self))
         (tree (tree self))
         (dyns (voice->y-markers self bpf min max))
         (newdyns (loop for i in chords
                        for vel in dyns
                        do (setf (lvel i) (list vel))))
         ) 
    (if (equal mode 'clone) 
        (make-instance 'voice 
                       :tree tree
                       :chords chords
                       :tempo tempo)
      (setf (chords self) chords))
    ))

;;* If voice ends with a measure ended itself by rests this will give 
;; a false obj-dur

(defmethod! bpf-dyn-to-score ((self poly) (bpf bpf) 
                              &key (mode 'clone) (min nil) (max nil))
  (let ((dyns (loop for i in (inside self)
                    collect (bpf-dyn-to-score i bpf mode min max))))
    (make-instance 'poly
                   :voices dyns)))


;to be tested:
(defmethod! bpf-dyn-to-score ((self measure) (bpf bpf) 
                              &key (mode 'clone) (min nil) (max nil))
  (let* ((voice (measure2voice self))
         (dyn (bpf-dyn-to-score voice bpf mode min max)))
    (car (inside dyn))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod! voice->bpf-markers ((self voice)
                               (bpf bpf)
                               &optional
                               (y1 nil)
                               (y2 nil))
  :initvals (list t t nil nil) 
  :indoc '("voice" "bpf" "y1" "y2")
  :icon 277
  :doc "Maps expressed rhythm onsets on a bpf curve. <y1> <y2> are optional scaling factors.
It outputs a multibpf" 

  (let* ((onsets (voice-onsets self));onsets of expressed notes
         (onsets (x-append onsets (get-obj-dur self)))_;with REAL duration of obj for scaling.
         (bpfscale (bpf-scale bpf 
                              :x1 0 ;should start from the beginning even if first event of self is a rest
                              :x2 (last-elem onsets)))
         (xpoints (x-points bpfscale))
         (ypoints (if y1 
                      (om-scale (y-points bpfscale) y1 y2)
                    (y-points bpfscale)))
         (bpfscale1 (om-make-bpf 'bpf xpoints ypoints 10))
         (ys (x-transfer bpfscale1 onsets 10))
         (ons (loop for i in onsets
                    collect (list i i)))
         (bpfs (loop for on in (butlast ons)
                     for y in ys
                     collect (set-color 
                              (om-make-bpf 'bpf on (list y 0) 10)
                              (om-make-color 0.9 0.3 0.3)))))
    ;(print (list ys onsets))
    (make-instance 'bpf-lib
                   :bpf-list (x-append bpfscale1 (butlast bpfs)))))


(defmethod! voice->bpf-markers ((self poly)
                               (bpf bpf)
                               &optional
                               (y1 nil)
                               (y2 nil))
  (let* ((onsets (voice-onsets self))
         (bpfscale (bpf-scale bpf 
                              :x1 0 ;(car onsets) 
                              :x2 (last-elem onsets)))
         (xpoints (x-points bpfscale))
         (ypoints (if y1 
                      (om-scale (y-points bpfscale) y1 y2)
                    (y-points bpfscale)))
         (bpfscale1 (om-make-bpf 'bpf xpoints ypoints 10))
         (ys (x-transfer bpfscale1 onsets 10))
         (ons (loop for i in onsets
                    collect (list i i)))
         (bpfs (loop for on in (butlast ons)
                     for y in ys
                     collect (set-color 
                              (om-make-bpf 'bpf on (list y 0) 10)
                              (om-make-color 0.9 0.3 0.3)))))
              ;(print ys)
    (make-instance 'bpf-lib
                   :bpf-list (x-append bpfscale1 bpfs))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;new stuff

(defmethod! bpf-xscale ((self bpf) 
                        (voice voice) 
                        (min number) 
                        (max number)
                        &optional (dec 0))
  :initvals (list t t 0 12 0)
  :indoc '("bpf" "voice" "min" "max" "decimals")
  :icon 237
  :doc "Scales <self> (a bpf) according to <voice>'s duration as x-points and scales its y-points according to boundaries given in <min> and <max>."

  (let* ((dur (get-obj-dur voice))
         (xscale (bpf-scale self :x1 0 :x2 dur))
         (ypoints (y-points xscale))
         (yscale (om-round (om-scale ypoints min max) dec)))
   ; (print (list xscale yscale))
    (om-make-bpf 'bpf (x-points xscale) yscale dec)))
         
(defmethod! voice->y-markers ((self voice)
                               (bpf bpf)
                               &optional
                               (y1 nil)
                               (y2 nil)
                               (dec 0))
            :initvals (list t t nil nil 0) 
            :indoc '("voice" "bpf" "y1" "y2" "decimals")
            :icon 277
            :doc "Maps expressed rhythm onsets on a bpf curve and returns the equivalent y-points. <y1> <y2> are optional scaling factors." 

            (let* ((onsets (voice-onsets self))
                   (bpfscale (bpf-scale bpf 
                                        :x1 0 ;(car onsets) 
                                        :x2 (last-elem onsets)))
                   (xpoints (x-points bpfscale))
                   (ypoints (if y1 
                                (om-scale (y-points bpfscale) y1 y2)
                              (y-points bpfscale)))
                   (bpfscale1 (om-make-bpf 'bpf xpoints ypoints dec))
                   (ys (x-transfer bpfscale1 onsets dec)))
              (butlast ys)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;MICROPHONIE;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;control voice starting from a bpf
;;;It is a per measure bpf control.

;;1) scaling bpf according to 
;;;;;;a) desired width of staff 
;;;;;;b) limits of desired max min y-points


;;2) make-voice from bpf


(defmethod! scale-bpf-lim ((self bpf)
                           (dur number)
                           (min number)
                           (max number)
                           &optional 
                           (minlim 0)
                           (maxlim 100)
                           )
            "scales bpf x-points according to a given duration (in ms) and y-points according to given limits, Returns a bpf."
            (let* ((x (x-points self))
                   (y (y-points self))
                   (newx (om-round (om-scale x 0 dur)))
                   (newy (om-round (om-scale y min max minlim maxlim)))
                   )
              (simple-bpf-from-list newx newy)))
                      
(defmethod! bpf->voice ((self bpf)
                        (dur number)
                        (timesig list)
                        (tempo list)
                        (min number)
                        (max number)
                        &optional 
                        (minlim 0)
                        (maxlim 100)
                        )
            "Converts a bpf in a voice according to given parameters."
            (let* ((scaled (scale-bpf-lim self dur min max minlim maxlim))
                   (x (x-points self))
                   (y (y-points scaled))
                   (tree (list '? (list (list timesig (x->dx (om-round x))))))
                   )
              (make-instance 'voice
                             :tree tree
                             :tempo tempo
                             :chords y)))
                   
(defmethod! voicebpfcontrol ((self voice)
                             (bself bpf)
                             (min number)
                             (max number)
                             &optional 
                             (minlim 0)
                             (maxlim 100)
                             )
            "Converts a bpf in a voice according to given parameters."
            (let* ((timesig (car (get-meas self)))
                   (dur (get-obj-dur self))
                   (tempo (tempo self))
                   (scaled (scale-bpf-lim bself dur min max minlim maxlim))
                   (x (x-points bself))
                   (y (y-points scaled))
                   (tree (list '? (list (list timesig (x->dx (om-round x))))))
                   (newvoice
                    (make-instance 'voice
                                   :tree tree
                                   :tempo tempo
                                   :chords y)))
              (make-instance 'poly
                             :voices (list self newvoice)
                             )))
                   


(defmethod! microphonie ((self chord-seq)
                         (mc-center number)
                         (time-span number))
            :icon 270
            :indoc '("self" "centroid" "time-scale")
            :initvals '(t 6000 4000)
            :doc "transforme un chord-seq en segments de controles bpf. <mc-center> est la valeur en midicents de note du chord-seq servant de centre bpf. <time-span> est la valeur en milliseconde qui sera le y du bpf."
            (let* ((midics (flat (lmidic self)))
                   (centroid (om- midics mc-center))
                   (y (flat (loop for i in centroid
                                  collect (list i i))))
                   (onsets 
                    (cdr
                     (flat 
                      (loop for i in (lonset self)
                            collect (list i i)))))
                   (x (om-scale onsets 0 time-span))
                   )
              (simple-bpf-from-list x y)))


;attention ici il prend le timespan = duree totale de la voice.
(defmethod! microphonie ((self voice)
                         (mc-center number)
                         (time-span number))
            (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
              (microphonie chrdseq mc-center (get-obj-dur self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod! bpf-affine->step ((self bpf) &optional (decimals 0))
                        :initvals (list t 0) 
            :indoc '("bpf" "decimals")
            :icon 275
            :doc "Maps" 
            (let* ((xpoints (x-points self))
                   (ypoints (y-points self))
                   (nxpoints (x-append (car xpoints)
                                       (flat 
                                        (loop for i in (cdr xpoints) 
                                              collect (list i i)))))
                   (nypoints (flat 
                              (loop for i in ypoints 
                                    collect (list i i)))))
              (om-make-bpf 'bpf nxpoints nypoints decimals)))
                   

(defmethod! bpf-step->affine ((self bpf) &optional (decimals 0))
                        :initvals (list t 0) 
            :indoc '("bpf" "decimals")
            :icon 275
            :doc "Maps" 
            (let* ((xpoints (x-points self))
                   (ypoints (y-points self))
                   (nxpoints (x-append (car xpoints)
                                      (mapcar 'car  (n-group-list (cdr xpoints) 2))))
                                            
                   (nypoints (mapcar 'car (n-group-list ypoints 2))))
                                   
              (om-make-bpf 'bpf nxpoints nypoints decimals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod! bpf-mvt-contraire ((self bpf))
  :icon 275
  :indoc '("self")
  :initvals '(t)
  :doc "Applies serial forms on bpf curves."
  (let ((ys (om* (y-points self) -1)))
    (simple-bpf-from-list (x-points self) ys 'bpf (decimals self))))


(defmethod! bpf-retrograde ((self bpf))
  :icon 275
  :indoc '("self")
  :initvals '(t)
  :doc "Applies serial forms on bpf curves."
  (let* ((xs (x-points self))
         (ys (y-points self))
         (xpt (dx->x (car xs) (reverse (x->dx xs))))) 
    (simple-bpf-from-list xpt (reverse ys) 'bpf (decimals self))))    
                 

(defmethod! bpf-trans ((self bpf) &optional (mode 'reverse))
            :icon 275
            :indoc '("self" "path")
            :initvals '(t 'reverse)
            :menuins '((1 (("reverse" 'reverse)
                           ("retro" 'retro)
                           ("retro-reverse" 'retro-rev)
                           )))
            :doc "Applies serial forms on bpf curves."
            (case mode
              (reverse (bpf-mvt-contraire self))
              (retro (bpf-retrograde self))
              (retro-rev (bpf-retrograde (bpf-mvt-contraire self)))))

