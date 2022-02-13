;; ==================================================================================== 
;;                                OMSCORETOOLS
;; ==================================================================================== 
;;
;;                                  
;;                          author : Karim Haddad   
;;                     
;;
;;
;;
;;
;;
;;
;;
;This program is free software; you can redistribute it and/or 
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------
(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------
(defparameter *omscoretools-files* '(
                               "prefs"   
                               "gnuplot"
                               "voice-bpf"
                               ))
                               
;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'(lambda (file) (compile&load (om-relative-path '("sources") file)))*omscoretools-files*)


;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-omscoretools-var* nil)

(setf *subpackages-omscoretools-var*
      '(
        ("Bpf/Score interaction"  
         nil nil (
                  voice->y-markers
                  voice->bpf-markers
                  scale-bpf-lim
                  bpf->voice
                  bpf-dyn-to-score
                  voicebpfcontrol
                  microphonie
                  ) nil)
        ("Bpf tools"  
         nil nil (
                  bpf-mvt-contraire
                  bpf-retrograde
                  bpf-trans
                  bpf-xscale
                  bpf-affine->step
                  bpf-step->affine
                  ) nil)
        ("GnuPlot"  
         nil nil (
                  om->gnuplot
                  voice-bpf->gnuplot	
                  ) nil)
        ("Utilities"  
         nil nil (
                  scale-dyn
                  dyn-to-score
                  ) nil)
        )
      )
;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-omscoretools-var*)

;--------------------------------------------------
;doc & info
;--------------------------------------------------

(doc-library "omScoreTools, a library for editing and printing score and shapes.
 ---- TO DO ----
" 
             (find-library "omscoretools"))

; (gen-lib-reference (find-library "omscoretools"))

(unless (fboundp 'om::set-lib-release) (defmethod om::set-lib-release (version &optional lib) nil))


(set-lib-release 0.1) 


(om-print "
;;;============================================================                                
;;               OMScoreTools 
;;      author : Karim Haddad 
;;      RepMus - IRCAM
;;;============================================================
")

;;; (gen-lib-reference (find-library "omscholar"))



