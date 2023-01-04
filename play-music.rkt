#lang racket
(require srfi/1)
(require rsound)
(require plot)


(define F 44100)
(define (make-pitch pitch) (make-tone pitch 0.3 (/ FRAME-RATE 4)))
(define (play-pitch pitch) (play (make-pitch pitch)))
(define (play-pitches . pitches)
  (let ((tones (map play-pitch pitches)))
    (play (rs-append* tones))))

(define peaks
(list 7702 24950 43606 65601 87681 110818 131148 151696 171138 191928 211782 231137 253633 273494 313906 339131 362137 433066 536123 575255 629334 676928 816829 849206 905578 925247 1014676 1177938 1196816 1212567 1230579 1245082 1261764 1282512 1302343 1321387 1343990 1360644 1378739 1445975 1511692 1528973 1553969 1610875 1674742 1765264 1782854 1798146 1815826 2263115 2356752 2477932 2539096 2561329 2596781 2619892 2649894 2695116 2816220 2847838 2884564 2972811 2985473 3034858 3083924 3430393 3480896 3500393 3515457 3530768 3548019 3574031 3602075 3718234 3747447 3790679 3805410 3822312 3859678 3908896 3957468 3996924 4142646 4200380 4245744 4277799 4315558 4428088 4548642 4563303))

(define (play-rhythm ps pitches indices)
  ;(define ps (if (not ps) (make-pstream) ps))
  (map (lambda (p i) (pstream-queue ps p i))
       (map make-pitch pitches)
       indices))

(define (play-rhythm-drums ps indices)
  ;(define ps (if (not ps) (make-pstream) ps))
  (map (lambda (i) (pstream-queue ps (list-ref (list clap-1 bassdrum) (random 2)) i))
       indices))


(define (create-scale fifth subdivs num-notes)
  (let ((note-indices (sort (map (lambda (x) (modulo (* fifth x) subdivs))
                                 (iota num-notes))
                            <)))
    (lambda (start)
      (map (lambda (i) (* start (expt 2 (/ i subdivs))))
           note-indices))))

(define pentatonic-lydian (create-scale 7 12 5))
(define lydian (create-scale 7 12 7))
(define lydian-blues (create-scale 7 12 7))
(define chromatic (create-scale 7 12 12))

(define (create-random-melody scale len)
  ;(play (create-random-melody (lydian 400) 10))
  (let ((scale+oct (append scale
                            (cons (* 2 (car scale)) '()))))
     (map (lambda _ (list-ref scale (random (length scale))))
                    (iota len))))

(define constr (rs-read "/home/scottviteri/LocalSoftware/ConstructionSound/construction.wav"))

(define (play-construction frac-len)
  (define last-index (exact-floor (* frac-len (rs-frames constr))))
  (define ps (make-pstream))
  (define some-peaks (filter (lambda (x) (<= x last-index)) peaks))
  ; maybe difficult to sync pstream queues in general
  (pstream-queue ps (clip constr 0 last-index) 5000)
  (play-rhythm ps (create-random-melody (lydian 400) (length some-peaks)) some-peaks)
  ;(play-rhythm-drums ps some-peaks)
  )

(play-construction .3)
(sleep 100)

; can also get rsound from indices using assemble
(define hits
  (assemble (zip (map make-pitch
                    (create-random-melody (lydian 400) (length peaks)))
               peaks)))


(define (plot-1-sec rs)
  (plot (points (map (lambda (i) (vector i (rs-ith/left rs i))) (iota 8820 0 10))
                #:sym 'fullcircle1)))

;(plot-1-sec hits)
; ISSUE -- rs not enough
;(rs-frames hits)
;242162
;zoop.rkt> (rs-frames constr)
;4654080
; 1000 constr -> 13000 hits
