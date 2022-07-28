\version "2.19.15"

\header {
  tagline = ##f
}

#(define-event-class 'music-boxer-event 'span-event)

#(define-event-class 'box-event 'music-event)

#(define (add-grob-definition grob-name grob-entry)
   (let* ((meta-entry   (assoc-get 'meta grob-entry))
          (class        (assoc-get 'class meta-entry))
          (ifaces-entry (assoc-get 'interfaces meta-entry)))
     ;; change ly:grob-properties? to list? to work from 2.19.12 back to at least 2.18.2
     (set-object-property! grob-name 'translation-type? ly:grob-properties?)
     (set-object-property! grob-name 'is-grob? #t)
     (set! ifaces-entry (append (case class
                                  ((Item) '(item-interface))
                                  ((Spanner) '(spanner-interface))
                                  ((Paper_column) '((item-interface
                                                     paper-column-interface)))
                                  ((System) '((system-interface
                                               spanner-interface)))
                                  (else '(unknown-interface)))
                          ifaces-entry))
     (set! ifaces-entry (uniq-list (sort ifaces-entry symbol<?)))
     (set! ifaces-entry (cons 'grob-interface ifaces-entry))
     (set! meta-entry (assoc-set! meta-entry 'classes (list class)))
     (set! meta-entry (assoc-set! meta-entry 'name grob-name))
     (set! meta-entry (assoc-set! meta-entry 'interfaces
                        ifaces-entry))
     (set! grob-entry (assoc-set! grob-entry 'meta meta-entry))
     (set! all-grob-descriptions
           (cons (cons grob-name grob-entry)
             all-grob-descriptions))))

#(define (make-box thick padding xext yext)
   (let ((xext (interval-widen xext padding))
         (yext (interval-widen yext padding)))
   (ly:stencil-add
    (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
    (make-filled-box-stencil xext (cons (cdr yext) (+ (cdr yext) thick)))
    (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
    (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

#(define (music-boxer-stencil grob)
   (let* ((elts (ly:grob-object grob 'elements))
          (refp-X (ly:grob-common-refpoint-of-array grob elts X))
          (X-ext (ly:relative-group-extent elts refp-X X))
          (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
          (Y-ext (ly:relative-group-extent elts refp-Y Y))
          (padding (ly:grob-property grob 'padding 0.3))
          (stil (make-box 0.1 padding X-ext Y-ext))
          (offset (ly:grob-relative-coordinate grob refp-X X)))
     (ly:stencil-translate-axis stil (- offset) X)))

#(define box-stil music-boxer-stencil)

#(add-grob-definition
  'Box
  `(
     (stencil . ,box-stil)
     (meta . ((class . Item)
              (interfaces . ())))))

#(add-grob-definition
  'MusicBoxer
  `(
     (stencil . ,music-boxer-stencil)
     (meta . ((class . Spanner)
              (interfaces . ())))))


#(define box-types
   '(
      (BoxEvent
       . ((description . "A box encompassing music at a single timestep.")
          (types . (general-music box-event music-event event))
          ))
      ))

#(define music-boxer-types
   '(
      (MusicBoxerEvent
       . ((description . "Used to signal where boxes encompassing music start and stop.")
          (types . (general-music music-boxer-event span-event event))
          ))
      ))


#(set!
  music-boxer-types
  (map (lambda (x)
         (set-object-property! (car x)
           'music-description
           (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
    music-boxer-types))

#(set!
  box-types
  (map (lambda (x)
         (set-object-property! (car x)
           'music-description
           (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
    box-types))

#(set! music-descriptions
       (append music-boxer-types music-descriptions))

#(set! music-descriptions
       (append box-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))


#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

musicBoxerEngraver =
#(lambda (context)
   (let ((span '())
         (finished '())
         (current-event '())
         (event-start '())
         (event-stop '()))
     
     `((listeners
        (music-boxer-event .
          ,(lambda (engraver event)
             (if (= START (ly:event-property event 'span-direction))
                 (set! event-start event)
                 (set! event-stop event)))))
       
       (acknowledgers
        (note-column-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)
                  (add-bound-item span grob)))
             (if (ly:spanner? finished)
                 (begin
                  (ly:pointer-group-interface::add-grob finished 'elements grob)
                  (add-bound-item finished grob)))))
        
        (inline-accidental-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))
        
        (script-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))
        
        (finger-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))
        
        ;; add additional interfaces to acknowledge here
        
        )
       
       (process-music .
         ,(lambda (trans)
            (if (ly:stream-event? event-stop)
                (if (null? span)
                    (ly:warning "No start to this box.")
                    (begin
                     (set! finished span)
                     (ly:engraver-announce-end-grob trans finished event-start)
                     (set! span '())
                     (set! event-stop '()))))
            (if (ly:stream-event? event-start)
                (begin
                 (set! span (ly:engraver-make-grob trans 'MusicBoxer event-start))
                 (set! event-start '())))))
       
       (stop-translation-timestep .
         ,(lambda (trans)
            (if (and (ly:spanner? span)
                     (null? (ly:spanner-bound span LEFT)))
                (ly:spanner-set-bound! span LEFT
                  (ly:context-property context 'currentMusicalColumn)))
            (if (ly:spanner? finished)
                (begin
                 (if (null? (ly:spanner-bound finished RIGHT))
                     (ly:spanner-set-bound! finished RIGHT
                       (ly:context-property context 'currentMusicalColumn)))
                 (set! finished '())
                 (set! event-start '())
                 (set! event-stop '())))))
       
       (finalize
        (lambda (trans)
          (if (ly:spanner? finished)
              (begin
               (if (null? (ly:spanner-bound finished RIGHT))
                   (set! (ly:spanner-bound finished RIGHT)
                         (ly:context-property context 'currentMusicalColumn)))
               (set! finished '())))
          (if (ly:spanner? span)
              (begin
               (ly:warning "unterminated box :-(")
               (ly:grob-suicide! span)
               (set! span '()))))))))


boxEngraver =
#(lambda (context)
   (let ((box '())
         (ev '()))
     
     `((listeners
        (box-event .
          ,(lambda (engraver event)
             (set! ev event))))
       
       (acknowledgers
        (note-column-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:grob? box)
                 (begin
                  ; (set! (ly:grob-parent box X) grob) ;; ??
                   (set! (ly:grob-parent box Y) grob)
                 (ly:pointer-group-interface::add-grob box 'elements grob)))))
        
        (inline-accidental-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))
        
        (script-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))
        
        (finger-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:item? box)
                 (ly:pointer-group-interface::add-grob box 'elements grob))))
        
        ;; add additional interfaces to acknowledge here
        
        )
       
       (process-music .
         ,(lambda (trans)
            (if (ly:stream-event? ev)
                (begin
                 (set! box (ly:engraver-make-grob trans 'Box ev))
                 (set! ev '())))))
       (stop-translation-timestep .
         ,(lambda (trans)
            (set! box '()))))))

musicBoxerStart =
#(make-span-event 'MusicBoxerEvent START)

musicBoxerEnd =
#(make-span-event 'MusicBoxerEvent STOP)

box = #(make-music 'BoxEvent)

\layout {
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
  \context {
    \Score
    \consists \musicBoxerEngraver % for spans
    \consists \boxEngraver
  }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EXAMPLE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

melody = \relative c'' {
  \set fingeringOrientations = #'(left)
  %1
  \repeat volta 2 {
    %\once\override Score.Box.padding = 1
    \box <g-3  c-2 f-1>1
    \musicBoxerStart d8-4 g,-0 d' g, d'-4 g,-0 d' \musicBoxerEnd g,
  }

  %2
  \repeat volta 2 {
    \box <d'-4  c'-2 f-1>1\f\fermata
    \musicBoxerStart g8-3 d-0 g d g8-4 d-0 g \musicBoxerEnd d\accent
  }
}

\score {
  \new Staff \melody
}

