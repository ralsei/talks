#lang at-exp slideshow
(require latex-pict
         pict/conditional
         pict/flash
         pict/shadow
         pict/tree-layout
         ppict/2
         ppict/slideshow2
         slideshow/code
         slideshow/staged-slide
         slideshow-text-style
         threading)

(define $ (curry tex-math #:scale 3.5))
(define $$ (curry tex-display-math #:scale 4))

;; these fonts are good but you don't have to use them
(define *global-font* "IBM Plex Sans")
(define *mono-font* "Julia Mono")

(current-main-font *global-font*)
(current-code-font *mono-font*)
(get-current-code-font-size (thunk 20)) ;; ???

;;;; helper functions
#;(define (frame p)
  (refocus (cc-superimpose
            p
            (rounded-rectangle (+ (pict-width p) 10)
                               (+ (pict-height p) 10)
                               #:border-width 3
                               #:border-color "blue"))
           p))
(define (frame p)
  (refocus (cc-superimpose
            (filled-rectangle (+ (pict-width p) 10)
                              (+ (pict-height p) 10)
                              #:border-width 3
                              #:border-color "black"
                              #:color "white")
            p)
           p))

(define-syntax-rule (pslide/staged [name ...] arg ...)
  (staged [name ...] (pslide arg ...)))

(define (authors whos where)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([author-name #:size 30 #:color "blue"]
     [institution #:size 20])

    (vc-append (current-line-sep)
               (apply hc-append 50
                      (for/list ([who (in-list whos)])
                        (colorize (author-name who) "blue")))
               (blank (/ (current-font-size) 3))
               (scale/improve-new-text (institution where) 0.8))))

(define (take* lst n)
  (if (> n (length lst))
      lst
      (take lst n)))

(define (make-bang radius [text ""])
  (define bang (cc-superimpose (colorize (filled-flash radius radius) "red")
                               (colorize (filled-flash (- radius (/ radius 5)) (- radius (/ radius 5))) "orange")))
  (with-text-style
    #:defaults [#:face *global-font*]
    ([bang-t #:size (round (/ radius 6))])
    (cc-superimpose bang (bang-t text))))

(define (section-card text)
  (pslide
   (with-text-style
     #:defaults [#:face *global-font*]
     ([t #:size 100 #:bold? #t])
     (t text))))

;;;; actual slides
(define (title-slide)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([heading #:size 50 #:bold? #t])

    (pslide
     #:go (coord 0.5 0.7 'ct)
     (scale-to-fit (bitmap "quote.png")
                   (get-client-w)
                   (get-client-h))
     #:go (coord 0.5 0.4 'cc)
     (vc-append
      (current-line-sep)
      @heading{Automating the Design Recipe}
      (authors
       '("Hazel Levine" "Sam Tobin-Hochstadt") 
       "Indiana University")))))

(define (htdp-slides)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 35]
     [tt #:size 35 #:face *mono-font*]
     [ti #:size 35 #:italic? #t])

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What is HtDP?}
     #:go (coord 0.05 0.2 'lt)
     @item[#:width 700]{A curriculum involving pedagogical subsets of the Racket language}
     @item[#:width 700]{Used at Indiana for the C211 course}
     @item[#:width 700]{Emphasizes reasoning based on the @ti{design recipe}: a "formula"
                        ensuring signature and test driven design} 
     #:go (coord 0.75 0.55 'cc)
     (bitmap "htdp-cover.gif"))

    (pslide/staged
     [structure enumeration union]
     #:go (coord 0.05 0.05 'lt)
     @title{The template}
     #:go (coord 0.05 0.2 'lt)
     @t{We have a correspondence between the shape of a data definition,
        and the shape of structural decomposition of that data definition:}
     #:go (coord 0.05 0.55 'lt)
     (pict-case
      stage-name #:combine lt-superimpose
      [(structure) (code (code:comment "A Date is a (make-date Number String Number)")
                         (define-struct date (year month day)))]
      [(enumeration) (code (code:comment "A TrafficLight is one of:")
                           (code:comment "- \"red\"")
                           (code:comment "- \"yellow\"")
                           (code:comment "- \"green\""))]
      [(union) (code (code:comment "A TreeOfDates is one of:")
                     (code:comment "- (make-leaf)")
                     (code:comment "- (make-node TreeOfDates Date TreeOfDates)")
                     (define-struct leaf ())
                     (define-struct node (left date right)))])
     #:go (coord 0.5 0.55 'lt)
     (pict-case
      stage-name #:combine lt-superimpose 
      [(structure) (code (define (process-date d)
                           (... (date-year d) ...
                                (date-month d) ...
                                (date-day d) ...)))]
      [(enumeration) (code (define (process-trafficlight tl)
                             (cond [(string=? tl "red") ...]
                                   [(string=? tl "yellow") ...]
                                   [(string=? tl "green") ...])))]
      [(union) (code (define (process-treeofdates tod)
                       (cond [(leaf? tod) ...]
                             [(node? tod)
                              (... (process-treeofdates (tod-left tod))
                                   (process-date (tod-date tod))
                                   (process-treeofdates (tod-right tod)))])))]))

    (pslide/staged
     [step-1 step-2 step-3 step-4 step-5 step-6 input-output]
     #:go (coord 0.05 0.05 'lt)     
     @title{The design recipe}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      (show @t{1. Data definitions}
            (at/after step-1))
      (show @t{2. Signature, purpose}
            (at/after step-2))
      (show @t{3. Unit tests/examples}
            (at/after step-3))
      (show @t{4. Template}
            (at/after step-4))
      (show @t{5. Function definition}
            (at/after step-5))
      (show @t{6. Testing}
            (at/after step-6)))
     #:go (coord 0.34 0.2 'lt)
     (show (vl-append
            (current-line-sep)
            @t[#:color "red"]{(input)}
            @t[#:color "red"]{(input)}
            @t[#:color "red"]{(input)}
            @t[#:color "blue"]{(output)}
            @t[#:color "dark green"]{(???)}
            @t[#:color "blue"]{(output)})
           (at/after input-output))
     #:go (coord 0.45 0.2 'lt)
     (show (code (code:comment "A ListOfNumbers is one of:")
                 (code:comment "- empty")
                 (code:comment "- (cons Number ListOfNumbers)"))
           (at/after step-1))
     (ghost (rectangle 30 30)) ; eh
     (show (code (code:comment "multiply-by : ListOfNumbers Number -> ListOfNumbers")
                 (code:comment "multiplies every element in the list by n"))
           (at/after step-2))
     (show (code (check-expect (multiply-by empty 3) empty)
                 (check-expect (multiply-by (list 1 2 3) 2) (list 2 4 6))
                 (check-expect (multiply-by (list 7 2 1) 3) (list 21 6 3)))
           (at/after step-3))
     (ghost (rectangle 30 30))
     (pict-case
      stage-name
      [(step-1 step-2 step-3) (ghost (rectangle 30 30))]
      [(step-4) (code (define (multiply-by ls n)
                        (cond [(empty? ls) ...]
                              [(cons? ls) (... (first ls)
                                               (multiply-by (rest ls) n) ...)])))]
      [else (code (define (multiply-by ls n)
                    (cond [(empty? ls) empty]
                          [(cons? ls) (cons (* (first ls) n)
                                            (multiply-by (rest ls) n))])))])
     (ghost (rectangle 30 30))
     (show @tt[#:size 20]{All 3 tests passed!}
           (at/after step-6)))

    ;; TODO: it would be great to show the expanding environment on this slide
    (pslide/staged
     [initial minimized bad-answer expand-template fill-blanks hell-yeah]
     #:go (coord 0.05 0.05 'lt)     
     @title{Steps 4-6 as a closed loop}
     #:go (coord 0.05 0.2 'lt)
     (parameterize ([get-current-code-font-size
                     (case stage-name
                       [(initial) (thunk 20)]
                       [else (thunk 12)])])
       (code (code:comment "A TreeOfNumbers is one of:")
             (code:comment "- (make-leaf)")
             (code:comment "- (make-node TreeOfNumbers Number TreeOfNumbers)")
             (define-struct leaf ())
             (define-struct node (left value right))
             code:blank
             (code:comment "depth : TreeOfNumbers -> Number")
             (code:comment "computes the maximum depth of the tree")
             (check-expect (depth (make-leaf)) 0)
             (check-expect (depth (make-node (make-leaf) 1 (make-leaf))) 1)
             (check-expect (depth (make-node (make-leaf)
                                             4
                                             (make-node (make-leaf) 1 (make-leaf))))
                           2)))
     (ghost (rectangle 30 30))
     (pict-case
      stage-name
      [(initial minimized)
       (code (define (depth tree)
               {... : Number}))]
      [(bad-answer)
       (code (define (depth tree)
               0))]
      [(expand-template)
       (code (define (depth tree)
               (cond [(leaf? tree) {... : Number}]
                     [(node? tree) {... : Number}])))]
      [(fill-blanks)
       (code (define (depth tree)
               (cond [(leaf? tree) {... : Number}]
                     [(node? tree) (max {... : Number}
                                        {... : Number})])))]
      [(hell-yeah)
       (code (define (depth tree)
               (cond [(leaf? tree) 0]
                     [(node? tree) (max (depth (node-left tree))
                                        (depth (node-right tree)))])))])
     (ghost (rectangle 30 30))
     (pict-case
      stage-name
      [(bad-answer) @tt[#:size 20]{Two tests failed!}]
      [(hell-yeah) @tt[#:size 20]{All 3 tests passed!}]))))

(define (implementation-slides)
  (define (tt s #:size [size 35])
    (text s *mono-font* size))

  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 35]
     ;[tt #:size 35 #:face *mono-font*]
     [ti #:size 35 #:italic? #t])

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{The Myth program synthesizer}
     #:go (coord 0.5 0.2 'ct)
     (shadow-frame @t{Peter-Michael Osera and Steve Zdancewic.
                      Type-and-Example-Directed Program Synthesis.
                      (PLDI ’15)})
     (vl-append
      (current-line-sep)
      @item{Describes a program synthesizer for a much simpler, ML-like language}
      @item{Extremely performant, but has some constraints that make it not what we want}))

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{How Myth gets really close}
     #:go (coord 0.5 0.55 'cc)
     (scale-to-fit (bitmap "myth.png") (get-client-w) (- (get-client-h) 200)))
    
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What's different in Beginning Student?}
     #:go (coord 0.05 0.2 'lt)
     (vl-append
      (current-line-sep)
      @item{Non-inductive data}
      @item{Constants}
      @item{Signatures that don't behave like types}
      @item{A large collection of primitives})) 
    
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{The core algorithm}
     #:go (coord 0.05 0.2 'lt)
     #;(vl-append
      (current-line-sep)
      @t{Build a (potentially infinite) tree, where:}
      @item{The vertices are partial programs}
      @item{The edges are refinements we can run}
      @t{When we reach a complete program, run tests})
     #:go (coord 0.45 0.55 'cc)
     (parameterize ([get-current-code-font-size (thunk 12)])
       (binary-tidier
        #:x-spacing 200
        #:y-spacing 100
        (tree-layout
         #:pict
         (frame
          (code (define (length ls)
                  {... : Number})))
         (tree-edge (tree-layout
                     #:pict
                     (frame
                      (code (define (length ls)
                              (+ {... : Number}
                                 {... : Number}))))
                     #f #f))
         (tree-edge (tree-layout
                     #:pict
                     (frame
                      (code (define (length ls)
                              (cond [(empty? ls) {... : Number}]
                                    [(cons? ls) {... : Number}]))))
                     (tree-layout
                      #:pict
                      (frame
                       (code (define (length ls)
                               (cond [(empty? ls) (abs {... : Number})]
                                     [(cons? ls) {... : Number}]))))
                      #f #f)
                     (tree-layout
                      #:pict
                      (frame
                       (code (define (length ls)
                               (cond [(empty? ls) 0]
                                     [(cons? ls) (add1 {... : Number})]))))
                      (tree-layout
                       #:pict
                       (frame
                        (code (define (length ls)
                                (cond [(empty? ls) 0]
                                      [(cons? ls) (add1 (first ls))]))))
                       #f #f)
                      (tree-layout
                       #:pict
                       (frame
                        (code (define (length ls)
                                (cond [(empty? ls) 0]
                                      [(cons? ls) (add1 (length (rest ls)))]))))
                       #f #f))))))))

    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Our refinements}
     #:go (coord 0.05 0.2 'lt)
     @item{@tt{introduce-lambda}: for any @tt{X -> Y} signature}
     @item{@tt{guess-var}: given a matching var in the environment, plug the hole with it}
     @item{@tt{guess-const}: guess a known constant or one from a unit test}
     @item{@tt{guess-app}: given an @tt{X -> Y}, and an @tt{X} hole, make a @tt{Y} hole}
     @item{@tt{guess-template}: given an inductive @tt{X} in the environment,
           try its template and extend the environment with any recursion})

    ;; more examples?
    ;; the search space is pretty big so I can't reasonably be exhaustive
    ))

(define (future-work-slides)
  (with-text-style
    #:defaults [#:face *global-font*]
    ([title #:size 50 #:bold? #t]
     [titlett #:size 50 #:bold? #t #:face *mono-font*]
     [t #:size 35]
     [tt #:size 35 #:face *mono-font*]
     [ti #:size 35 #:italic? #t])

    ;; autograding (talk about 211 checkers),
    ;; determining inconsistent unit tests
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{What's this good for?}
     #:go (coord 0.5 0.2 'ct)
     (scale-to-fit
      (bitmap "land-rocket.png")
      (get-client-w)
      (get-client-h))
     #:go (coord 0.05 0.5 'lt)
     @item[#:width (get-client-w)]{Making autograding scripts less manual and tedious to make}
     @item[#:width (get-client-w)]{Determining inconsistent sets of unit tests}
     ;; see land-rocket example:
     ;; we have so many ways that this could be correct,
     ;; so what we're really enforcing is that the function looks like something that's correct,
     ;; which we can verify with hole plugging and synthesis using the correct constants
     @item[#:width (get-client-w)]{Checking that a function follows a certain "shape"}
     )

    ;; Dijkstra's algorithm,
    ;; check narrowing,
    ;; refinement trees from Myth
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Making it faster}
     #:go (coord 0.05 0.2 'lt)
     @item{Using Dijkstra's algorithm instead of breadth-first search}
     @item{Removing unit tests from consideration inside conditionals}
     @item{Using refinement trees from Myth}
     #:go (coord 0.6 0.35 'lt)
     (scale-to-fit (bitmap "stutter-tree.png")
                   (get-client-w)
                   (- (get-client-h) 280)))

    ;; Rosette
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Making it do more}
     #:go (coord 0.05 0.2 'lt)
     @item{We can't easily apply this algorithm for arithmetic/string functions}
     @item{Calling another synthesizer, like Rosette, would work}
     @item{This can be implemented as another refinement}
     #:go (coord 0.5 0.9 'cb)
     (scale-to-fit (bitmap "days-to-year.png")
                   (- (get-client-w) 200)
                   (get-client-h)))

    ;; DrRacket Quickscript,
    ;; a decent API for checkers
    (pslide
     #:go (coord 0.05 0.05 'lt)
     @title{Making it user-friendly}
     #:go (coord 0.05 0.2 'lt)
     @item{We need some way to call this}
     @item{We provide a basic API that parses comments from student files}
     @item{We have a DrRacket Quickscript that runs the synthesizer}
     #:go (coord 0.05 0.45 'lt)
     (scale-to-fit (bitmap "bingus1.png")
                   (- (/ (get-client-w) 2) 100)
                   (get-client-h))
     #:go (coord 0.95 0.45 'rt)
     (scale-to-fit (bitmap "bingus2.png")
                   (- (/ (get-client-w) 2) 100)
                   (get-client-h)))
    ))

;;;; main
(module+ main
  (title-slide)
  (section-card "1. How to Design Programs")
  (htdp-slides)
  (section-card "2. Implementation")
  (implementation-slides)
  (section-card "3. Future directions")
  (future-work-slides)
  (section-card "Thank you!")
  )
