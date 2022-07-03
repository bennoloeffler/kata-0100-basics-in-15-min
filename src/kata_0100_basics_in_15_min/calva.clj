(ns kata-0100-basics-in-15-min.calva)


; -------------------------------------------------------------------
;           vs-code  basics 
; -------------------------------------------------------------------

; install vs code
; install calva
; open this directory in vs code (cd kata-bel; code .)
; press F1 and find calva commands (macos SHIFT-COMMAND-P, linux STRG-ALT-P)
; find: "start a project repl (Jack-In)" (CTRL-OPTION-C CTRL-OPTION-J)


;place cursor between 1 and 2 and evaluate top-form (OPTION-Enter)
(+ 1 2) ;; spaces are whitespace - obvoiusly

; evaluate next left-form ( go to the 1 and CONTROL-Enter)
(+ 1, 2) ;; but commas are whitespace, too! 
; try a TAB to format code
; try two spaces at the beginning and then option+i (par infer)
; try Shift-Tab
; undo if you messed up ;-)

; barf/slurp left
; barf/slurp right
; place the cursor at the 2
; then play with CTRL-OPTION-arrow-left / arrow-right
; and try SHIFT-CTRL-OPTION-arrow-left / arrow-right
(+ 1 2 (* 12 10))

; try CTRL-W in the expression above

; go to * in the line above and press F12 (go to Definition)
; then press COMMAND - (go back)

; type 'comm' and then TAB
; try with def, let, defn, defn-doc
; for experimentation, work inside those comments.
; because, when you load the file, forms in comments are ignored.


(println "this will be printetd loading the file")
; try it: CTRL-OPTION-C Enter loads the file and evaluates it in repl
; you may have a look to the file "output.calva-repl". This is where *out* caused by println is sent to.

(comment
  ( + 5 6)
  )

(comment ; inside those comments, calva treats every s-expression as top-level (try OPTION-Enter) 

  (println "this will _not_ be printed when you load the file")
  ; #_ this is a kind of comment for a single form - details later...
  (+ 1 2 (* 12 10 #_"place-cursor-here-and-press-CTRL-SHIFT-Enter")) ; eval the current form
  (+ 1 2 (* 12 #_"place-cursor-before-and-OPTION-SHIFT-Enter" 10)) ; eval up to here

  (inc 15)

  (set! *print-length* 10)
  (println (range)) ; this will NOT try to print "all", but only 10 
  
  (range) ; infinite range, calva does stop printing after the first 100 elements

  (set! *print-length* nil)
  (println (range)) ; this will try to print "all" - infinitely! 
  ; infinite loops may be stopped with CTRL-OPTION-C CTRL-OPTION-D (without killing the REPL)

  ; this nil will prevent the paran to jump up from Option-i
  nil
  )



