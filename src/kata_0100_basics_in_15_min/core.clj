
; -------------------------------------------------------------------
;           clojure basics 
; -------------------------------------------------------------------

; Clojure is written in "forms", which are just
; lists of things inside parentheses, separated by whitespace.
;
; The clojure reader assumes that the first thing is a
; function or macro to call, and the rest are arguments.

(inc 15) ;; This is a meaningless single line comments.
(println "Hello Armin!") ;; returns nil, prints on output

; There are block comments, too.
; This is a block comment -> ; instead of ;; 
; Block comments will be formated as _one_ block by some editors.
; By convention, we assume, that the lines belongs together.

; You may see some help text from your IDE regarding operators.
; Hover over map, filter, reduce and see documentation and examples.
map     ; map, filter and reduce may be known by java folks.
filter  ; HOFs (higher oder functions) are functions,
reduce  ; that take or create other functions.


; -------------------------------------------------------------------
;          useful docs 
; -------------------------------------------------------------------

; Cheat sheat - to get an idea, when you are exploring.
; https://clojure.org/api/cheatsheet

; API - for an overview of namespaces.
; https://clojure.github.io/clojure/clojure.core-api.html

; Community driven docs - many examples, e. g. 
; https://clojuredocs.org/clojure.core/map

; Style guide - formatting and idioms, that prooved good.
; https://guide.clojure.style/


; -------------------------------------------------------------------
;          namespaces 
; -------------------------------------------------------------------

; namespaces keep our names from colliding with
; existing names, e.g. in the clojure.string lib.
; The form do declare and move into a namespace is:
(ns kata-0100-basics-in-15-min.core)
; kind of packages from java

; You may need to use other namespaces of your own code or from libs. 
; Then ":require" them ":as" shortcut or ":refer" to specific symbols 
; inside the namespace to use them without prefix.  
(ns kata-0100-basics-in-15-min.core
  (:require [clojure.set :as set]
            [clojure.string :as str :refer [join]]
            [erdos.assert :as ea]))
; This constuct can be found at the beginning of clojure files
; File naming is typically kata_0100_basics_in_15_min/core
; So the point marks subdirectories. The - is replaced by _

; now we can use symbols of namespace clojure.set
; by prefixing them with set/
(set/intersection
 #{1 :a "b" false}
 #{11 :a "b" true})

; or from erdos.assert prefixing with ea
(ea/assert (= (+ 3 4) (- 10 3)))
;(ea/assert (= (+ 3 4) (- 10 4))) ; this fails...

; the function join from the namespace clojure.string
; may be used in different forms - depending on how the namespace 
; was made available (:require :as :refer):
clojure.string/join ;; plain :require -> complete namespace as prefix
str/join ;; :require :as str -> abbrevation str as prefix
join ;; :require :refer [join] -> no prefix at all

; you may leave things out...
; This is is also a kind of comment - an IGNORED form - because of #_
#_(ns kata-0100-basics-in-15-min.core
    (:require [clojure.set]
              [clojure.string :as str]))

; You may use the complete namespace with ALL symbols
; (use 'clojure.string)
; This is NOT recommended, because name clashes may become a problem.


; -------------------------------------------------------------------
;          lisp = LISt Processing  
; -------------------------------------------------------------------

; lisp is called lisp, because it's a list-processor...
; Lists consists of symbols between paranthesis: (str 2 3 "x" false)
; Lists are executed, if not quoted by ' or constructed by 'list'
; so let's look at lists
; Lists have constant O(1) access and adding to the first element.
; Everything else is O(n).
; Vectors have random access in O(1).

(+ 2 3)
(str 2 \x 3 " = " (* 2 3))

; list can serve as data, data is code = AST = homoiconic

(quote (+ 3 4)) ; the "reader" prevents the list from beeing executed
'(+ 2 3) ; just an abbrevation
(list + 2 3) ; construct a list by the 'list' function
(list '+ 2 3) ; dont resolve the +
(conj '(0 6) 2) ; add at the beginning
(first (list 2 6 0)) ; get the first
(rest (list 2 6 0)) ; get the rest

; -------------------------------------------------------------------
;          combining functions and data  
; -------------------------------------------------------------------

; in clojure, there are other data structures, too
; there is a vector (random acess)
[1 2 3] ; reader literals to create a vector
(vector 1 2 3) ; create a new one
(vec '(3 4 5)) ; build from list
; vectors have random access in constant time

; this is an anonymous function - not attached to a symbol
; f(x) = 2 * x
(fn [x] (* 2 x))
; seems not useful - but wait a moment...

; 'map' applies the function inc to every element of the vector
(map inc [1 2 3])

; this uses our anonymous function from above
; to be mapped to 1 2 3
(map (fn [x] (* 2 x))
     [1 2 3])

; this is the same, only shorter
(map #(* 2 %)
     [1 2 3])

; filter out those, that fit a predicate, e.g. odd?
(filter odd? [1 2 3 4 5 6]) ;; => (1 3 5)

; take the result of filter and square it
(map #(* % %) (filter odd? [1 2 3 4 5 6])) ;; => (1 9 25)

; finally, reduce it by +
(reduce + (map #(* % %) (filter odd? [1 2 3 4 5 6]))) ;; => 35

; where does the reduce start? look at this... pure operators deliver the "identity-value" (called somehow like this ;-)
; If there is no start value, it takes the first two elements in the collection.
; But sometimes, this comes in handy...
(+)
(*)

(reduce + 100 (map #(* % %) (filter odd? [1 2 3 4 5 6]))) ;; starts with 100

; write the same more readable... 
; with the so called "thread last" macro
; it puts the last result at the very end of the next parameter list, 
; there: (  ,,,)
(->> [1 2 3 4 5 6]
     (filter odd? ,,,)
     (map #(* % %) ,,,)
     (reduce + 100 ,,,))

; see what the "threading macro does..."
(macroexpand '(->> [1 2 3 4 5 6]
                   (filter odd?)
                   (map #(* % %))
                   (reduce + 100)))

; A little bit more understandable may be this one.
; In the first line, the vector is bound to a symbol, $ in this example.
; Then, each line rebinds its result to $ and it is used at the
; right position in the next line.
; You may do it step by step... SHIFT-OPTION-ENTER
(as-> ["Benno" "Leo" "Sabine" "Paul"] $
  (map str/upper-case $)
  (join "---" $)
  (replace {\E \3} $)
  (vec $)
  (update $ 3 #(int %))
  (str/join $))

; thread first macro puts the last result right after the function
; at the beginning of the parameter list.
(-> "a b c d"
    str/upper-case
    (str/replace "A" "X")
    (str/split #" ")
    reverse
    last)

; -------------------------------------------------------------------
;            defining symbols, variables and functions 
; -------------------------------------------------------------------

; lexical scoped variables - invisible after that expression 
(let [x 10 ; x is defined ony in this scope
      y 20
      z (+ x y)]
  (* x y z)) ; x is not defined any more after closing behind let

; KATA: remove comment from x and evaluate
; then (def x 1), come back and re-evaluate 
;x ; it is not 10... 

; this x is different: it is globally scoped,
; part of this namespace and 
; visible from everywhere
; HINT: use rarely. Use 'let' whenever possible!
(def x 1)
(+ x 5) ; does not change x and results in 6!

; define a function and attatch a symbol (name) to it
(def half-of-v1
  (fn [x] (/ x 2)))

(half-of-v1 10)

; identical - with a string as doc
; formatting 
(defn half-of-v2
  "returns x divided by 2"
  [x]
  (/ x 2))

; KATA: try with 9, with 10.0 and with 10/2
(half-of-v2 10)

; multi-arity
(defn dimension
  ([] "No dimension. Walhalla. Heaven. Nirvana.")
  ([x] (str "We live on a line at adress: " x))
  ([long lat] (str "On a ideal round planet, long = " long ", lat = " lat))
  ([x y z] (str "somewhere in space. " x " " y " " z))
  ([x y z & more] (str (dimension x y z) ", and beyond imagination: " more)))

(dimension)
(dimension 15)
(dimension 11 12)
(dimension 11 12 44)
(dimension 1 2 3 4 5 6 7)

; this is a "stackoverflow-bomb..."
(defn recurse-somehow-3
  ([n] (recurse-somehow-3 n []))
  ([n acc]
   (if (> n 0)
     (recurse-somehow-3 (- n 3) (conj acc n))
     acc)))

(recurse-somehow-3 10)
; (recurse-somehow-3 100000) ; try this... stack overflow

; This is proper recursion in clojure without building up stack.
; Basically, thats the way, functional looping is done.
; No stateful variables involved.
(defn recurse-somehow-3-proper
  [number]
  (loop [n number ; initialize n with n... seems strange
         acc []] ; initialize acc with []
    (if (> n 0)
      (recur (- n 3) (conj acc n)) ; re-enter the loop and set n to (n-3) and acc to (conj acc n)
      acc)))

(recurse-somehow-3-proper 100000)

; Re-Def's are only for debugging and repl. Remember that
; *earmuffs* are a convention for global, mutable variables.
; see: https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*clojure-version*

(def ^:dynamic *xyz* 30)
*xyz*

(alter-var-root #'*xyz* (fn [_] "7-eleven parking lot"))
*xyz*


(do ; alter-var-root is thread save!
  (def xyz 0)
  (future (dotimes [n 10000] (alter-var-root #'xyz inc)))
  (future (dotimes [n 10000] (alter-var-root #'xyz inc)))
  (Thread/sleep 200)
  xyz)


(do ; re-defs are not. TOTAL nogo. Except for debugging or repl.
  (def xyz-1 0)
  (future (dotimes [n 10000] (def xyz-1 (inc xyz-1))))
  (future (dotimes [n 10000] (def xyz-1 (inc xyz-1))))
  (Thread/sleep 200)
  xyz-1)

(alter-var-root #'*xyz* (fn [_] 7))
(let [v 3
      r (* v *xyz*)] ; use the global xyz
  [:v= v :xyz= *xyz* :r= r])


;(def ^:dynamic *xyz* 30)

(binding [*xyz* 100] ; change binding for this scope (and leave it identical for other threads and scopes)
  (let [v 3
        r (* v *xyz*)]
    [:v= v :xyz= *xyz* :r= r]))

*xyz* ; 


; -------------------------------------------------------------------
;          calling a function 
; -------------------------------------------------------------------

; As soon as you start calling those functions,
; it is expresed in the form of a list.
; The first symbol in the list is interpreted as operator
; or function. All the rest in the list are parameters.
(+ 1) ;; call with only one param
(+ 1 2) ;; call with two
(+ 1 2 3) ;; with 3 or more

; Math is straightforward
(- 2 1) ;; => 1
(* 1 2) ;; => 2
(/ 2 4) ;; => 1/2 which is a Ratio
(/ 2.0 4) ;; => 0.5 which is a Double
(/ 8 2 2 2) ;; => 1 (need to get used to the meaning 8/2/2/2)
(mod 16 5) ;; => 1 (modulo)
(quot 9.5 -5.0) ;; => -1.0 (quotient, integerish)
(quot 9 4)

; Math from Java
Math/PI

; use Java Math class:
; static method Math.sin(x) becomes (Math/sin x)
; static field Math.PI becomes Math/PI 
(let [pi Math/PI
      pi-half (/ pi  2)] ; bind two variables
  [(Math/sin pi-half) ; return a vector of two numbers,
   (Math/sin pi)]) ; sin(pi) is not exactly zero...
; the last expression of a function body is its return value

; Equality is also a function...
(= 1 1) ; => true
(= 2 1) ; => false
(= 1 1.0) ; => false, because different types
(== 1 1.0) ; => true, long or double does not matter

; KATA: try to subtract down to zero 
(- 10)
; HINT:
#_(- 10 2 3 1 2) ;is near zero

; KATA: operator or symbol? Evaluate it.
; remove the space between - and 10. Evaluate it.
'(- 10)

; KATA: Uncomment and evaluate (10) below.
;(10)
; See the Exception - this is a VERY common error.
; You try to evaluate a symbol as a function
; that is not a known function. does not impl IFN interface

; define your own function
; evaluating the function means:
; defining it.
(defn square
  "Just returns n times n."
  [x] ;; why call it x? see: https://github.com/bbatsov/clojure-style-guide#idiomatic-names
  (* x x))

; this is the longer form
(def square-twice
  "n * n * n * n"
  (fn [x] ;; why call it x? see: https://github.com/bbatsov/clojure-style-guide#idiomatic-names
    (* x x x x)))

; now use the function
(square 5)
(square-twice 5)

; KATA create a function 'triple' and use it.



; -------------------------------------------------------------------
;          boolean and logic 
; -------------------------------------------------------------------

; booleans are true or false
true
false

; logic
(not true)
(and true false)
(or true false)

; some operators evaluate to boolean
(> 4 3)
(> 5.2001 5.2)
(< 1 2 3 5)

; simple branching. More to come later. 
(if (< 1 100)
  (println "yes, one is smaller than 100")
  (println "oh noooo - 'if-expression' failed"))

; or and and deliver value!
(or nil (+ 16 1) false)
(and 15 false 17)
(and 15 16 17)

; if is an expression! it delivers value
; everything is an expression - some deliver nil.
; there are no statements!
(if nil
  "100"
  :something-else)

; bitwise logic, 
; see: https://books.google.de/books?id=xX38AgAAQBAJ&pg=PA36&lpg=PA36&dq=clojure+bitwise+operators

; -------------------------------------------------------------------
;           types 
; -------------------------------------------------------------------

; type returns java class or metadata-info (so dont use class)
(class 2) ;; no... instead use type
(type 2) ;; => java.lang.Long
(type 2.0) ;; => java.lang.Double
(type true) ;; => java.lang.Boolean
(type "abc") ;; => java.lang.String
(type \a) ;; => java.lang.Character
(type :key-word-is-not-a-string)
(type nil) ;; => nil (like null)

(type 2N) ;; => clojure.lang.BigInt
(type 2.0M) ;; => java.math.BigDecimal

;; operators supporting arbitrary precision
; +', -', *', inc', and dec'

; the collections and maps
; a persistent vector can contain everything 
; (functions, numbers, booleans, ...)
(type [inc 1 "2" :three \f false nil]) ; => clojure.lang.PersistentVector
(type #{1 2 :x "4"}) ; => clojure.lang.PersistentHashSet
(type {:x 3 :y 3}) ; => clojure.lang.PersistentArrayMap
(type '(1 2)) ; => clojure.lang.PersistentList
(type '()) ; => clojure.lang.PersistentList$EmptyList
(type #".*regex$")
(type 3/2)

; KATA: correct 6/2 and make the ratios equal
(= 3/2 6/2)
; HINT: 6/4 may be very near to 3/2

; KATA: add more parameters at the , and make it true...
(= 1.0 (- 10 2 2.0))
(== 1 (- 10 2 2.5 1.5 2))
; HINT: commas are like spaces and tabs: just whitespace
; HINT: (- 10 2 2.0 2 2 1) is not only near

; A more functional style of viewing all types 
; apply the function 'type' to every element in the vector...
(map #(vec [% (type %)]) [+
                            1
                            1N
                            2.0
                            3.0M
                            1/3
                            "2"
                            :three
                            \f
                            false
                            nil
                            '(1 2 3)
                            '()
                            {:1 2 :3 4 :5 6 :7 8 :9 1 :2 3 :4 5 :6 7 :8 9} ;; => big = PersistentHashMap
                            {:1 2} ;; => small = PersistentArrayMap
                            []
                            #{}])


; -------------------------------------------------------------------
;  resolving names, evaluating expressions, symbols, vars, values 
; -------------------------------------------------------------------

; TODO where to put that
; macros replace code with other code...
;(map for  [:foo :bar])    ; can't pass macros as functions

; see the difference
(+ 1 2) ; => 3
(quote (+ 1 2)) ; => (+ 1 2) as list
;shorthand for quoting
'(+ 1 2) ; => (+ 1 2)

; and the type? 
(type '(+ 1 2)) ;; => clojure.lang.PersistentList
(type (+ 1 2)) ;; => java.lang.Long, because its resolved.

; you may also do this with a symbol
+ ; unquoted symbol
(quote +) ; a quoted symbol will not be resolved
'+ ;; shorthand for (quote +)

(resolve '+) ;; resolve the symbol and get the var (which points to value)
#'+ ; shorthand to say "resolve"

; see the type of a symbol, that was resolved to a var
(type #'+) ; => clojure.lang.Var

; if we deref the var, we find the value (a function in case of +)
(var-get #'+)
@#'+ ; shortcut for dereferencing is @

; again...
+ ; the symbol, which gets fully resolved to a function by clojure
'+ ; quoted, just the symbol +
#'+ ; resolving the symbol results in the Var with namespace
@#'+ ; dereferencing the var results in the function (or value)
(=  +   @#'+) ; IDENTICAL

;; '+         #'+     @#'+
;; Symbol --> Var --> Value

;; in order to use it, you just need the symbol...
(+ 4 5)
; the + will be "resolved to a var, 
; dereferenced to a value (the function)
; and then called with parameters 4 5"

; this is, what happens behind the scenes
((var-get (resolve '+)) 4 5)  ; => identical to (+ 4 5) (as full name)

;; you may execute clojure code by 'eval'
(eval '(+ 4 5)) ; => evaluate the whole quoted list

; quoted undefined symbols are evaluated to a name.
; KATA: try to unquote (remove ') and evaluate
'this-is-not-defined-but-quoted
; HINT: be prepared for an error


; -------------------------------------------------------------------
;            strings 
; -------------------------------------------------------------------

; the shortes hello world of all...
; just evaluate the string as an expression.
"Hello World"

; create a string. Use str. There is no + operator for that.
(let [first "Hirokuni"
      last "Kim"]
  (str "My name is " first " " last))

; numbers to string
(str (+ 2 3 (* 5 100)) 999)

; or print it to system.out (console)
; println does a side effect and returns nil.
(println "Hello, world!" 42 "is meaningful?")
; without linefeed
(print "a")
(print "b")
(print "c")

; print data
(prn "abc" 123)

; use known formatters
(format "this is a %.2f" 17.12999)
(format "this %s is a %s" "string" "normal one.")
(format "this is a %d. But try to make it a 17.5" 17)
; prefer printf over (print (format ...))
(printf "this is a %.2f\n" 17.12345)

; string may be used as sequence
(map int "abc")

; string functions
(str/join "---" [11 "12" :13])

; -------------------------------------------------------------------
;         exploring in the repl
; -------------------------------------------------------------------

(use '[clojure.repl])
(dir str)
(doc str/split-lines)
(dir clojure.repl)

; -------------------------------------------------------------------
;         more comments 
; -------------------------------------------------------------------

; The reader ignores every form that is prefixed by #_
; Even forms in between.
(+ 40 #_(+ 100) 2)

; This is a very special comment...
; The forms inside are not evaluated
; when you load the file.
; But you may evaluate all the s-expressions
; for experimenting and documentation purposes 
; in the REPL. Those comments are called 
; "rich comments", since Rich Hickey uses them, too.
; You may show your thoughts in an executable way 
; in those comments.

(comment ; rich comment

  (+ 40 2)

  ; evaluate in order to define this function 
  (defn fun [n]
    (str/join (repeat n " 🤪 ")))

  (fun 3)

  ; in some editors, nil at the end
  ; is useful, so that the 
  ; final paranthesis does not jump up  
  ; by an autoformatter to (fun 3))

  nil)

; -------------------------------------------------------------------
;         lists, vectors and sets 
; -------------------------------------------------------------------

; __Collection_operations__ take a certain type of collection and return the same type.
; So if you do an update with a map, it returns a new map. 
; If you do update with a vector, it returns a new vector. 
; These operations are update, dissoc, assoc, etc.
(update [1 2 3] 1 dec)
(update [1 2 3] 1 - 5) ; uses function and params to transfrom one value
; get the original value (ov) with idx = 1, 
; call function with all the params and ov as FIRST parameter
; e.g. (- 2 5)
(assoc [1 2 3] 1 5) ; uses keys/value(s) to transform 1..n values
(assoc [1 2 3] 1 5 3 17) ; more... even adding at the end. But not more

; __Sequence_operations__ will coerce their argument to a sequence and return a particular type.
; These are your map, filter, take, drop, group-by, frequencies, etc. 
; You’ll notice that all sequence operations take the sequence last.
(cons 1 [3 4]) ; sequence operation: returns a sequence
(conj [3 4] 1) ; collection operation: returns the original type
; more on sequences later

; lists - add at head (conj), take at head (first), remove head (rest)
; Basically, that's a stack.
(conj '(1 2 3) 5) ;; add at head
(conj [1 2 3] 5) ;; vector conjoins at end
(first '(1 2 3))
(rest '(1 2 3))
(count '(1 2 3))
(nth '(1 2 3) 2);; but is O(n)! 
(drop 4 '(1 2 3 4 5 6 7 8))
(remove odd? '(1 2 3 4 5 6 7 8))
(remove #(and (< % 4) (odd? %))
        '(1 2 3 4 5 6 7 8))
(filter #(= 2 (count %)) '("O" "TT" "OOOO"))
(keep identity [false 1 nil])
(keep seq [[]
           {:a 3 :b 13}
           nil
           '()
           [1 2 3]])

; vectors
(conj [1 2 3 4] 1)
(first [1 2 3 4])
(rest [1 2 3 4])
(get [1 2 3 4] 1) ; random access
([1 2 3 4] 1) ; use vector as function (just remove the 'get')

; mixed
(flatten [1 2 3 [4 5 '(6 7) 8] 9 10 [11]])

; and sets
#{"a" "b"}
(def mixed (conj #{"a" "b"} 1 2 3)) ;; => #{1 "a" 3 2 "b"}
(disj mixed 1)

(def one #{"hulk" "catwoman" "frodo"})
(def two #{"hulk" "catwoman" "frida"})
(set/difference one two)
(set/difference two one)
(set/union one two)

; KATA: try to add another 3
#{1 2 3}
(conj #{1 2 3} 3)
;#{1 2 3 3} ; or try this

; queue 
; details https://github.com/danielmiladinov/joy-of-clojure/blob/master/src/joy-of-clojure/chapter5/how_to_use_persistent_queues.clj
(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

; use conj, into, peek, and pop.
(into (clojure.lang.PersistentQueue/EMPTY) [4 6 8 9 12 15])
(def q (into (clojure.lang.PersistentQueue/EMPTY) [4 6 8 9 12 15]))

(println q)
(conj q 99)
(peek q) ; same as first (peek and pop are working together 'at one side' - for different collections)
(pop q) ; same as rest, but delivers queue, not sequence
(first q)
(rest q)

(empty [1 2])
(empty '(1 2))
(empty #{1 2})
(empty {:s 1 :z 2})
(empty q)

; -------------------------------------------------------------------
;         KATA 1: count the number of increasing pairs  
; -------------------------------------------------------------------
(defn count-increasing
  "Count those pairs, where the second value is higher than the first.
   Example: [1, 2, 3 1 1 1, 2 1 5, 9 8 7 6]
   The commas mark the pairs to count."
  [coll]
  (count coll)) ; replace the obviously wrong (count coll)

; (assert (= 5 (count-increasing [1, 2, 3 1 1 1, 2 1, 5, 9 8 7 6])))

(comment
; do experimentation here 
  (->> [1 2 3 1 1 1 2 1 5 9 8 7 6])
         ; partition
         ; filter
         ;...)


; one possible solution...
; wait for transducers, to get it without intermediate 
; lazy sequence
  #_(defn count-increasing [coll]
      (->> coll
           (partition 2 1)
           (filter #(< (first %) (second %)))
           count))
  nil)


; -------------------------------------------------------------------
;         keywords and maps 
; -------------------------------------------------------------------

; a keyword is a symbol that starts with : 
; It is not resolved and is useful e.g. in maps
:not-a-string
:column
(def cursor {:column 14 :row 19})
(:row cursor)
(cursor :row)
(keyword "row")
(name :row)

; namespaced keyword - if you want to use it somewhere else,
; and the namespace is required as "my-ns" then refer to it as :my-ns/column
; in my-ns, the may be referred to as ::column
(assert (=
         ::column
         :kata-0100-basics-in-15-min.core/column))

(def cursor-with-ns-keys {::column 14 ::row 19})

(assert ;; usage in another namespace, keyword as unique value
  (= 14
     (:kata-0100-basics-in-15-min.core/column cursor-with-ns-keys)))

(assert
  (= 14
     (::column cursor-with-ns-keys)))

; maps
; with keywords as keys

{:width 15 :heigth 25}
(hash-map :width 15 :heigth 12) ; identical

(def example-map {:width 15 :heigth 25})
; a keyword may be used as a function
(:heigth example-map)
; and a map, too
(example-map :heigth)

(let [new-map (assoc example-map :thickness 10) ;; assoc(iate) a key/value to the map, name the new map u
      k :thickness]
  (println "exa-map =" example-map)
  (println "new-map =" new-map)
  [(k new-map)   ; you will find the key in new-map 
   (k example-map)]) ; but not in exa-map!

(merge {:a 1 :b 0 :c 99} {:a 22} {:x 33 :y 44 :c 55})

(merge-with +
            {:a 1  :b 2}
            {:a 9  :b 98 :c 0})

; dissoc(iate) a key
(dissoc {:width 15 :heigth 25} :width) ;; => {:heigth 25}

; a map with integers as keys.
{12 "Benno"
 21 "Paul"
 37 "Leo"}

; You may also use strings. Or anything else.
{"B" "Benno"
 "P" "Paul"
 "L" "Leo"}

; -------------------------------------------------------------------
;         KATA 2: create a data structure, that describes
;                 the state of a card game with 2 players, 
;                 a card-deck and the full set of all cards
;                 for initialization.
; -------------------------------------------------------------------

{:players {:next-turn :player-1
           :player-1 {:name "Armin"
                      :points 0
                      :at-hand #{:♦️7 :♥️D}}
           :player-2 {:name "Benno"
                      :points 0
                      :at-hand #{:♣K :♣9}}}
 :deck [:♦️8 :♦️9 :♦️10 :♦️B :♦️D :♦️K :♦️A
        :♥️7 :♥️8 :♥️9 :♥️10 :♥️B  :♥️K :♥️A
        :♠7 :♠8 :♠9 :♠10 :♠B :♠D :♠K :♠A
        :♣7 :♣8  :♣10 :♣B :♣D  :♣A]
 :all-cards (for [color ["♦️" "♥️" "♠" "♣"]
                  number [7 8 9 10 "B" "D" "K" "A"]]
              (keyword (str color number)))}

; -------------------------------------------------------------------
;        ranges, sequences and laziness 
; -------------------------------------------------------------------

; Sequence operations will coerce their argument to a sequence and return a particular type. 
; These are map, filter, take, drop, group-by, frequencies, etc.
; You’ll notice that all sequence operations take the sequence last.

(range 10) ; create a sequence of numbers from 0 to 9
(type (range 10))

; a sequence is an abstraction, that many collections implement
(seq? '(1 2 3))
(seq? (range 10))
(seq? [1 2 3]) ;; this is _not_ a sequence. But it can easily be made one.

; to create a sequence out of other collections, use seq
(seq [1 2 3])
(type (seq [1 2 3]))

; first, rest, cons and concat work on sequences.
; AND: Those Sequence lib functions transform other data types to sequences,
; if they are "seqable?".
(seq? [1 2 3])
(seqable? [1 2 3])
(first [1 2 3])
(rest [1 2 3])
(cons 0 [1 2 3]) ; cons(truct) a new sequence
(concat '(7) [1 2 3] {:k1 "value" :k2 "v2"} #{4 5 6})
(lazy-cat '(7) [1 2 3] {:k1 "value" :k2 "v2"} #{4 5 6})
; difference comes later

(seq []) ; this is the idiomatic way of testing, if a collection is empty

(range) ; this is an infinite range. Lazy!

(take 5 (range))

; a square function that signals it's parameter, when called
(defn mysqr [n]
  (println (str " " n "^2 "))
  (* n n))

; create a lazy sequence of squares recursively, starting at n
; see documentation page: https://clojuredocs.org/clojure.core/lazy-seq
(defn squares
  [n] (lazy-seq (cons (mysqr n) (squares (inc n)))))

(def s (squares 10))

; still "in lazy mode"...
(realized? s)

; they are realized one by one during "taking"
(def res (take 3 s))
(realized? s)
(type res)
(def real (doall res))
(type real)
(println "result: " real)
(def res (take 5 s))
(println "result: " res)

(realized? s)

; Seq in, Seq out
; Shorter seq from a longer seq: distinct filter remove for keep keep-indexed
(distinct '(3 1 1 1 2))
(filter #(> % 1) '(3 1 1 1 2)) ; the predicate decides about the value
(keep  #(> % 1) '(3 1 1 1 2)) ; the return value is used...
(keep-indexed (fn [idx data]
                (if (odd? idx)
                  {:unchanged data :idx idx}
                  (str "stringed-not-odd...idx=" idx ",data=" data)))
              '(19 \d 10 20))

; Longer seq from a shorter seq: cons concat lazy-cat mapcat cycle interleave interpose
(cons 1 '(4 5 6))
(concat '(1 2) [8 9] #{"e" :kw})
(lazy-cat '(1 2) [8 9] #{"e" :kw})

; compare lazy-cat and concat https://clojuredocs.org/clojure.core/lazy-cat
(time (first (concat (sort > (range 10)) (sort > (range 1e6)))))
(time (first (lazy-cat (sort > (range 10)) (sort > (range 1e6)))))

(take 5 (cycle [3 5]))
(interleave [1 2 3 4] [9 9 9 9 9 9 9 9 9])
(interleave [1 2 3] '(:a :b :c :d :e) {:x 11 :y 12 :z 13})
(interpose 6 [1 2 3])
(interpose "separator" [1 2 3])

; Seq with head-items missing: rest next fnext nnext drop drop-while nthnext for
(rest '(1 2 3))
(next '(1 2 3))
(rest '())
(next '())
(seq '())
(seq nil)
(fnext '(1 2 3)) ; (first (next ...))
(fnext '())
(fnext nil)
(nnext '(1 2 3 4 5)); (next (next ...))
(drop 4 '(1 2 3 4 5 6))
(drop-while neg? [-1 -2 -5 (* 2 -4) -4 3 4 5 -1 -2 3 4 5 -1 2 3])
(nthnext [1 2 3 4 5 6 7] 4); identical to drop?
(nthnext (range 10) 5)
(drop 5 (range 10))
(nthnext [] 3)
(drop 3 [])


; NOT a for loop - but list creation!
(for [x (range 8) y (range 5)
      :while (< y x)
      :let [z (* x y)]]
  [x y (str "x*y=" z)])
(for [x (range 20) :when (not= x 10)] x)
(for [x (range 20) :while (not= x 10)] x)

; Seq with tail-items missing: take take-nth take-while butlast drop-last for
(take 5 (range))
(take-nth 3 [1 2 3 4 5 6 7 8 9 0])
(take-while neg? [-1 -2 -3 4 5 6 -7 -8 -9 0])
(butlast [-1 -2  0])
(drop-last [-1 -2  0])

; Rearrangment of a seq: flatten reverse sort sort-by shuffle
(flatten [1 [2 3 4 '(5 6) 7 8] 9 [0]])
(reverse [1 2 3 4 5 6 7 8 9 0])
(shuffle [1 2 3 4 5 6 7 8 9 0])
(sort [6 3 8 1 4 5 9 7 0 2])
(sort-by #(- %) [6 3 8 1 4 5 9 7 0 2])
(sort-by - [6 3 8 1 4 5 9 7 0 2])
(sort-by first [[6 3] [8 1] [4 5] [9 7] [0 2]])
(sort-by (comp - second) [[6 3] [8 1] [4 5] [9 7] [0 2]])
(comp - second); is a function that comp(oses) of some functions... (- (second x))

; Create nested seqs: split-at split-with partition partition-all partition-by
(split-at 3 [0 1 2 3 4 5 6 7 8])
(split-with odd? [1 3 5 4 5 6 7 8])
(partition 3 [1 3 5 4 5 6 7 8])
(partition 3 1 [1 2 3 4 5 6 7 8 9 10 11 12])
(partition-all 3 [1 3 5 4 5 6 7 8])
(partition-by odd? [1 3 5 4 4 4 4 5 7 9 8 6 4 3 1 5 0 1 4])

; Process each item of a seq to create a new seq: map pmap mapcat for replace reductions map-indexed seque
(map square [1 2 3 4])

(set! *print-length* 20)
(time (doall (map square (range 1000))))
(time (doall (pmap square (range 1000)))) ; only useful if f is heavy!

(defn really-heavy-square [x]
  (let [square (* x x x x)]
    (loop [counter square]
      (if (zero? counter)
        square
        (recur (dec counter))))))

; you see a difference of factor 4 
(time (doall (pmap really-heavy-square (range 50 60))))
(time (doall (map really-heavy-square (range 50 60))))

(mapcat  reverse [[1 2 3] [4 5 6]])
(map  reverse [[1 2 3] [4 5 6]])

(replace {1 99 4 77} '(1 2 3 4 5))

(reduce + 100 '(1 2 3))
(reduce (fn [acc val]
          (println (str "acc: " acc ", val: " val))
          (+ acc val))
        100
        '(1 2 3))

(reductions + 100 '(1 2 3))

(set! *print-length* 20)
; be careful in repl... this prints forever...
; or you set the *print-length*
(prn (iterate inc 10))
(set! *print-length* nil)

(def add3 (partial + 3)) ;; like currying in groovy
(def sqr-and-add-3 (comp add3 square)) ;; applied right to left!
(sqr-and-add-3 2)

(take 30 (iterate (partial + 3) 10))
(take 30 (iterate add3 10))
(take 3 (iterate sqr-and-add-3 10))

(map-indexed (fn [idx val] [(str "idx=" idx) val]) [:the-one :two-too :three-me])
;;(seque) we leave that out until threading

; Using a seq
; Extract a specific-numbered item from a seq: first ffirst nfirst second nth when-first last rand-nth
(first [1 2 3])
(ffirst [[4 5] [6 7]])
(nfirst [[4 5 6 7] [8 9]]) ; (next (first ...))
(second [1 2 3])
(nth [1 2 3 4 5 6 7 8] 5)
(when-first [a '(4 5 6)]
  (* 2 a))
(when-first [a '()]
  (* 2 a))
#_(when-first [a '(nil 5 6)]
    (* 2 a)) ; this gives error...
(last [1 2 3])
(rand-nth [1 2 3 4 5 6 7 8])

; Construct a collection from a seq: zipmap into reduce set vec into-array to-array-2d frequencies group-by
(zipmap [:a :b :c] [1 2 3 4 5 6])
(into (sorted-set) [3 3 3 4 0  0 0  5 -1 8 9])
(reduce + 100 '(1 2 3))
(reduce + 100 '(2 3))
(reduce + '(2 3))
(reduce + '())
(reduce * 100 '(2 3))
(reduce * '(2 3))
(reduce * '())
(reduce conj [1 2 3] [:more :of :this])
(reduce (fn [acc val](conj acc (name val)))
        [1 2 3]
        [:more :of :this])

(defn factorial
  [x]
  (reduce * (range 1M (inc x))))

(factorial 10)

; suppress printing in repl
(set! *print-length* 10)
(time (doall (map factorial (range 2000))))

; memoize
(def mem-fac (memoize factorial))
(time (doall (map mem-fac (range 2000))))
(time (doall (map mem-fac (range 2000))))

(set '(1 2 2 2 2 3))
(vec '(1 2 2 2 2 3))
(into-array '(1 2 2 2 2 3)) ; java array

; Pass items of a seq as arguments to a function: apply
(max 4 -1 99 -99)
(apply max [4 -1 99 -99])

; Compute a boolean from a seq: not-empty some reduce seq? every? not-every? not-any? empty?
(not-empty [3 4])
(not-empty [])
(not-empty '())
(some pos? '(-3 -2))
(some pos? '(-3 -2 1))
(every? pos? '(-3 -2 1))
(every? pos? '(3 2 1))
(not-every? pos? '(-3 -2 1))
(not-every? pos? '(3 2 1))
(not-any? pos? '(-3 -2 -1))
(empty? '(3))
(empty? '())
(empty? nil)
(seq? '())
(seq? [])
(seq? (hash-map))

; Force evaluation of lazy seqs: doseq dorun doall
(doseq [x [1 2 3]
        y [4 5 6]]
  (prn (* x y)))

;; since lazy AND chunked, will not be printed completely!
(for [x [1 2 3]
      y (range 100)]
  (do
    (prn x y)
    (* x y)))

;; doall changes that...
(doall (for [x [1 2 3]
             y (range 100)]
         (do
           (prn x y)
           (* x y))))

; lazy1 - no println at all, because lazy sequence is constructed and the mapping is done during evaluation
(def lazy1 (map println [3 4 5 6]))
; Check if lazy seqs have been forcibly evaluated: realized?
(realized? lazy1)
; dorun --> only side-effects, no return value
(dorun lazy1)
(realized? lazy1) ; now, it's realized. But it's also realized with (take 1 lazy1)!

; doall --> sequence with results
; lazy2 - realization of ALL is enforced
(def lazy2 (doall (map println [3 4 5]))) ; return realized sequence
(println lazy2)
(realized? lazy2)

(dorun 1 (map #(println "hi" %) ["mum" "dad" "sister"]))
(def lazy3 (dorun 2 (map println (range 100)))) ; returns nil
(println lazy3)

;; run! and mapv also kills laziness

; Creating a seq
; Lazy seq from collection: seq vals keys rseq subseq rsubseq
(seq [3 4 5])
(vals {:k1 "value" :k2 "v2"})
(keys {:k1 "value" :k2 "v2"})
(rseq [3 4 5])
(subseq (apply sorted-set [0 1 2 3 4 5]) > 2)
(rsubseq (apply sorted-set [0 20 3 4 5 9 99 23 23 10 23]) > 2 < 11)

; Lazy seq from producer function: lazy-seq repeatedly iterate
(defn increasing-numbers [n]
  (lazy-seq (cons n (increasing-numbers (inc n)))))

(take 4 (increasing-numbers 0))

(take 5 (repeatedly #(rand-int 11)))

(take 10 (iterate (partial + 2) -10))

; Lazy seq from constant: repeat range

;; compare with repeat, which
;; only calls the 'rand-int' function once,
;; repeating the value five times.
(repeat 5 (rand-int 100))
; e.g. (94 94 94 94 94)

(range 4 16 2)

; Lazy seq from other objects: line-seq resultset-seq re-seq tree-seq file-seq xml-seq iterator-seq enumeration-seq
(with-open [rdr (clojure.java.io/reader "/etc/passwd")]
  (count (line-seq rdr)))

(set! *print-length* nil)
(-> (slurp "/etc/passwd")
    (str/split  #"\n")
    count)

(import '(java.io BufferedReader StringReader))
(line-seq (BufferedReader. (StringReader. "1\n2\n\n3")))

(re-seq #"[A-Z][a-z]+|[0-9]+" "ManishKumar12332")
(tree-seq
  (fn [elem] (not (number? elem))) ;; is the element a branch?
  seq ;; get the children
  '((1 2 (3)) (4)))

(def f (clojure.java.io/file "/etc"))
(def fs (file-seq f))
;(clojure.pprint/pprint (take 10 fs))

; not on classpath [org.clojure/data.xml "0.0.8"]
#_(use '[clojure.data.xml :only [parse-str]])
#_(let [xml-text "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
                      <foo key=\"val\">1<bar>2</bar>3</foo>"]
    input (java.io.ByteArrayInputStream. (.getBytes xml-text))
    root (parse input)
    (xml-seq root))

;(iterator-seq) ; ignore for the moment
;(enumeration-seq) ; ignore for the moment


; -------------------------------------------------------------------
;         state 
; -------------------------------------------------------------------
; Rule 1: try hard to avoid state...
; Rule 2: If you cant avoid it, make a central map as atom.
; Rule 3: STM or Agents are rarely needed... 
; see: https://clojure.wladyka.eu/posts/share-state/ 

; a little helper first:
(defn say-hello [name] (str "Hello " name))
(def say-hello-with-defaults (fnil say-hello "World"))
(say-hello-with-defaults "Sir")
(say-hello-with-defaults nil)

(def my-state (atom {})) ; could be anything... long, string, ...
(type my-state)
(println @my-state)


(reset! my-state {:now-totally-resetted :yes})

; if you want to change an atom, use swap or reset
(swap! my-state update :x (fnil inc 0))
(swap! my-state update :x inc)
(swap! my-state update :x (constantly 450))
(swap! my-state assoc :x 1000 :y 500 :z 70)
(swap! my-state assoc :submap {:sx 11000 :sy 1500 :sz 170})
(swap! my-state update-in [:submap :sy] + 100)
(swap! my-state merge {:x :new :new-kew 123})

(reset! my-state {:now-totally-resetted :yes})

; Those are atomic changes. Threadsave. Without thread blocking.
; Instead, swap! uses javas AtomicReference compareAndSet.
; This means, that the function may be called several times.
; Therefore, it has to be pure - means, free from side effects.

; -------------------------------------------------------------------
;         higher order functions 
; -------------------------------------------------------------------

; HINT: in clojure, use higher order functions like
; map, filter and reduce, iterate, juxt, comp, repeatedly, partial.
; try to avoid looping and branching


; a function returning functions while closing over parameters
(defn get-the-right-fun
  "Depending on x < 5 (4 3 1 ...), returns a function with 2 or else with 3 parameters.
  And even more interessting, x is 'closed over'.
  It keeps it's value in the subsequent calls to the
  newly created functions."
  [x]
  (if (< x 5)
    (fn [y z] (* x y z))
    (fn [y z some-more] (+ x y z some-more))))

(def f2 (get-the-right-fun 2))
(f2 1 2)
(def f3 (get-the-right-fun 7))
(f3 1 2 3)

; HOFs (higher order functions) either use functions as parameters or return functions as results - or both.
; https://christophermaier.name/2011/07/07/writing-elegant-clojure-code-using-higher-order-functions/

; they can replace many loops and branching...
(map inc [1 2 3])
(filter odd? [1 2 3 4 5])
(reduce + 1000 [10 15 100])
(reduce conj [0]  [6 6 6])
((fnil seq ["17"]) nil)
(mapv inc '(17))
(into [] #{1 2})
((juxt inc dec #(* % % %)) 2)

(def times-5 (partial * 5))
(times-5 20)

(def plus-20-times-5 (comp (partial * 5)
                           (partial + 20)))
(plus-20-times-5 2) ; (* 5 (+ 20 x)

((complement seq) []) ; may be the same as (comp (not (seq)))

(take 10 (iterate inc 5))

; -------------------------------------------------------------------
;         imperative, objects and functional 
; -------------------------------------------------------------------
; https://docs.microsoft.com/en-us/dotnet/standard/linq/functional-vs-imperative-programming
;
; imperative, algorithmic, procedural
;  state1 = something
;  state2 = something-else
;  loop-until-state2-and-state1-satisfies-condition
;   if state2-satisfies-predicate 
;   then
;      side-effect1-mutating-state1
;   else 
;      side-effekt2-mutating-state2 
;
; --> mutating state all over the place (side effects)
; --> reasoning about code need understanding of about "everything"
; --> micro-algorithmic all over the place (loops, if, ...)
; --> state in form of values are changing "in place" 
; --> multithreading is hard - because mutating state needs to be synchronized, all sorts of race conditions may appear, if not.
; 
; object-oriented:
;  object1 encapsulates state
;  object2 encapsulates state
;  many dependencies between objects.
;  object1.mutateSomething(object2.getState)
;
; --> mutating (encapsulated, but anyway) state all over the place.
; --> reasoning about code need understanding of about "everything"
; --> multithreading for objects, that are not meant to be used multithreaded is error prone
;
; functional: https://gist.github.com/Maikon/97322cd85c085a62abbb304d081d16d1
;             dominant pattern: new-value = f-without-side-effekt(old-value1, old-value2)
;             mutating state only "at the border of the system"
; very good illustration with code: https://www.youtube.com/watch?v=vK1DazRK_a0
;
; from the point of view of reasoning, functional results in code, that is understandable locally isolated.
; from the point of view of tests, is is testable without (or with very simple) mocks and stubs.
; from the point of view of performance, it is easy to parallelize pure functions (e.g. pmap)
; from the point of view of using and mutating state concurrently, atomic and persistent data structures make it less error prone.

; WHAT TO DO in clojure?
; 1. Try hard to avoid mutating state - instead, you write pure functions without side-effect.
;    Maybe, those functions return a data description of the side-effects they want to do.
;    The real effect is executed somewhere else.
; 2. You create new values instead of changing them in place. 
;    Persistent data structures do that for you.
; 3. You avoid imperative style with long seqences of side effects and loops. 
;    Instead, use higher oder functions like map, reduce, filter...
;    Write small functions, that may easily testet.
; 4. Center your application around data.
;    You don't think in "Objects with data and methods".
;    Instead, you design your app aroound your data structures.
;    incomming-data --> processing --> outgoing-data

(do ;; 'do' one side effect after the other... 
  (println "abc") ;; Not interested in result. Only in side effect.
  (cons :a [:b :c]))

;;;; this is imperative (and shows java interop)

(defn simple-date [d]
  (.format
   (java.text.SimpleDateFormat. "dd.MM.yyyy") d))

(defn time-imperative []
  (println (System/nanoTime))
  (println (simple-date (java.util.Date.))))

(time-imperative)

;;;; this is functional

(defn time-functional [nt d]
  {:nano-time nt
   :date (simple-date d)})

(defn show-time [time-map]
  (println (:nano-time time-map))
  (println (:date time-map))
  time-map)


(defn show-now [] (-> (time-functional
                       (System/nanoTime)
                       (java.util.Date.))
                      show-time))
(show-now)

;; you may test that easily with or without side effects
(-> (time-functional
     111111111
     #inst "2012-05-30")
    show-time)

(assert (= (time-functional 111111111 #inst "2012-05-30")
           {:nano-time 111111111, :date "30.05.2012"}))

; -------------------------------------------------------------------
;         branching 
; -------------------------------------------------------------------

; sometimes, (or ...) is used to provide a value
(or false "default-value")
(or nil "default-value")
(or "value-available" "default-value")

; (if ...) has exactly one form for if branch and for else branch
(if true
  "this"
  "that")

(if true
  (do
    (println "true")
    [:this :is :the :result])
  [:this :is :never :reached])


(defn sum-even-numbers [nums]
  (if-let [nums (seq (filter even? nums))]
    (reduce + nums)
    "No even numbers found."))

(sum-even-numbers [1 3 5 7])
(sum-even-numbers [1 2 3 5 7 8])

(when true
  (println "true")
  [:result :because :of :implicit :do :and :no :else])

(let [n 10]
  (cond
    (< n 5) "n is smaller than 5"
    (<= 10 n 25) "n is between 10 and 25 (inclusive each)"
    :else "this is the default result"))

(cond
  (fn? sum-even-numbers) "is fn")

(cond
  (number? sum-even-numbers) "not this branch but nil")

(cond
  (number? sum-even-numbers) "yes, it's a number"
  :else "this is the default branch, typically called :else")

(let [n 4]
  (condp > n
    ;; (pred test-expr expr), e.g. (> 5 n )
    5 "5 is bigger than n -- unintuitive precedence" ; (> 5 4=n )
    25 "n is 5 or bigger but smaller than 25"
    "this is the default result >= 25"))

; start with 42
; if the next form is trueish, use the last result and calc the form with with the result as first parameter after operation
; (* 42 2)
; (cons 84 [1 2 3])
(cond-> 42
  true (* 2)
  nil (+ 16)
  15  (cons [1 2 3])
  false (str/join))

;; styleguide recomendations, see also https://clojuredocs.org/clojure.core/case
(let [x 30]
  ;; "good"
  (time (cond
          (= x 10) :ten
          (= x 20) :twenty
          (= x 30) :thirty
          :else :dunno))     ;;=> "Elapsed time: 0.0666 msecs"

  ;; "better"
  (time (condp = x
          10 :ten
          20 :twenty
          30 :thirty
          :dunno))           ;;=> "Elapsed time: 0.4197 msecs"

  ;; best in performance if else statement is executed (not shown)
  ;; best in readability
  (time (case x
          10 :ten
          20 :twenty
          30 :thirty
          :dunno))           ;;=> "Elapsed time: 0.0032 msecs"

  ;; best in performance if known branches are executed
  ;; worst in readability
  #_(time (if (= x 10))
          :ten
          (if (= x 20)
            :twenty
            (if (= x 30)
              :thirty
              :dunno))))   ;;=> "Elapsed time: 0.0021 msecs" 

; -------------------------------------------------------------------
;         loops and recursion 
; -------------------------------------------------------------------

; first we look for the 'functional loops'

; this is not an imperative loop - but a creation of a sequence!
(for [x [0 1 2 3 4 5]
      :let [y (* x 3)]
      :when (even? y)]
  y)

; recursion as loop - but: stackframes are becoming a problem...
(defn recursive-fun-fail [coll]
  (println coll)
  (if (seq coll)
    (recursive-fun-fail (rest coll))
    "consumed everything"))

(recursive-fun-fail (range 100))
;(recursive-fun-fail (range 100000))

; clojure has 'recur' to recurse without stack overflow
(defn recursive-fun [coll]
  ;(println coll)
  (if (seq coll)
    (recur (rest coll))
    "consumed everything"))
(recursive-fun (range 100000))

; recur may be used for loops with rebinding of parameters as with recursive function calls
; a very simple loop
(loop [data [3 5 7]] ; start with data initialized as vector
  (println data)
  (when (seq data) ; when there is still data in vector
    (recur (rest data)))) ; call a recur with data reduced by one element

;with loop, we can create a very idiomatic tail-recursion in clojure
(loop [input (range 10) ;; initialize first loop parameter
       r-result '()] ;; initialze second parameter, the recursion result.
  (if (seq input)
    (let [times-3 (* 3 (first input))]
      (recur
       (rest input) ; loop with this as first loop parameter
       (cons times-3 r-result))) ; loop with this as second loop parameter
    r-result))

; but, this is very easily achived by map, iterate or reduce or filter...
; so loop/recur may be better replaced by map or reduce or iterate
(map #(* 3 %) (range 9 -1 -1))

; Finally, there is trampoline.
; Whenever it get's a function back, it calls it again.
; When it gets another value, it returns it.
(defn countdown [start]
  (println start)
  (if (pos? start)
    (fn [] (countdown (dec start))) ; this returns a function - instead of calling it 
    "countdown finished"))

(countdown 20) ; not that useful...
(trampoline (countdown 20)) ; but this avoids stack frames
; trampoline is useful in mutual, recursive function calls
; because they cannot be made with recur.
; BUT: slower...

; there are some more imperative loops, you have already seen
; e.g. doseq


; -------------------------------------------------------------------
;         asserts and tests 
; -------------------------------------------------------------------

; some global variables, that enable working in REPL

(comment

  (use '[clojure.repl])
  (doc use)
  (source use)
  (ns-publics 'clojure.repl)
  (dir clojure.repl)
  "result-one"
  (* 50 4)
  (+ 1 3)
  (str *1 " - " *2 " - " *3) ; *1 = last, *2 = before and before before
  (/ 1 0)
  (pst))


;meta info for the compiler, the runtime and humans
(def ^{:dynamic true :author :bel} a-var "value")
;; just to remember... symbol -> variable -> value
(assert (= (deref (var a-var)) a-var))

(println a-var)
(meta (var a-var))
(meta #'a-var) ; same...
(meta a-var) ; DOES NOT WORK
(set! *print-meta* true)
(pr (var a-var))
(set! *print-meta* false)
(println (var a-var))

;; this is a shorthand notation for
;; {:tag java.lang.String, :private true}
;; type hint for compiler
(def ^:private ^String priv-str "Hello, world!")
(meta #'priv-str)

;; sometimes, rebinding is useful for testing in REPL
(def ^:dynamic xxx 1) ; try remove ^:dynamic
(def ^:dynamic yyy 1)
(+ xxx yyy)

;; Within the scope of the binding, x = 2 and y = 3
(binding [xxx 2
          yyy 3]
  (+ xxx yyy))

;; But once you leave the binding's scope, x and y maintain their original
;; bindings:
(+ xxx yyy)

; plain assert
(assert (= 2 2))

;; lets try a macro - it get's its parameters unevaluated...
(defmacro a
  "an assert that returns it's value"
  [expected check-and-return]
  `(do
     (assert (= ~expected ~check-and-return))
     ~check-and-return))

(* 100 (a 2 (- 4 2))) ; you may use an assert in the middle of a calculation

(macroexpand '(a 2 (- 4 2)))

(require '[clojure.test :as t])

(t/deftest bels-fun-test
  (t/testing "simple things..."
    (t/is (= 2 2))
    (t/is (= 1 1))
    (t/is (thrown? ArithmeticException (/ 1 0))))
  (t/testing "a formula and a data set"
    (t/are [x y] (= x y)
      2 (+ 1 1)
      4 (* 2 2)))
  (t/is (= [1 2 3] (range 1 4))))

(comment

  (t/run-tests)

  ; this is only used to mark the tests a little bit more colored (green / red)
  ; this is much better with metosin test runner...
  (defn format-results [test-results]
    (if (or (pos? (:error test-results))
            (pos? (:fail test-results)))
      (do
        (println)
        (clojure.pprint/pprint :=======-FAILED-some-tests-FAILED-=======)
        (println)
        (println test-results))
      (do
        (println)
        (clojure.pprint/pprint (str "*** passed tests *** "
                                    "(assertions: "
                                    (:pass test-results) ")"))
        #_(println)
        #_(prn test-results))))

  (format-results (t/run-tests)))
  ;(format-results {:test 1, :pass 5, :fail 0, :error 0, :type :summary}))

(comment

  ; pre and post condition
  (defn constrained-fn [f & args]
    {:pre  [(pos? (first args))
            (> (second args) 1)]
     :post [(= % (* 2 (first args)))]}
    (apply f args))

  (constrained-fn max 1 2 0)
  (constrained-fn max -1 2 0)
  (constrained-fn max 1 2 3)
  (constrained-fn + 1 2 -1 0)
  (constrained-fn * 1 2 1 -1 -1)
  (constrained-fn / 10 5 1/10)

  nil)


; -------------------------------------------------------------------
;         files 
; -------------------------------------------------------------------
(line-seq (BufferedReader. (StringReader. (slurp "/etc/passwd"))))
(str/split-lines (slurp "/etc/passwd"))
(spit "/Users/benno/something.txt" "a text\nand a new line")
(str/split-lines (slurp "/Users/benno/something.txt"))
; works for urls, too
(count (slurp "https://clojuredocs.org"))

; -------------------------------------------------------------------
;         regex 
; -------------------------------------------------------------------
; https://ericnormand.me/mini-guide/regexes-in-clojure
(type #"regex") ; this creates a regex pattern
(re-matches #"abc" "zzzabcxxx") ; no complete match...
(re-matches #".*a.c.*" "zzzabcxxx") ; complete match...

(re-seq #"i[sp]+" "mississippi") ; a sequence of matches

; if you want positions of matches
(defn re-pos [re s]
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(re-pos #"\w+" "The quick brown fox")


; see e.g. re-find re-matcher re-groups

; -------------------------------------------------------------------
;         date time 
; -------------------------------------------------------------------
; tutorial, see: https://github.com/dm3/clojure.java-time
; use that one in the project.clj: [clojure.java-time "0.3.3"]
;

; -------------------------------------------------------------------
;         exceptions 
; -------------------------------------------------------------------
(try
  (/ 1 0)
  (catch Exception e (str "caught exception: " (.getMessage e))))

(try
  (if true
    (throw (ex-info "my ex" {:data 123}))
    ;(assert (= 1 3))
    "this will happen - maybe later")
  (catch Exception e (str "caught exception: " (.getMessage e)))
  (catch AssertionError e (str "YES! " e))
  (finally (println "finally...")))


; -------------------------------------------------------------------
;         destructuring 
; -------------------------------------------------------------------
; https://gist.github.com/john2x/e1dca953548bfdfb9844

(def my-vector [:a :b :c :d])
(def my-nested-vector [:a :b :c :d [:x :y :z]])

(let [[an-a the-b cee dee] my-vector]
  (println an-a the-b cee dee))

(let [[a _ _ d [x y z]] my-nested-vector]
  (println a d x y z))


(def my-hashmap {:a "A" :b "B" :c "C" :d "D"})
(def my-nested-hashmap {:a "A" :b "B" :c "C" :d "D" :q {:x "X" :y "Y" :z "Z"}})

(let [{a :a, b :b, {x :x, y :y} :q} my-nested-hashmap]
  (println a b x y))

(let [{:keys [a d nix]} my-hashmap]
  (println a d nix))

(let [{a :a, not-found :not-found, b :b, :or {not-found ":-/"}} my-hashmap]
  (println a not-found b))

(defn mult-those-three-nums [[_ _ _ a b c :as all]] ; 0 1 2 a=3 b=4 c=5 6 7 8
  (println "all values: " all)
  (println "a = " a)
  (println "b = " b)
  (println "c = " c)
  (if (and a b c)
    (* a b c)
    "numbers not complete..."))

(mult-those-three-nums (range 10))
(mult-those-three-nums (range 3))
(mult-those-three-nums (range 5))
(mult-those-three-nums (range 15 30))

; https://clojure.org/news/2021/03/18/apis-serving-people-and-programs
; Keyword argument functions now also accept maps
(defn destr [& {:keys [a b] :as opts}]
  [a b opts])

(destr :a 1 :c 2)

; starting from clojure version 1.11
*clojure-version*
;(destr {:a 1 :b 2})

; -------------------------------------------------------------------
;         concurrency and state
; -------------------------------------------------------------------
; https://ericnormand.me/guide/clojure-concurrency

(do (def the-answer (future
                      (Thread/sleep 1000)
                      42))
    (println (future-done? the-answer)) ;; no, not yet...
    (println @the-answer)
    (println (future-done? the-answer))) ;; yes, now awaited...)


(do (def choose-burger (future
                         (Thread/sleep 100)
                         :hamburger))
    (deref choose-burger 500 :cheeseburger) ; try with 200. timeout with default
    #_(@choose-burger 10000 :cheeseburger)) ; last one does not work!

(let [make-burger (fn [b-type wait] (future (Thread/sleep wait) b-type))
      burger-1 (make-burger :royale-with-cheese 1000)
      burger-2 (make-burger :hamburger 50)]
  (println (deref burger-1 300 :cheeseburger))
  (println (deref burger-2 300 :cheeseburger)))

; first of all with a TOTALLY WRONG redefinition of variables. RACE CONDITION.
; DON'T USED re-def's in code. Re-Def's are only for debugging and repl.
(time (do (def number 0)
          (let [f1 (future (dotimes [_ 50000] (def number (inc number)))
                           number)
                f2 (future (dotimes [_ 25000] (def number (dec number)))
                           number)
                fin [@f1 @f2]]
            (println "finished both " fin)
            (println "final value should be: 25000, is: " number))))

;; now with an atom... The clojure way to synchronize state without thread locking...
;; For "lockfree synchronisation", see java AtomicReference compareAndSet

(time (let [number-a (atom 0)
            f1 (future (dotimes [_ 50000] (swap! number-a inc))
                       @number-a)
            f2 (future (dotimes [down 25000] (swap! number-a dec))
                       @number-a)
            fin [(str "finished-up-at: " @f1) (str "finished-down-at: " @f2)]]
        (println "finished both " fin)
        (println "final value should be: 25000, is: " @number-a)))

(do (def xa 0)
    (doall (repeatedly 500
                       (fn [] (future (def xa (inc xa))))))
    ;(Thread/sleep 300) ; you dont need that...
    (println xa));(pmap)

(do (def xa2 (atom 0))
    (doall
     (repeatedly 500
                 (fn [] (future (swap! xa2 inc)))))
    ;(Thread/sleep 10) ; really! you dont need that
    (println @xa2));(pmap)
;(seque)

; for STM and Agents, see
; https://clojure.wladyka.eu/posts/share-state/

; -------------------------------------------------------------------
;         transients (if speed really counts) 
; -------------------------------------------------------------------
; the need thread isolation. Use only, if performance is needed.

(defn vrange [n]
  (loop [i 0 v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))

; make a structure transient
; use e.g. conj! - instead of conj
(defn vrange2 [n]
  (loop [i 0 v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

(time (def v (vrange 10000000)))    ;; 80 - 200 ms
(time (def v2 (vrange2 10000000)))  ;; 43 -70 ms



; -------------------------------------------------------------------
;         java interop 
; -------------------------------------------------------------------
; https://clojure.org/reference/java_interop



; (.instanceMember instance args*)
; (.instanceMember Classname args*)
; (.-instanceField instance)
; (Classname/staticMethod args*)
; Classname/staticField

; static function call
(java.time.LocalDateTime/now)
(System/getProperty "java.vm.version")

; static field
(Math/PI)

; field access with object
(.-x (java.awt.Point. 1 2))

; call a method on an object
(.toUpperCase "fred")

(.. System getProperties (get "os.name"))
(-> (System/getProperties) (.get "os.name"))

(doto (new java.util.HashMap) (.put "a" 1) (.put "b" 2))

(bean java.awt.Color/black)


; move to time handling
(comment (require '[java-time :as java-time])

         (-> (java-time/local-date 2020 10 02)
             java-time/year
             java-time/format))

; https://nextjournal.com/schmudde/java-time
(type #inst "2022-06-05"); deprecated!
(.getTime #inst "2022-06-05") ; milliseconds, but deprecated

; -------------------------------------------------------------------
;         transducers, reducers 
; -------------------------------------------------------------------

; https://eli.thegreenplace.net/2017/reducers-transducers-and-coreasync-in-clojure/


; reducer - avoid intermediates and parallelize
(require '[clojure.core.reducers :as r])
(->> (vec (range 10000000)) ;; make it a random access collection. NOT LIST!
     (r/filter even?)
     (r/map inc)
     (r/fold +)) ;; fold instead of reduce

; wont work... get back a reducer, no seq
; (first (r/map inc [1 2 3]))

; need to create a coll...
(first
 (into [] ; use into instead of r/foldcat, if you want to determine type
       (r/map inc [1 2 3])))

; or written a bit more readable
(->> [1 2 3]
     (r/map inc)
     (r/foldcat) ; similar to into - creates a vectorish thing...
     first)      ; foldcat executes parallely!

; fast java version, can be used for more reducers work
(type (->> [1 2 3]
           (r/map inc)
           (r/foldcat))) ; foldcat executes parallely

; returns persistent clojure data structure
(type (into [] (r/map inc [1 2 3])))

;;;;;;; see performance of fully exploited reducers: 
(comment
 (def snums (range 10000000))
 (def snumsv (vec snums)) ; foldable

; fastest: fold parallel, vector, reducers without intermediate colls
; 70 ms
 (time (->> snumsv (r/map inc) (r/filter even?) (r/fold +)))

; slowest: just plain and serial: 400 ms
 (time (->> snums (map inc) (filter even?) (reduce +)))

; reducers with unfoldable coll: 300 ms
 (time (->> snums (r/map inc) (r/filter even?) (r/fold +)))

; reducers with unfoldable coll: 300 ms
 (time (->> snums (r/map inc) (r/filter even?) (reduce +)))

; reducers with foldable coll but reduce: 300 ms
 (time (->> snumsv (r/map inc) (r/filter even?) (reduce +))))


;;;;; transducers

; this creates a transducer
(map inc)

; use it
(into [] (map inc) (range 10))
(sequence (map inc) (range 10))

; why not use seq? what's the difference to sequence?
;(seq (map inc) (range 10)) ; does not work
(seq []) ; delivers nil
(sequence []) ; delivers an empty sequence

; this creates a transducer, too
(def xf
  (comp ; when used with transducers, the order is forward - not backward!
   (filter odd?)
   (map inc)))
   ;(take 50)))

; use the xf
(transduce xf + (range 5))
(into [] xf (range 1000))
(sequence xf (range 10))

(def iter (eduction xf (range 5)))
(reduce + 0 iter)


; Transducers can be used with collections, async channels and with other "sequential data mechanisms".
; They describe the transformation of elements without having to know about the access mechanism of the source.
; While doing this, they use the complete power of clojure core (map, filter, ...)

; -------------------------------------------------------------------
;         your own types: polymorphism, protocolls and more 
; -------------------------------------------------------------------
; https://github.com/ligurio/clojure-from-the-ground-up#polymorphism

;typing
(type '())
(supers clojure.lang.PersistentList$EmptyList) ; all supertypes of a type
(isa? (type '()) clojure.lang.Counted) ; is a type a subtype of another one?
(instance? clojure.lang.PersistentVector [])
(instance? clojure.lang.Counted [])
(instance? clojure.lang.IFn []) ; ahhh...
(instance? java.lang.Integer [])


(defmulti append
  "Appends an x to collection coll."
  (fn [coll x] (type coll))) ; this function return value is used to SWITCH between specific implementations

(defmethod append clojure.lang.PersistentVector [coll x] (conj coll x))
(defmethod append clojure.lang.PersistentList [coll x] (concat coll [x])) ; at the end... slow but "appendish"
(defmethod append java.lang.String [coll x] (str/join "" [coll (str x)]))
(append [1 2] 4)
(append '(1 2) 4)
(append "1 2" 4)

;(append (map inc [1 2]) 4) ; does not work yet
(supers (type (map inc [1 2])))
(defmethod append clojure.lang.Seqable [coll x] (conj coll x :seqable))
(append (map inc [1 2]) 4)

; create hierarchies...
(clojure.repl/doc derive)
(derive ::men ::creature)
(derive ::woman ::men)
(derive ::man ::men)
(derive ::asshole ::man)
(derive ::workoholic ::man)
(derive ::phantastic ::creature)
(derive ::werewolf ::phantastic)
(derive ::frog ::creature)
(derive ::frog ::animal) ; multiple
(derive ::man ::animal) ; multiple
(parents ::frog)
(descendants ::man)
(isa? ::werewolf ::creature)
(isa? ::werewolf ::man)
(isa? ::asshole ::man)


(definterface IAppend
  (append [x]))

(defn grocery-list
  "Creates an appendable grocery list. Takes a vector of
  groceries to buy."
  [to-buy] ; this is closed over
  (reify

    IAppend
    (append [this x]
      (grocery-list (conj to-buy x))) ; therefore, can be used here

    Object
    (toString [this]
      (str "To buy: " to-buy))))

(grocery-list "apples")
(supers (type (grocery-list [:eggs])))

; this tries to call the append of multi-method...
;(append (grocery-list [:eggs]) :tofu)

(.append (grocery-list [:eggs]) :tofu)

; build a wrapper, to avoid .append
(defn append
  "Appends x to the end of coll."
  [coll x]
  (.append coll x))

; now it works...
(str (append (grocery-list [:eggs]) :tofu))

;; BUT: WE CANNOT EXTEND Vector to implement IAppend....
;(.append [1 2] 3)

;; protocol CAN do just that... extend existing types by other types

(defprotocol Append
  "This protocol lets us add things to the end of a collection."
  (append [coll x]
    "Appends x to the end of collection coll."))

(clojure.repl/doc Append)
(clojure.repl/doc append)

(defn grocery-list
  "Creates an appendable (via Append) grocery list. 
   Takes a vector of
   groceries to buy."
  [to-buy]
  (reify

    Append
    (append [this x]
      (grocery-list (conj to-buy x)))

    Object
    (toString [this]
      (str "To buy: " to-buy))))

(str (append (grocery-list [:eggs]) :tomatoes))

(extend-protocol Append
  ; do many different extentions
  ; in one call to extend-protocol
  clojure.lang.IPersistentVector
  (append [v x]
    (conj v x))
  clojure.lang.Sequential
  (append [v x]
    (concat v (list x))))

(extend-protocol Append
  ; call `extend-protocol Append` again - even in different namespace
  nil
  (append [_ x] ; parameter not in use...
    [x]))

(append [1 2] 3)
(append nil 3)

(defrecord GroceryList [to-buy]
  Append
  (append [this x]
    (GroceryList. (conj to-buy x))))

(def gl (->GroceryList [:apples]))
(.-to-buy gl) ; like java field
(.to-buy gl) ; like java method
(:to-buy gl) ; like a clojure map...
(append gl :oranges)

(map ->GroceryList [[:some :more] [:other :less]])
;(map GroceryList. [[:some :more] [:other :less]]) ; not clojure function

; -------------------------------------------------------------------
;         reader, REPL 
; -------------------------------------------------------------------
; https://www.braveclojure.com/read-and-eval/
; https://clojure.org/reference/reader
; https://clojure.org/guides/weird_characters

; REPL
(comment
  (/ 1 2)
  (* 2 5)
  (* *1 *2)
  *e ; the last exception
  (pst) ; print stack trace of last exception
  (pst *e) ; print stack trace of an exception
  (pst 3)) ; 3 frames 


(read-string "(+ 1 2)")

(list? (read-string "(+ 1 2)"))

(conj (read-string "(+ 1 2)") :zagglewag)

(eval (read-string "(+ 1 2)"))

; what happens?
(read-string "#(+ 1 %)")
(read-string "::abc")
; well just reader macros... replacing shortcuts or sorting out...
(read-string "; ignore!\n(+ 1 2)")
(read-string "#_(+ 1 20) (+ 1 2)")

(read-string "(quote form)") ; the IDE may give you 'form

(def x 5)
(def lst '(a b c))
; Syntax quoting, unquote and unquote-splicing... 
; basis for human-readable macros 
`(fred x ~x lst ~@lst 7 8 :nine)

; REPL...
(def multi-form "(println \"Adding 2 and 2 together...\") (+ 2 2)")
(load-string multi-form)

(println (eval (read-string multi-form))) ; only first
(println (eval (load-string multi-form))) ; all

(-> multi-form
    load-string ; Read 
    eval ; Eval
    print) ; Print
    ; this is, what the R_E_P_L does in a L_oop

; -------------------------------------------------------------------
;         macros 
; -------------------------------------------------------------------
; https://www.braveclojure.com/writing-macros/
; https://clojure-doc.org/articles/language/macros/
; Macros are applied before compiling the code
; Avoid them, when functions do the job...
; 
; symbol 'and' is a macro...
(clojure.repl/doc and)

; (reduce and [1 2 3]) ; a macro is expanded by the reader...
; therefore, macros may not be used, where functions are given 
; as parameters to functions. Macros are no vars...
; But they change code at compile time.

; defmacro prevents the parameters to be evaluated before passed
; into the macro body... Therefore, the macro-body can 
; change code by working on the lists...
(defmacro my-m-no-quote [not-evaluated]
  (str "form = " not-evaluated " = " (eval not-evaluated)))
(my-m-no-quote (* 2 3))
(macroexpand '(my-m (* 2 3)))

(defmacro my-m [not-evaluated]
  `(str  "form = " '~not-evaluated " = " ~not-evaluated))

(my-m (* 2 3))
(macroexpand '(my-m (* 2 3)))


(defmacro around-zero [number negative-expr zero-expr positive-expr]
  `(let [number# ~number] ; so number is only evaluated once
     (cond
       (< (Math/abs number#) 1e-15) ~zero-expr
       (pos? number#) ~positive-expr
       :else ~negative-expr)))

(around-zero -0.0000000000000001 "-" "zero" "+")
(eval (around-zero -0.000000000001 '(- 1 2) "zero" '(+ 1 2)))
(around-zero +0.000000000001 "-" "zero" "+")
(around-zero (+ -0.000000000001 0.1) - "zero" +)

(macroexpand-1 '(around-zero -0.0000000000000001 "-" "zero" "+"))

(gensym)
(gensym 'armin)
; this is a abbrevation to create and use
; variables with a unique name - 
; in order to avoid name clashes
; due to the macro gets resolved in a namespace, 
; where macro-variable
; names are already 
`(let [number-count# 22])

(defmacro my-m2 [not-evaluated]
  `(let [bel-n# '~not-evaluated
         bel# ~not-evaluated]
     (str  "form = " bel-n# " = " bel#)))
;

(my-m2 (+ 1 3))
(macroexpand '(my-m2 (+ 1 3)))

(defmacro bel-dbg [body]
  `(let [body# ~body]
     (println "-------- dbg -------->>")
     (println  '~body " => " body#)
     (println "<<------ dbg --------")
     (println)
     body#))


(comment
  (+ 5 (bel-dbg (/ 1 (+ 2 1))))

  ; a little bit strage in thread-last macro
  (->>  (range 100)
        (map (partial + 20))
        (take 10)
        bel-dbg
        (map inc))

  (macroexpand '(->>  (range 100)
                      (map (partial + 20))
                      (take 10) bel-dbg
                      (map inc))))



(defn what-arg [arg]
  (cond
    ;(vector? arg) arg
    (list? arg) [(eval arg)]
    :else arg))
(comment
  (what-arg '(+ 4 5))
  (what-arg (+ 4 5))
  (bel-dbg (+ 4 5))
  (what-arg [3 4])
  (what-arg [])
  (what-arg 4)
  (what-arg nil))



(defmacro make-sure
  "This macro can be used as condition in 
   - pre- and post-conditions.
   - let bindings.
   It produces meaningful error messages by erdos power asserts.
   If everything is ok, that means 
   (assert (pred val)), 
   it returns the val."
  [pred val]
  `(do
     (when-not (~pred ~val)
       (println)
       (println)
       (when (not= '~val (eval ~val)) ; when evaluation makes a difference (not value)
         (println '~val " = " (eval ~val))))
     (when (nil? (ea/assert (~pred ~val)))
       ~val)))

(comment
  (make-sure float? (+ 4.3 (make-sure integer? 5)))
  (macroexpand '(make-sure float? 5))
  (make-sure float? (+ 5 1))
  (make-sure float? nil)
  (make-sure float? [nil nil])
  (make-sure float? [5.0 1])

  (macroexpand '(make-sure #(< 6 %) 5))
  (type #(< 6 %))
  (+ 20 (make-sure float? (+ 5 5)))
  (make-sure float? (+ 5 5))
  (macroexpand '(make-sure float? (+ 5 5)))
  (macroexpand '(make-sure (fn [n m] (< (* n m) 100))  20 4))
  (make-sure (fn [n m] (< (* n m) 100))  20)
  (make-sure (fn [n] (< (* n 4) 100)) 29)
  (def n 20)
  (make-sure  #(< (* % %) 100) n)
  (let [n 20.0
        x (make-sure integer? n)])

  (defn square
    "square n"
    [n]
    {:pre [(make-sure (fn [n] (< n 5)) n)
           (make-sure #(< % 5) n)]}
    (* n n))
  (square 4)
  (square 5))


; -------------------------------------------------------------------
;         special forms - this is the core of clojure... 
; -------------------------------------------------------------------
; Those are neither vars or functions nor macros
; but built in basics
; https://clojure.org/reference/special_forms

; def
(def
  the-name
  "a doc string"
  15)

(if true
  "then this"
  "else that")

(do
  (println "frist")
  (println "second"))

; let
(let [x 15]
  (println x))

(quote '(+ 4 5))

(var the-name)

; fn 
(fn f [x] (println x))

(fn f
  ([] (f "nothing"))
  ([x] (println x)))

; loop and recur
(loop [n 6 acc []]
  (if (> n 0)
    (recur (dec n) (conj acc (* n n)))
    acc))

; monitor-enter and monitor-exit - but you may not use them


; -------------------------------------------------------------------
;         use forms in an ideomatic way... 
; -------------------------------------------------------------------
; https://guide.clojure.style/#idioms

