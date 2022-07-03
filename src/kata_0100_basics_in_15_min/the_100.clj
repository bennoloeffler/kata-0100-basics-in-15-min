(ns kata-0100-basics-in-15-min.the-100)

;; the 100 most used clojure functions
;; analysed here: https://ericnormand.me/article/100-most-used-clojure-expressions

; defn ; Define a function at the top level.;228347
(defn f
  "Comment."
  [parameter]
  (* 5 parameter))
(f 4)

; let ; Bind names in a local scope.;201799
(let [x "string-value"])

; = ;Return true if all arguments are equal, false otherwise.;183713
(= "Return true if all arguments are equal, false otherwise."
   "183713")

; is ; Assert that an expression returns truthy inside of a test. ; 182587
(use 'clojure.test)
(deftest my-test
  (is (= 42 43)))
(run-tests)

; if ;Two-way conditional branch. ; 102992
(if true
  "this"
  "else that")

; fn ; Create a unnamed function. ; 78498
(fn [x] x)

; def ; Define a top level Var. ; 68451
(def x 1)

; str ; Convert the arguments to strings and concatenate them. ; 65937
(str "create " 1 " string")

; deftest ; Define a test. ; 62985
(use 'clojure.test)
(deftest my-test
  (is (= 42 43)))

; map ; Create a seq whose elements are the result of applying a function to the elements of another seq.;52295
(map inc (range 3))

; ns ; Define a namespace at the top of a file. ; 49644
(ns kata-0100-basics-in-15-min.the-100)

; -> ; Thread-first macro. ; 46439
(-> -5
    (/ 10)
    (- 100)
    double)

; defn- ; Define a function at the top level that is private to the namespace. ; 44741
(defn- my-priv [x y z]
  (let [z (* x y z)]
    z))
; first ; Return the initial element of a seq. ; 37981
(first [4 6 9])

; when ; One-way conditional branch. ; 34247
(when true
  (prn 5)
  :and-return-that-afterwards)

; testing ; Add context text to enclosed assertions. ; 32891
(deftest my-test
  (testing "basic operations"
    (is (= 10 (* 2 5)))
    (is (= 4 (/ 20 5))))
  (testing "relations"
    (is (> 10 2 1))
    (is (< 4 6 78))))

; or ; Evaluate expressions in turn, returning the first that is truthy or nil otherwise. ; 31252
(or false nil :something-true)

; apply ; Call a function on a seq of arguments. ; 30907
(apply max [-4 2 5 -9])

; assoc ; Add one or more key/value pairs to an associative data structure such as a hashmap or vector.;29215
(assoc {:x 0 :z 223} :x 15 :y 22)

; count ; Return the number of elements in the given collection.;28660 
(count [3 4 5])

; and ; Evaluate expressions in turn, returning the first that is not truthy, or the last one otherwise.;26442
(and 2 3 "a value")

; not ; Return true if the argument is falsey, false otherwise.;24785 
(not :some-truthy-thing)

; nil? ; Return true if the argument is nil, false otherwise.;21570 
(nil? :something-except-nil)

; defmethod ; Define a method in a multimethod.;20983

; + ; Add all arguments numerically.;19170
(+ 1 2 3 4)

; defmacro ; Define a new macro.;18829
(defmacro m
  "prints value and returns it"
  [form]
  `(prn '~form)
  form)

; get ; Return the value associated with the key in an associative data structure.;18547
(get [23 4 5 6] 2)

; * ; Multiply all arguments numerically.;16945
(* 1 2 3)

; cond  ; Multi-way conditional branch.;16672
(cond
  (> 3 2) "this is true" ; first wins
  (> 5 2) "this is also true"
  :else "default")

; recur ; Explicit tail recursion.;16569
(loop [i 0 acc []]  
  (if (< i 5)
    (recur (inc i) (conj acc (* i i i)))
    acc))

; println ; Print out all arguments and output a new line.;16419
(println "Hello World!")

; do ; Execute all expressions and return the value of the last expression.;15841
(do (println :hey)
    (println :ho)) 

; seq ; Convert a value to a seq or nil if it's empty. ; 15683
(seq [2 3 4])
(seq [])

; doseq ; Iterate over a seq, executing the body expressions for each element. ; 15442
(doseq [x (range 5)
        y (range 3)]
  (println x " * " y " = " (* x y)))

; ->> ; Thread last macro. ; 15004
(->> [2 5 9]
     (map (partial * 10))
     (reduce *))

; throw ; Raise an exception.;14419
(when false
  (throw (ex-info         ; ExceptionInfo
          "Error is this" ; description 
          {:x 1 :y 2}))) ; data

; atom ; Create a Atom. ; 14024
(def v (atom {}))

; reduce ; Starting with an initial value, apply a function to that value and successive elements of a seq.;13785
(reduce + 100 '(9 10 11))

; - ; Subtract arguments numerically, or negate a number.;13236
(- 100 9 10 11)

; name ; Return the non-namespace part of a keyword or symbol as a string, or the argument itself if it's a string.;12880
(name 'clojure.core/+)

; instance? ; Check if a value is an instance of a class or interface.;12674
(instance? clojure.lang.Seqable [])

; conj ; Add an element to a collection.;12548
(conj [1 2 3] 5)


; if-let ; Two-way conditional branch with binding of the test result.;12483
(if-let [x :some-calculation-result]
  (println "delivered... " x)
  (println "x does not exist"))

; inc ; Add one to a number.;12482
(inc 5)

; swap! ; Replace an Atom's value with the result of applying a function to the current value.;12462

; into ; Add elements of a seq to a collection.;11580
(into #{1 2 3} [1 2 3 4 4 4 5 5 5])

; range ; Create a seq of numbers from start to end.;11124
(range 1 20 4)

; filter ; Keep elements of a seq that are truthy for a given predicate.;10851
(filter pos? [-1 0 5 -9 0 4 7 8 8 8])

; get-in ; Get the value from a nested associative data structure at a path.;10604
(get-in  {:x {:y {:z 1}} :a 2} [:x :y :z])

; merge ; Add key/value pairs from one map into another.;10506
(merge {:x 1 :y 2} {:x 3 :z 1})

; empty? ; Returns true if there are no elements in a collection, false otherwise.;10495
(empty? [1])

; loop ; Create a scope for tail recursion.;10419

; for ; List comprehension.;10234
(for [x [0 1 2 3 4 5]
      :let [y (* x 3)]
      :when (even? y)]
  y)

; try ; Create a point for catching exception.;9680
(try
  (/ 1 0)
  (catch Exception e (str "caught exception: " (.getMessage e))))

; list ; Create a list.;9523
(list 2 5 9)

;Create a string using java.lang.String.format.;9508
(format "x in percent %.1f" (* 100 0.0188345))

; catch ; Declare what to do when a particular class of exception is caught.;9105

; when-not ; Negative one-way conditional branch.;8992
(when-not (seq '())
  "this is true")

; rest ; Return a seq of elements from a collection excluding the first.;8766
(rest [1 2 3 4 5])

; < ; Mathematical less than.;8618
(< 1 3 7 9)

; vec ; Convert a collection to a vector.;8601
(vec '(1 3 5))

; partial ; Take a function and arguments and make a new function that has those arguments already applied.;8595
(map (partial + 5) [100 200])

; concat ; String seqs together in order.;8334
(concat [1 2 3] '(2 3 4))

; reset! ; Set the value of an Atom without regard to the current value.;8122

; set ; Convert a collection to a set.;8071
(set [1 1 1 2 2 2])

; when-let ; One-way conditional branch with binding of the test result.;8040
(when-let [x (seq [1 2 3])]
  [x x x])

; / ; Numeric division.;8033
(/ 1 3)
(/ 1.0 3)

; int ; Convert a number to an integer.;7811
(int 2.3)

; cons ; Add an element to the beginning of a seq.;7652
(cons 0 '(1 2 3))

; nth ; Retrieve an element from a seq by numeric index.;7628
(nth [:a :b] 1)
(nth [:a :b] 1 :default)
(nth "01234" 7 "other")

; assert ; Throw an AssertionError if expression is false.;7613

; defproject ; Define a Leiningen project.;7604

; second ; Return the second element of a seq.;7565
(second [1 2 3 4])

; are ; Assert that multiple expressions are truthy in a test.;7445

contains?;Does a key appear in an associative data structure?;7300
(contains? {:a 1} :a)
(contains? ["a" "b"] "b")

; update-in ; Modify a value at a path in a nested data structure by applying a function to that value.;7266
(def users [{:name "James" :age 26}  {:name "John" :age 43}])
(update-in users [1 :age] inc)

; > ; Mathematical greater than.;7223
(> 3 4)

; doto ; Execute forms on first argument, then return it.;7221
(doto (java.util.HashMap.)
  (.put "a" 1)
  (.put "b" 2)
  (println))

; defprotocol ; Define a new protocol.;7077

; . ; Execute a Java method.;7051
(. "abc" (toUpperCase))

; meta ; Return the metadata on a value.;6737
(meta #'first)

; == ; Numeric equality comparison (type-independent).;6720

; keys ; Return a seq of keys from a hash map.;6616

; next ; Return a seq of elements from a collection excluding the first, or nil if it's empty.;6535
(next [2 3 4])

; map? ; Return true if the argument is a map, false otherwise.;6528
(map? [3 4 5])

; string? ; Return true if the argument is a string, false otherwise.;6468
(string? "123")

; set! ; Set thread-local vars, Java object instance fields, and Java class static fields.;6421
(set! *warn-on-reflection* true)

; aget ; Return the value of an array at an index.;6363

; keyword ; Create a keyword from a string or a namespace and name.;6338
(keyword "to-keyword")

; if-not ; Two-way negative conditional branch.;6323
(if-not (zero? 0) :then :else)

; symbol ; Create a symbol from a string or a namespace and name.;5839
(symbol "clojure.core" "foo")

; binding ; Create thread-local bindings for dynamic vars.;5829

; dec ; Subtract one from a number.;5823
(dec 5)

; dissoc ; Remove a key/value from an associative data structure.;5720
(dissoc {:a 1 :b 2 :c 3} :b)

; defrecord ; Create a new record type.;5409

; comp ; Compose two or more functions.;5305
(def negative-quotient (comp - /))
(negative-quotient 8 3)

; not= ; Return true if the arguments are not equal, false otherwise.;5286
(not= 1 2)

; thrown? ; A special form in clojure.test/is expressions to check if an exception is thrown.;5249

; float ; Convert a number to a Java float.;5175
(float 1.111111111111111111111111111M)

; select ; Return a new set keeping only elements that are truthy for a given predicate.;5129