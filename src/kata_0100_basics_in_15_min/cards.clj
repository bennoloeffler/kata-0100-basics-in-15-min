(ns kata-0100-basics-in-15-min.cards
  [:require
   ;[clojure.string :as str]
   ;[clojure.set :as set]
   [clojure.test :refer [deftest testing is are]]
   [erdos.assert :as ea]])

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
       (when (not= '~val (eval ~val)) ; when evaluation makes a difference (not value)
         (println '~val " = " (eval ~val) "(type:" (type (eval ~val)) ")")))
     (when (nil? (ea/assert (~pred ~val)))
       ~val)))
(comment
  (let [n "17"
        new-n (make-sure int? n)]
    (println new-n))
  (let [n 17
        new-n (make-sure int? n)]
    (println new-n))
  )

(defmacro make-sure-2
  "This macro can be used as condition in 
   - pre- and post-conditions.
   - let bindings.
   It produces meaningful error messages by erdos power asserts.
   If everything is ok, that means 
   (assert form), 
   it returns the the last element, e.g. 
   (make-sure-2 (= 2 (+ 1 1))) delivers (+ 1 1)"
  [form]
  `(let [l# (last '~form)]
     (when-not ~form
       (when (not= l# (eval l#)) ; when evaluation makes a difference (not value)
         (println (str l# " = " (eval l#)))))
     (when (nil? (ea/assert ~form))
       (eval l#))))
(comment
  (make-sure-2 (= 2 (+ 12 1)))
  (def n "17")
  (let [other-n (make-sure-2 (string? (clojure.string/join (repeat 15 n))))]
    other-n)
  )
; -------------------------------------------------------------------
;                 "A two player card game with simple rules:
;                  Play one Card - if the other has one with more value, both Cards go to him, else to you.
;                  Play two Cards - if the other can go below and above, you need to give a third and the 
;                  other get's your three cards. If you win - so if the other can not go below and above: 
;                  he needs to give you two (random) cards.
;                  Corner Case: if you play pair with 7, an A counts as below - nothing else.
;                  Corner Case: if you play pair with A, a 7 counts as above - nothing else."
;                 - write a data structure to represent the cards, the two players and the state of the game.
;                 - write a function that returns an empty card game structure.
;                 - write a function to initialize it 
;                   (create a shuffeled deck, give every playwer 8 cards, ...)
;                 - write a function to do the next turn.
;                 - write a function to check, if there is a winner.
;                 - provide a data structure, to make strategy decisions.
; -------------------------------------------------------------------

(def n-numbers [7 8 9 10 11 12 13 14])
(def n-colors ["♦️" "♥" "♠" "♣"])

(defn n-create-card [number color]
  (let [m {7 "7" 8 "8" 9 "9" 10 "10" 11 "B" 12 "D" 13 "K" 14 "A"}]
    (as-> [number color] $
      (conj $ (str color (m (first $)))))))

(comment
  (n-create-card 13 "♥")
  (n-create-card 7 "♥"))

(deftest create-card-test
  (are [card num color] (= card (n-create-card num color))
    [13 "♥" "♥K"] 13 "♥"
    [7 "♥" "♥7"] 7 "♥"))


(defn n-create-complete-card-set []
  (for [color n-colors
        number n-numbers]
    (n-create-card number color)))

(deftest create-complete-card-set-test
  (let [all-cards (n-create-complete-card-set)]
    (is (= 32 (count all-cards)))
    (is (= all-cards (distinct all-cards)) "no double cards")))

(comment (n-create-complete-card-set))


;; create forward and backward mapping
;; beteen card date (cd) [7 ♦️ ♦️7] and card keyword ck :♦️7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn n-create-card-mapping [cards]
  (->> (map (fn [e] [(keyword (e 2)) e]) cards)
       (into {})))
(comment (n-create-card-mapping (n-create-complete-card-set)))

(defn n-create-back-mapping [cards]
  (->> (map (fn [e] [e (keyword (e 2))]) cards)
       (into {})))
(comment (n-create-back-mapping (n-create-complete-card-set)))

(def card-map
  "like this: {:♦️7 [7 ♦️ ♦️7]}"
  (n-create-card-mapping (n-create-complete-card-set)))

(def ck->cd card-map)

(def back-map
  "like this: {[7 ♦️ ♦️7] :♦️7}"
  (n-create-back-mapping (n-create-complete-card-set)))

(defn c
  "Get _c_ards from keywords.
   For better visual readability.
   Given cards as collection of keywords :♠8,
   returns a collection of cards as vectors [8 ♠ ♠8].
   Or a single card, if there is only one argument."
  [& ck-coll]
  {:post [(is (= 0 (count (filter nil? %)))
              (str "there are wrong keys in " ck-coll))]} ; use https://github.com/ptaoussanis/truss ?
  (let [cards (seq (map #(ck->cd %) ck-coll))
        result (if (= 1 (count cards))
                 (first cards)
                 cards)]
    result))
(comment (c :♠7 :♣9 :♣A)
         (c :♠7 :♠9 :♠B :♠B :♠K)
         (c :♠7 :♠9 :♠B :♠11 :♠K) ; fail with wrong mapping 
         (c :♠7)
         (c)
         (c nil))

(deftest c-test
  (testing "symbols to cards"
    (is (= [7 "♠" "♠7"] (c :♠7)))
    (is (= '([7 "♠" "♠7"] [9 "♠" "♠9"]) (c :♠7 :♠9))))
  (testing "corner cases"
    (is (= nil (c)))
    (is (= nil (c nil)))))


(defn k
  "Get _k_eyword from cards.
   For better visual readability.
   Given cards as collection of e.g. [8 ♠ ♠8],
   returns a collection of keywords lile :♠8"
  [& c-coll]
  {:post [(is (= 0 (count (filter nil? %)))
              (str "there are wrong keys in " c-coll))]} ; use https://github.com/ptaoussanis/truss ?
  (if (= '(nil) c-coll)
    nil
    (seq (map #(back-map %) c-coll))))
(comment (k [8 "♠" "♠8"] [7 "♠" "♠7"])
         (k [7 "♠" "♠7"] [9 "♠" "♠9"] [11 "♠" "♠B"] [11 "♠" "♠B"] [13 "♠" "♠K"])
         (k)
         (k nil))

(deftest k-test
  (testing "cards to symbols"
    (is (= '(:♠7) (k [7 "♠" "♠7"])))
    (is (= '(:♠7 :♠K) (k [7 "♠" "♠7"] [13 "♠" "♠K"]))))
  (testing "corner cases"
    (is (= nil (k)))
    (is (= nil (k nil)))))

(def c-val
  "value of a card"
  first)
(comment
  (c-val [8 "♠" "♠8"]))

(defn pair
  "create pair with right diff of 
   boarder cases e.g. 9 A(as 6) 7(as 15)"
  [card1 card2]
  (let [v1-raw (c-val card1)
        v2-raw (c-val card2)
        v1 (if (= v1-raw 14) 6 v1-raw)
        v2 (if (= v2-raw 7) 15 v2-raw)
        diff (if (and (= v1-raw v2-raw)
                      (or (= v1-raw 7)
                          (= v1-raw 14)))
               8
               (- v2 v1))]
    [diff card1 card2]))
(comment
  (pair (c :♣7) (c :♣A))
  (pair (c :♣9) (c :♣9))
  (pair (c :♣9) (c :♣10))
  (pair (c :♣A) (c :♣7))
  (pair (c :♣A) (c :♣9))
  (pair (c :♣A) (c :♣A))
  (pair (c :♣7) (c :♣7))
  (pair (c :♣9) (c :♣7)))

(deftest pair-test
  (are [result-pair first-card second-card]
       (= result-pair (pair (c first-card) (c second-card)))
    [7 [7 "♣" "♣7"] [14 "♣" "♣A"]] :♣7 :♣A
    [9 [14 "♣" "♣A"] [7 "♣" "♣7"]]  :♣A :♣7
    [5 [14 "♣" "♣A"] [11 "♣" "♣B"]]  :♣A :♣B
    [6 [9 "♣" "♣9"] [7 "♣" "♣7"]]  :♣9 :♣7
    [0 [9 "♣" "♣9"] [9 "♣" "♣9"]]  :♣9 :♣9))


(defn n-create-game [name-player1, name-player2]
  {:players {:next-turn [:player-1 :player-2]
             :player-1 {:name name-player1
                        :points 0
                        :at-hand #{}}
             :player-2 {:name name-player2
                        :points 0
                        :at-hand #{}}}
   :deck (n-create-complete-card-set)})
(comment (n-create-game "Armin" "Benno"))

(defn n-shuffle-deck [game]
  (update game :deck shuffle))
(comment (n-shuffle-deck (n-create-game "Armin" "Benno")))

(defn n-give-cards
  "Returns game with number cards given to the players."
  [game number]
  #_{:pre  [(= 32 (count (:deck game)))
            (= 0 (count (get-in game [:players :player-1 :at-hand])))
            (= 0 (count (get-in game [:players :player-2 :at-hand])))]
     :post [(= (- 32 (* 2 number)) (count (:deck %)))
            (= number (count (get-in % [:players :player-1 :at-hand])))
            (= number (count (get-in % [:players :player-2 :at-hand])))]}
  (-> game
      (assoc-in [:players :player-1 :at-hand] (take number (game :deck)))
      (assoc-in [:players :player-2 :at-hand] (take number (drop number (game :deck))))
      (assoc :deck (drop (* 2 number) (game :deck)))))
(comment
  (-> (n-create-game  "Benno" "Armin")
      n-shuffle-deck
      (n-give-cards 16)))


(def a-game {:players {:next-turn [:player-1 :player-2],
                       :player-1 {:name "Benno",
                                  :points 0,
                                  :at-hand '([9 "♠" "♠9"] [10 "♥" "♥10"] [12 "♥" "♥D"] [7 "♠" "♠7"]
                                                          [13 "♣" "♣K"] [13 "♠" "♠K"] [11 "♠" "♠B"] [7 "♦️" "♦️7"])},
                       :player-2 {:name "Armin",
                                  :points 0,
                                  :at-hand '([14 "♣" "♣A"] [12 "♣" "♣D"] [11 "♦️" "♦️B"] [7 "♥" "♥7"]
                                                           [9 "♥" "♥9"] [10 "♣" "♣10"] [10 "♠" "♠10"] [8 "♥" "♥8"])}},
             :deck '([8 "♦️" "♦️8"] [9 "♦️" "♦️9"] [12 "♠" "♠D"] [11 "♣" "♣B"]
                                    [13 "♦️" "♦️K"] [8 "♣" "♣8"] [10 "♦️" "♦️10"] [14 "♥" "♥A"]
                                    [7 "♣" "♣7"] [13 "♥" "♥K"] [9 "♣" "♣9"] [8 "♠" "♠8"]
                                    [14 "♦️" "♦️A"] [14 "♠" "♠A"] [12 "♦️" "♦️D"] [11 "♥" "♥B"])})
(def b-game {:players {:next-turn [:player-1 :player-2],
                       :player-1 {:name "Benno",
                                  :points 0,
                                  :at-hand '([9 "♠" "♠9"] [10 "♥" "♥10"]  [7 "♦️" "♦️7"])},
                       :player-2 {:name "Armin",
                                  :points 0,
                                  :at-hand '([7 "♥" "♥7"] [9 "♥" "♥9"] [10 "♣" "♣10"]
                                                          [10 "♠" "♠10"] [8 "♥" "♥8"])}},
             :deck '([8 "♦️" "♦️8"] [9 "♦️" "♦️9"] [12 "♠" "♠D"] [11 "♣" "♣B"]
                                    [13 "♦️" "♦️K"] [8 "♣" "♣8"] [10 "♦️" "♦️10"] [14 "♥" "♥A"]
                                    [7 "♣" "♣7"] [13 "♥" "♥K"] [9 "♣" "♣9"] [8 "♠" "♠8"]
                                    [14 "♦️" "♦️A"] [14 "♠" "♠A"] [12 "♦️" "♦️D"] [11 "♥" "♥B"])})

(def c-game
  {:players
   {:next-turn [:player-1 :player-2],
    :player-1
    {:name "Benno",
     :points 0,
     :at-hand
     '([14 "♦️" "♦️A"]
       [13 "♥" "♥K"]
       [9 "♥" "♥9"]
       [7 "♠" "♠7"]
       [12 "♠" "♠D"]
       [7 "♣" "♣7"]
       [11 "♣" "♣B"]
       [8 "♥" "♥8"]
       [12 "♥" "♥D"]
       [8 "♠" "♠8"]
       [9 "♣" "♣9"]
       [7 "♥" "♥7"]
       [9 "♠" "♠9"]
       [11 "♠" "♠B"]
       [13 "♠" "♠K"]
       [13 "♦️" "♦️K"])},
    :player-2
    {:name "Armin",
     :points 0,
     :at-hand
     '([12 "♣" "♣D"]
       [10 "♦️" "♦️10"]
       [10 "♥" "♥10"]
       [11 "♦️" "♦️B"]
       [10 "♠" "♠10"]
       [14 "♥" "♥A"]
       [7 "♦️" "♦️7"]
       [12 "♦️" "♦️D"]
       [11 "♥" "♥B"]
       [14 "♣" "♣A"]
       [13 "♣" "♣K"]
       [9 "♦️" "♦️9"]
       [8 "♦️" "♦️8"]
       [10 "♣" "♣10"]
       [8 "♣" "♣8"]
       [14 "♠" "♠A"])}},
   :deck ()})

(defn n-all-pairs
  "A pair is a lower and a higher card and the difference
   as first element - sorted by difference:
   [[7 :07 :14-A]
    [6 :07 :13-K]
    [6 :08 :14-A]
    ...]
   You may also provide cards. The result looks like:
   [...
    [7 [7 ♦️ ♦️7] [14 ♦️ ♦️A]]
     6 [8 ♦️ ♦️8] [14 ♦️ ♦️A]
    ...]"
  [cards-coll]
  (vec
   (sort
    #(compare (- (first %1)) (- (first %2)))
    (vec (set (for [l cards-coll
                    h cards-coll
                    :let [lv (c-val l)
                          hv (c-val h)
                          d (- hv lv)]
                    :when (>= d 0)]
                (pair l h)))))))
(comment
  (n-all-pairs [[7 "♦️" "♦️7"]
                [8 "♦️" "♦️8"]
                [9 "♦️" "♦️9"]]))


(defn no-doubles
  "returns false, if there multiples in the coll"
  [coll]
  (or (empty? coll) ; empty case fails by next condition..
      (= coll (seq (distinct coll)))))  ; seq to handle nil

(deftest no-doubles-test
  (testing "empty cases"
    (is (no-doubles nil))
    (is (no-doubles []))
    (is (not (no-doubles [nil nil]))))
  (testing "success"
    (is (no-doubles [nil]))
    (is (no-doubles [1 2 3])))
  (testing "fails"
    (is (not (no-doubles [1 2 [3 4] [3 4]])))))

(defn n-find-pairs-with-diff
  "Providing a coll of cards and a coll of numbers, it provides all the
   diffs, that are available. Example:
   (find-pairs-with-diff #{:07♠ :09♠} [1 2 5 4]) => ([2 :07♠ :09♠] [there could be more...])"
  [cards coll-of-diffs]
  {:pre [(make-sure no-doubles coll-of-diffs)]}; "in pair-strategies, a diff value may appear only once!"
  (let [all-pairs-of-hand (n-all-pairs cards)
        set-of-diffs (set coll-of-diffs)]
    (->> all-pairs-of-hand
         (filter #(set-of-diffs (first %)))
         (sort-by ; found that in docs... do not understand...
          #((into {} (map-indexed (fn [i e] [e i]) coll-of-diffs)) (first %)))
         seq)))

(comment ; handle nil gracefully...
  (n-find-pairs-with-diff (c :♠7 :♠9) [2])
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠D :♠A) [2 4])
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B :♠A) [4 5 6 7 3 2 1 0])
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B) [1])
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B) [1 2 1]) ;fail!
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B) [])
  (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B) nil)
  (n-find-pairs-with-diff (c) nil))

(deftest n-find-pair-with-diff-test
  (testing "normal pairs"
    (are [expected cards pairs] (= expected (n-find-pairs-with-diff cards pairs))
      '([2 [7 "♠" "♠7"] [9 "♠" "♠9"]]) (c :♠7 :♠9) [2]
      '([2 [12 "♠" "♠D"] [14 "♠" "♠A"]]
        [2 [9 "♠" "♠9"] [11 "♥" "♥B"]]
        [2 [7 "♠" "♠7"] [9 "♠" "♠9"]]
        [4 [7 "♠" "♠7"] [11 "♥" "♥B"]]) (c :♠7 :♠9 :♥B :♠D :♠A) [2 4]
      nil (c :♠7 :♥7) [5 6]))
  (is (thrown? Throwable (n-find-pairs-with-diff (c :♠7 :♠9 :♥B :♠B) [1 2 1]))))


(defn n-get-card-with-value
  "Delivers first card found with value or nil."
  [value, hand]
  (let [found (filter #(= value (c-val %)) hand)]
    (first found)))

(deftest get-card-with-value-test

  (are [result value cards] (= result (n-get-card-with-value value cards))
    (c :♥B) 11 (c :♠7 :♠9 :♥B :♠B)
    (c :♠B) 11 (c :♠7 :♠9 :♠B)
    nil 8 (c :♠7 :♠9 :♥B :♠B)))

(defn n-try-find-value
  "Return the first possible single card in the coll-of-values."
  [hand coll-of-values]
  {:pre [(make-sure no-doubles coll-of-values)]} ; "in single-strategies, a value may appear only once!"
  (->> coll-of-values
       (map #(n-get-card-with-value % hand))
       (remove nil?)
       first))
(comment
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [9 14 12 7])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [11])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) nil)
  (n-try-find-value (c) [9 14 12 7 9]))

(deftest try-find-value-test
  (testing "find cards"
    (are [result cards coll] (= result (n-try-find-value cards coll))
      [12 "♣" "♣D"] (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [9 14 12 7]
      [12 "♣" "♣D"] (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [12]
      [7 "♠" "♠7"] (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [7 8]))
  (testing "don't find cards"
    (are [cards coll] (nil? (n-try-find-value cards coll))
      (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [9 14]
      (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) []
      (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [9])))

(defn n-try-find-pair
  "Return the possible diff-pair in the sequence of coll-of-diffs."
  [hand coll-of-diffs]
  (let [pairs (n-find-pairs-with-diff hand coll-of-diffs)]
    ;(println (str "try-find-pair, all: " pairs))
    (first pairs)))
(comment
  (n-try-find-pair (c :♣B :♣D :♦️8 :♠7 :♠B :♣A :♣7 :♣K) [2 1 5 6 7 3])
  (n-try-find-pair (c :♣7 :♣A :♣K) [4 3 2 1 6 5 7])
  (n-try-find-pair (c :♣B :♣D :♦️8 :♠7 :♠B :♣A :♣7 :♣K) [16])
  (n-try-find-pair (c :♣B :♣D :♦️8 :♠7 :♠B :♣A :♣7 :♣K) [])
  (n-try-find-pair (c) [1 2 7 15 16])
  (n-try-find-pair (c) nil))
(deftest try-find-pair-test
  (testing "Context of the test assertions"
    (are [result cards pair-diffs] (= result (n-try-find-pair cards pair-diffs))
      [1 [13 "♣" "♣K"] [14 "♣" "♣A"]] (c :♣7 :♣A :♣K) [1]
      [1 [13 "♣" "♣K"] [14 "♣" "♣A"]] (c :♣7 :♣A :♣K) [4 3 2 1 6 5 7]
      nil  (c :♣7 :♣K) [1]
      nil  (c :♣7 :♣K) []
      nil  (c) [])))

(def act-strategy [[:single [14]]
                   [:pair   [7 6 5]] ; diff
                   [:single [13 12]] ; value
                   [:pair   [4]]
                   [:single [11 10]]
                   [:pair   [3]]
                   [:random-single []]])

(def loose-strategy [[:single [10 11 9 8 7 12 13 14]]
                     [:single [10 11 9 8 7 12 13 14]]]) ; choose, which card to give, if you cannot win

(defn n-apply-strategy-entry
  "Apply one strategy entry to a coll of cards (hand).
   A strategy entry may be one of those:
   [:pair   [1 7 6 5]] ; finds pair with diff 1 before diff 7, 7 before 6 ...
   [:single [14 7 12 13]] ; finds value 14 before 7, 7 before 12, ... 
   [:random-single []] ; finds a random single card
    ...]"
  [hand strategy-entry]
  (let [strategy-type (first strategy-entry)
        strategy-params (second strategy-entry)]
    (case strategy-type
      :pair (n-try-find-pair hand strategy-params)
      :single (n-try-find-value hand strategy-params)
      :random-single (first hand)
      (throw (ex-info (str "Error in strategy: " strategy-entry)
                      {:strategy-type-unknown strategy-type})))))
(comment
  (def ase n-apply-strategy-entry)
  (def h (c  :♦️8 :♠7  :♣A :♣7 :♣K))
  (ase h [:single [10 8 07]])
  (ase h [:single [9 11 7]])
  (ase h [:single [10]])
  (ase h [:single [1 1]]) ;fail

  (ase h [:single nil])
  (ase h [:pair [1 6 5 7 3 2 1]]) ; fail
  (ase h [:pair [3 4 1 7 2]]))

(defn apply-strategy [hand strategy]
  (->> strategy
       (map #(n-apply-strategy-entry hand %))
       (remove nil?)))
(apply-strategy (c  :♦️8 :♠7  :♣A :♣7 :♣K) act-strategy)

(defn remove-card
  "returns the cards without the card"
  [card cards]
  (remove #(= % card) cards))
(comment
  (remove-card  (c :♣D) (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (remove-card  (c :♣D) (c :♣B :♣K))
  (remove-card nil (c :♣B :♣B))
  (remove-card  (c :♣D) (c))
  (remove-card nil (c))
  (remove-card nil nil))


(defn pair? [card-or-pair]
  (not (string? (get card-or-pair 2))))
(comment
  (pair? [2 [8 "♣" "♣8"] [10 "♣" "♣10"]])
  (pair? [7 "♣" "♣8"]))

(declare play)
(declare a-game)
(comment (play a-game))

(defn start-turn
  "The player, whose turn it is, does his turn.
   Returns the played cards - and the rest of the hand."
  [game]

  (let [player (get-in game [:players :next-turn 0])
        ;_ (println "player" player)
        hand (get-in game [:players player :at-hand])
        ;_ (println "hand" hand)
        possible-moves (apply-strategy hand act-strategy)
        ;_ (println "MOVES" possible-moves)
        move (first possible-moves)

        rest-hand (if (pair? move)
                    (->> hand
                         (remove-card (move 1))
                         (remove-card (move 2)))
                    (remove-card move hand))] ; replace that by reduce / stop with reduced
    ;(println possible-moves)
    [move rest-hand]))

(comment
  (start-turn a-game)
  (start-turn b-game))

(defn sort-cards
  [pos-or-neg cards]
  (sort
   #(compare (pos-or-neg (first %1)) (pos-or-neg (first %2)))
   cards))
(comment
  (sort-cards + (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (sort-cards - (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K)))


(defn find-lower-single-for-pair
  [value my-hand]
  (let [sorted-cards (sort-cards - my-hand)
        highest-card (first sorted-cards)
        highest-value (c-val highest-card)]
    (if (= value 7)
      (if (= 14 highest-value)
        [highest-card (remove-card highest-card my-hand)]
        [nil my-hand])
      (let [the-one (->> sorted-cards
                         (filter #(< (first %) value))
                         first)]
        [the-one (remove-card the-one my-hand)]))))
(comment
  (find-lower-single-for-pair 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single-for-pair 7 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-lower-single-for-pair 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single-for-pair 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single-for-pair 8 (c :♣A)))

(defn find-lower-single
  [value my-hand]
  (let [sorted-cards (sort-cards - my-hand)
        the-one (->> sorted-cards
                     (filter #(< (first %) value))
                     first)]
    [the-one (remove-card the-one my-hand)]))
(comment
  (find-lower-single 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 7 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-lower-single 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 8 (c :♣A)))


(defn find-higher-single-for-pair
  "Rind higher card (7 for A).
   Return the card and the rest of the hand.
   Rturn nil and hand, if there was nothing higher."
  [value my-hand]
  (let [sorted-cards (sort-cards + my-hand)
        lowest-card (first sorted-cards)
        lowest-value (c-val lowest-card)]
    (if (= value 14)
      (if (= 7 lowest-value)
        [lowest-card (remove-card lowest-card my-hand)]
        [nil my-hand])
      (let [the-one (->> sorted-cards
                         (filter #(> (first %) value))
                         first)]
        [the-one (remove-card the-one my-hand)]))))
(comment
  (find-higher-single-for-pair 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single-for-pair 14 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-higher-single-for-pair 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single-for-pair 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single-for-pair 14 (c :♣A)))


(defn find-higher-single
  [value my-hand]
  (let [sorted-cards (sort-cards + my-hand)
        the-one (->> sorted-cards
                     (filter #(> (first %) value))
                     first)
        seven (if (and (nil? the-one) (= 14 value))
                (->> sorted-cards
                     (filter #(= 7 (first %)))
                     first)
                nil)
        the-one (or seven the-one)]
    [the-one (remove-card the-one my-hand)]))
(comment
  (find-higher-single 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 14 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-higher-single 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 14 (c :♣A :♣B))
  (find-higher-single 14 (c :♣A :♣7 :♣B)))


(defn find-better-pair
  "find pair with higher spread: if pair opponent is e.g.
   [2 [12 ♣ ♣D] [14 ♣ ♣A]] we need e.g. [4 [11 ♣ ♣B] [7 ♣ ♣7]]"
  [pair-opponent my-hand]
  ;(println pair-opponent)
  ;(println (get pair-opponent 1))
  ;(println (c-val (get pair-opponent 1)))
  (let [[lower hand1] (find-lower-single-for-pair (c-val (get pair-opponent 1)) my-hand)
        [higher hand2] (find-higher-single-for-pair (c-val (get pair-opponent 2)) hand1)]
    (if (and lower higher)
      [(pair lower higher) hand2]
      [nil my-hand])))
(comment
  (def hand1 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (def hand2 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣A))

  (find-better-pair  ["" [8 "♣" "♣8"] [10 "♣" "♣10"]] hand1)
  (find-better-pair  ["" [8 "♣" "♣8"] [13 "♣" "♣K"]] hand1)
  (find-better-pair  ["" [8 "♣" "♣8"] [14 "♣" "♣A"]] hand2)
  (find-better-pair  ["" [7 "♣" "♣8"] [14 "♣" "♣A"]] hand2))

(defn better-single? [potentially-better-single single]
  (or (> (c-val potentially-better-single) (c-val single))
      (and (= 14 (c-val single))
           (= 7 (c-val potentially-better-single)))))

(defn better-pair?
  [potentially-better-pair pair]
  {:pre [(some? pair) (some? potentially-better-pair)]}
  (and (or (< (first (get potentially-better-pair 1)) (first (get pair 1)))
           (and (= 14 (first (get potentially-better-pair 1))) (= 7 (first (get pair 1)))))
       (or (> (first (get potentially-better-pair 2)) (first (get pair 2)))
           (and (= 7 (first (get potentially-better-pair 2))) (= 14 (first (get pair 2)))))))

(comment
  (better-pair? [2 [8 "" ""] [14 "" ""]]   [0 [7 "" ""] [14 "" ""]]) ; dont care for (wrong) diffs
  (better-pair? ["" [7 "" ""] [11 "" ""]]  ["" [8 "" ""] [10 "" ""]]) ; nomal case
  (better-pair? ["" [14 "" ""] [11 "" ""]] ["" [7 "" ""] [10 "" ""]]) ; A under 7
  (better-pair? ["" [7 "" ""] [7 "" ""]]   ["" [9 "" ""] [14 "" ""]]) ; 7 over A
  (better-pair? ["" [14 "" ""] [7 "" ""]]  ["" [7 "" ""] [14 "" ""]]) ; both corner cases
  (better-pair? ["" [7 "" ""] [14 "" ""]]  ["" [8 "" ""] [13 "" ""]])
  (better-pair? ["" [8 "" ""] [12 "" ""]]  ["" [9 "" ""] [11 "" ""]])
  (better-pair? ["" [7 "" ""] [14 "" ""]]  ["" [13 "" ""] [11 "" ""]])) ; tolerate this


(defn react-turn
  "The player, who has to react, reacts to the turn of the first player.
   The played cards of the first player are given.
   Returns the cards, this player plays as reaction - and the remaining hand."
  [cards game]
  (let [player (get-in game [:players :next-turn 1])
        hand (get-in game [:players player :at-hand])
        answer (if (pair? cards)
                 (find-better-pair cards hand)
                 (find-higher-single (first cards) hand))
        found-better (first answer)
        possible-loses1 (apply-strategy hand loose-strategy)
        lose1 (first possible-loses1)
        possible-loses2 (apply-strategy (remove-card lose1 hand) loose-strategy)
        lose2 (second possible-loses2)
        losing-hand (if (pair? cards)
                      (->> hand
                           (remove-card lose1)
                           (remove-card lose2))
                      (remove-card lose1 hand))]
    (if found-better
      answer
      (if (pair? cards)
        [["X" lose1 lose2] losing-hand]
        [lose1 losing-hand]))))

(comment
  (def hand1 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (def hand2 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣A))

  (react-turn  (pair [8 "♣" "♣8"] [10 "♣" "♣10"]) a-game)
  (react-turn  (pair [8 "♣" "♣8"] [13 "♣" "♣K"]) a-game)
  (react-turn  (pair [8 "♣" "♣8"] [14 "♣" "♣A"]) b-game)
  (react-turn  (pair [7 "♣" "♣8"] [14 "♣" "♣A"]) b-game)

  (let [[cards-turn player-turn-hand] (start-turn a-game)
        [cards-react player-react-hand] (react-turn cards-turn a-game)]))

(defn higher? [cards-react cards]
  (if (or (nil? cards-react)
          (nil? (cards-react 2)))
    false
    (if (pair? cards)
      (better-pair?  cards-react cards)
      (better-single? cards-react cards))))
(comment
  (higher? (card-map :♣B) (card-map :♣D))
  (higher? (card-map :♣D) (card-map :♣B))
  (higher? (pair (card-map :♣B) (card-map :♣D))
           (pair (card-map :♣10) (card-map :♣K)))
  (higher? (pair (card-map :♣10) (card-map :♣K))
           (pair (card-map :♣B) (card-map :♣D))))

(defn card-as-vec [card-or-pair]
  (if (pair? card-or-pair)
    [(card-or-pair 1) (card-or-pair 2)]
    [card-or-pair]))
(comment
  (card-as-vec [2 [2 "b" "b"] [3 "c" "c"]])
  (card-as-vec [2 "b" "b"]))
(defn next-turn
  ""
  [game]
  (let [[cards-turn player-turn-hand] (start-turn game)
        [cards-react player-react-hand] (react-turn cards-turn game)
        all-cards (remove nil? (concat (card-as-vec cards-turn) (card-as-vec cards-react)))
        players (get-in game [:players :next-turn])
        ;_ (println "cards react " cards-react)
        ;_ (println "cards turn " cards-turn)

        who-won-turn (if (higher? cards-react cards-turn)
                       (get players 1)
                       (get players 0))
        who-lost-turn (if (higher? cards-react cards-turn)
                        (get players 0)
                        (get players 1))

        turn-changes (not= (first players) who-won-turn)
        players-new (if turn-changes
                      (vec (reverse players))
                      players)
        ;_ (println players-new)
        shuffled-in (shuffle all-cards)
        pair (pair? cards-turn)
        ass (and (not pair) (= 14 (c-val cards-turn)))
        back-card (if pair #_(or ass pair) (first shuffled-in) nil)
        win-cards (if pair #_(or ass pair) (rest shuffled-in) shuffled-in)]
    ;(println (get-in game [:players (players 0) :name]) ",  turn: " cards-turn "   hand: " player-turn-hand)
    ;(println (get-in game [:players (players 1) :name]) ", react: " cards-react "   hand: " player-react-hand)
    ;(println who-won-turn "got: " win-cards)
    ;(println who-lost-turn "lost ")
    ;(when back-card (println "got back: " back-card))
    ;(println "next move: " players)
    (let [game (-> game
                   (assoc-in [:players (players 0) :at-hand] player-turn-hand)
                   (assoc-in [:players (players 1) :at-hand] player-react-hand)
                   (update-in [:players who-won-turn :at-hand] concat win-cards)
                   (assoc-in [:players :next-turn] players-new))]
      (if back-card
        (update-in game [:players who-lost-turn :at-hand] conj back-card)
        game))))

(comment
  (next-turn a-game))

(defn winner
  "Returns the winner - if there is one.
   Returns nil, if there is not yet a winner.
   You win, if the other has no more cards."
  [game]
  (if-not (seq (get-in game [:players :player-1 :at-hand]))
    (get-in game [:players :player-2 :name])
    (when-not (seq (get-in game [:players :player-2 :at-hand]))
      (get-in game [:players :player-1 :name]))))

(defn play [game]
  (loop [game game round 0]
    ;(println "----- ROUND: " round)
    (if-let [end (or
                  (if (> round 20)
                    :exhausted-by-20-rounds
                    nil)
                  (winner game))]
      end
      (recur (next-turn game) (inc round)))))

(comment
  (let [new-game #(-> (n-create-game "Armin" "Benno")
                      n-shuffle-deck
                      (n-give-cards 8)
                      play)]
    (frequencies (map (fn [_] (new-game)) (range 100))))

  (let [same-game #(play a-game)]
    (frequencies (map (fn [_] (same-game)) (range 1000)))))

(defn -main [& args]
  (println (play a-game)))

(comment
  (-main)
  )


