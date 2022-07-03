(ns kata-0100-basics-in-15-min.cards
  [:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test :refer [is]]])

; -------------------------------------------------------------------
;         KATA 3: "A two player card game with simple rules:
;                  Play one Card - if the other has one with more value, both Cards go to him, else to you.
;                  Play two Cards - if the other can go below and above, you need to give a third and the 
;                  other get's your three cards. If you win - so if the other can not go below and above: 
;                  he needs to give you two (random) cards.
;                  Corner Case: if you play pair with 7, an A counts as below - nothing else.
;                  Corner Case: if you play pair with A, a 7 counts as above - nothing else."
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

(defn n-create-complete-card-set []
  (for [color n-colors
        number n-numbers]
    (n-create-card number color)))
(comment (n-create-complete-card-set))

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

(def back-map
  "like this: {[7 ♦️ ♦️7] :♦️7}"
  (n-create-back-mapping (n-create-complete-card-set)))

;(def all-keys (sort (keys card-map)))

(defn c
  "Get _c_ards from keywords.
   For better visual readability.
   Given cards as collection of keywords :♠8,
   returns a collection of cards as vectors [8 ♠ ♠8]"
  [& ck-coll]
  {:post [(is (= 0 (count (filter nil? %))) 
              (str "there are wrong keys in " ck-coll))]} ; use https://github.com/ptaoussanis/truss ?
  (seq (map #(card-map %) ck-coll)))
(comment (c :♠7 :♣9 :♣A)
         (c :♠7 :♠9 :♠B :♠B :♠K)
         (c :♠7 :♠9 :♠B :♠11 :♠K) ; fail with wrong mapping 
         (c :♠7)
         (c)
         (c nil))

(defn k 
  "Get _k_eyword from cards.
   For better visual readability.
   Given cards as collection of e.g. [8 ♠ ♠8],
   returns a collection of keywords lile :♠8"
  [& c-coll]
  {:post [(is (= 0 (count (filter nil? %)))
              (str "there are wrong keys in " c-coll))]} ; use https://github.com/ptaoussanis/truss ?
  (seq (map #(back-map %) c-coll)))
(comment (k [8 "♠" "♠8"] [7 "♠" "♠7"])
         (k [7 "♠" "♠7"] [9 "♠" "♠9"] [11 "♠" "♠B"] [11 "♠" "♠B"] [13 "♠" "♠K"])
         (k) 
         (k nil); this fails
         )

(def c-val first)

(defn pair [card1 card2]
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
  (pair (card-map :♣7) (card-map :♣A))
  (pair (card-map :♣9) (card-map :♣9))
  (pair (card-map :♣9) (card-map :♣10))
  (pair (card-map :♣A) (card-map :♣7))
  (pair (card-map :♣A) (card-map :♣9))
  (pair (card-map :♣A) (card-map :♣A))
  (pair (card-map :♣7) (card-map :♣7))
  (pair (card-map :♣9) (card-map :♣7)) 
  )


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
      (n-give-cards 8)))
  

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
                [9 "♦️" "♦️9"]])
  )


(defn unique-or-empty [coll-of-diffs]
  (or (empty? coll-of-diffs) ; empty case fails by next condition..
      (= coll-of-diffs (seq (distinct coll-of-diffs)))))  ; seq to handle nil

(defn n-find-pairs-with-diff
  "Providing a coll of cards and a coll of numbers, it provides all the
   diffs, that are available. Example:
   (find-pairs-with-diff #{:07♠ :09♠} [1 2 5 4]) => ([2 :07♠ :09♠] [there could be more...])"
  [cards coll-of-diffs]
  {:pre [(is (unique-or-empty coll-of-diffs)
             "in pair-strategies, a diff value may appear only once!")]}
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


(defn n-get-card-with-value
  "Delivers first card found with value or nil."
  [value, hand]
  (let [found (filter #(= value (c-val %)) hand)]
    (first found)))

(n-get-card-with-value 11 (c :♠7 :♠9 :♥B :♠B))
(n-get-card-with-value 11 (c :♠7 :♠9 :♠B))
(n-get-card-with-value 8 (c :♠7 :♠9 :♥B :♠B))

(defn n-try-find-value
  "Return the first possible single card in the coll-of-values."
  [hand coll-of-values]
  {:pre [(is (unique-or-empty coll-of-values)
             "in single-strategies, a value may appear only once!")]}
  (->> coll-of-values
       (map #(n-get-card-with-value % hand))
       (remove nil?)
       first))
(comment
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [9 14 12 7])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [11])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) [])
  (n-try-find-value (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K) nil)
  (n-try-find-value (c) [9 14 12 7]))


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
  

(def act-strategy [[:pair   [7 6 5]] ; diff
                   [:single [14 13 12]] ; value
                   [:pair   [4]]
                   [:single [11 10]]
                   [:pair   [3]]
                   [:random-single []]])

(def react-strategy {:single [10 11 9 8 7 12 13 14] ; just higher - but start in the middle
                     :pair [1 2 3 4 5 6 7]}) ; just start with those with small diff

(def loose-strategy [10 11 9 8 7 12 13 14]) ; choose, which card to give, if you cannot win

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
  (ase h [:pair [ 3 4 1 7 2]]))
  

(defn start-turn
  "The player, whose turn it is, does his turn.
   Returns the played cards."
  [game]

  (let [player (get-in game [:players :next-turn 0])
        hand (get-in game [:players player :at-hand])
        possible-moves (remove nil? (map #(n-apply-strategy-entry hand %) act-strategy))] ; replace that by reduce / stop with reduced
    ;(println possible-moves)
    (first possible-moves)))


(comment
  (start-turn a-game)
  (start-turn b-game))
  
(defn sort-cards 
  [pos-or-neg cards ]
  (sort
   #(compare (pos-or-neg (first %1)) (pos-or-neg (first %2))) 
   cards))
(comment
  (sort-cards + (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (sort-cards - (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  )

(defn remove-card 
  "returns the cards without the card"
  [card cards]
  (remove #(= % card) cards))
(comment 
  (remove-card (first (c :♣D)) (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (remove-card (first (c :♣D)) (c :♣B ))
  (remove-card nil (c :♣B))
  (remove-card (first (c :♣D)) (c))
  (remove-card nil (c))
  (remove-card nil nil)
  )

(defn find-lower-single [value my-hand]
  ;sort descending by value
  ;find first that is lower
  ;special case A is lower than 7
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
  (find-lower-single 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 7 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-lower-single 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-lower-single 8 (c :♣A))
  )

(defn find-higher-single [value my-hand]
  ;sort descending by value
  ;find first that is lower
  ;special case A is lower than 7
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
  (remove #(= % (first (c :♣D))) (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 13 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 14 (c :♣B :♣D :♦️8 :♠7 :♣A :♠B :♣7 :♣K))
  (find-higher-single 11 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 7 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (find-higher-single 14 (c :♣A)))

(defn find-better-pair 
  "find pair with higher spread: if pair opponent is e.g.
   [2 [12 ♣ ♣D] [14 ♣ ♣A]] we need e.g. [4 [11 ♣ ♣B] [7 ♣ ♣7]]"
  [pair-opponent my-hand]
  (let [[lower my-hand] (find-lower-single (c-val (get pair-opponent 1)) my-hand)
        [higher my-hand] (find-higher-single (c-val (get pair-opponent 2)) my-hand)]
    (if (and lower higher)
      [(pair lower higher) my-hand]
      nil))
  )
(comment
  (def hand1 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣K))
  (def hand2 (c :♣B :♣D :♦️8 :♠7 :♠B :♣7 :♣A))

  (find-better-pair  ["" [8 "♣" "♣8"] [10 "♣" "♣10"]] hand1)
  (find-better-pair  ["" [8 "♣" "♣8"] [13 "♣" "♣K"]] hand1)
  (find-better-pair  ["" [8 "♣" "♣8"] [14 "♣" "♣A"]] hand2) 
  (find-better-pair  ["" [7 "♣" "♣8"] [14 "♣" "♣A"]] hand2)
  )

(defn pair? [card-or-pair]
  (string? (get card-or-pair 2)))

(defn better-pair? [pair potentially-better-pair]
  #_(>= (- (first potentially-better-pair) (first pair)) 2)
  (and (or (< (first (get potentially-better-pair 1)) (first (get pair 1)))
           (and (= 14 (first (get potentially-better-pair 1))) (= 7 (first (get pair 1)))))
       (or (> (first (get potentially-better-pair 2)) (first (get pair 2)))
           (and (= 7 (first (get potentially-better-pair 2))) (= 14 (first (get pair 2)))))))

(comment
  (better-pair? [0 [7 "" ""] [14 "" ""]] [2 [8 "" ""] [14 "" ""]]) ; dont care for (wrong) diffs
  (better-pair? ["" [8 "" ""] [10 "" ""]] ["" [7 "" ""] [11 "" ""]]) ; nomal case
  (better-pair? ["" [7 "" ""] [10 "" ""]] ["" [14 "" ""] [11 "" ""]]) ; A under 7
  (better-pair? ["" [9 "" ""] [14 "" ""]] ["" [7 "" ""] [7 "" ""]]) ; 7 over A
  (better-pair? ["" [7 "" ""] [14 "" ""]] ["" [14 "" ""] [7 "" ""]]) ; both corner cases
  (better-pair? ["" [8 "" ""] [13 "" ""]] ["" [7 "" ""] [14 "" ""]])
  )


(defn react-turn
  "The player, who has to react, reacts to the turn of the first player.
   The played cards of the first player are given.
   Returns the cards, this player plays as reaction."
  [cards game]
  (let [player (get-in game [:players :next-turn 1])
        hand (get-in game [:players player :at-hand])
        answer (if (pair? cards)
                 (find-better-pair cards hand)
                 (find-higher-single cards hand))]
    answer))

(defn higher? [cards cards-react]
  (if (nil? cards-react)
    false
    (if (pair? cards)
      (better-pair? cards cards-react)
      (> (first cards-react) (first cards))))
  )

(defn next-turn
    "Simple rules:
   Play one Card - if the other has one with more value, both Cards go to him.
   Play two Cards - if the other can go below and above, you need to give a third.
   Otherwise, you get two of him.
   Corner Case: if you play pair with 7, an A counts as below - nothing else.
   Corner Case: if you play pair with A, a 7 counts as above - nothing else."
    [game]
  ; who is to act?
  ; act
  ; react
  ; potentially sacrifice a card if pair was lost
  ; give cards to winner
    (let [
          cards (start-turn game)
          cards-react (react-turn cards game)
          players (get-in game [:players :next-turn])
          who-won (if (higher? cards cards-react)
                    (get players 1)
                    (get players 0))
          players (get-in game [:players :next-turn])
          turn-changes (not= (first players) who-won)
          players (if turn-changes 
                    (reverse players) 
                    players)]
      (-> game
          (update-in [:players who-won :at-hand] conj all-cards)
          (assoc-in [:players :next-turn] players))))

(defn winner
  "Returns the winner and some data - if there is one.
   Returns :both, if there was a remis.
   Returns nil, if there is not yet a winner.
   Winner is, if the other has no more cards."
  [game]

  nil)

#_(defn play [game]
    (loop [game game round 0]
      (if-let [end (or
                    (if (> round 50)
                      :exhausted-by-50-rounds
                      nil)
                    (winner game))]
        (println (str "GAME OVER: " end))
        (recur (next-turn game) (inc round)))))




