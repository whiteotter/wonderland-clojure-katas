(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn rank [card]
  (last card))

(defn suit [card]
  (first card))

(defn rank-score [card]
  (.indexOf ranks (rank card)))

(defn suit-score [card]
  (.indexOf suits (suit card)))

(defn card-score [card]
  (let [rank-score (rank-score card)
        suit-score (suit-score card)]
    (+ suit-score (* rank-score (count suits)))))

(defn play-round [card-1 card-2]
  (let [card-1-score (card-score card-1)
        card-2-score (card-score card-2)]
    (if (> card-1-score card-2-score) 0 1)))

(defn out-of-cards? [deck]
  (zero? (count deck)))

(defn won-card [losing-deck]
  (first losing-deck))

(defn winning-deck [deck won-card]
  (let [new-deck (rest deck)
        winner-card (first deck)]
    (conj new-deck won-card winner-card)))

(defn losing-deck [deck]
  (rest deck))

(defn play-game [player1-cards player2-cards]
  (loop [deck-1 player1-cards deck-2 player2-cards]
    (cond (out-of-cards? deck-2) 0 ;; Player 1 wins
          (out-of-cards? deck-1) 1 ;; Player 2 wins
          :else (let [round-winner (play-round (first deck-1) (first deck-2))]
            (if (= 0 round-winner) (recur (winning-deck deck-1 (won-card deck-2))
                                          (losing-deck deck-2))
                                   (recur (losing-deck deck-1)
                                          (winning-deck deck-2 (won-card deck-1))))))))
