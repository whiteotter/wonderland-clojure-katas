(ns card-game-war.game)

;; feel free to use these cards or use your own data structure
(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn rank-position [ranking-list rank-val]
  (.indexOf ranking-list rank-val))

(defn card-rank [card]
  (rank-position ranks (last card)))

(defn suit-rank [card]
  (rank-position suits (first card)))

(defn play-round [player1-card player2-card]
  (let [player1-card-rank (card-rank player1-card)
        player1-suit-rank (suit-rank player1-card)
        player2-card-rank (card-rank player2-card)
        player2-suit-rank (suit-rank player2-card)]
    (if (= player1-card-rank player2-card-rank) (if (> player1-suit-rank player2-suit-rank) 0 1)
        (if (> player1-card-rank player2-card-rank) 0 1))))

(defn play-game [player1-cards player2-cards]
  (loop [player1-deck player1-cards player2-deck player2-cards]
    (cond (zero? (count player2-deck)) 0 ;; Player 1 wins
          (zero? (count player1-deck)) 1 ;; Player 2 wins
          :else (let [round-winner (play-round (first player1-deck) (first player2-deck))
                      player1-winning-deck (reverse (cons (first player1-deck) (cons (first player2-deck) (reverse (rest player1-deck)))))
                      player2-winning-deck (reverse (cons (first player2-deck) (cons (first player1-deck) (reverse (rest player2-deck)))))]
            (if (= 0 round-winner) (recur player1-winning-deck (rest player2-deck))
                                   (recur (rest player1-deck) player2-winning-deck))))))
