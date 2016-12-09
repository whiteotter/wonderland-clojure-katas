(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

;; fill in  tests for your game
(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= 0 (play-round [:spade 3] [:spade 2]))))
  (testing "queens are higher rank than jacks"
    (is (= 0 (play-round [:spade :queen] [:spade :jack]))))
  (testing "kings are higher rank than queens"
    (is (= 0 (play-round [:spade :king] [:spade :queen]))))
  (testing "aces are higher rank than kings"
    (is (= 0 (play-round [:club :ace] [:heart :king]))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= 0 (play-round [:club 9] [:spade 9]))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= 0 (play-round [:diamond 8] [:club 8]))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= 1 (play-round [:diamond 3] [:heart 3])))))

(deftest test-play-game
  (testing "the player with only one card loses"
    (is (= 1 (play-game (take 1 cards) (drop 1 cards)))))
  (testing "game between first and last 26 cards"
    (is (= 1 (play-game (take 26 cards) (drop 26 cards)))))
  (testing "player with all the aces wins the game"
    (let [aces (filter #(= :ace (last %)) cards)
          non-aces (filter #(not= :ace (last %)) cards)]
      (is (= 1 (play-game non-aces aces))))))
