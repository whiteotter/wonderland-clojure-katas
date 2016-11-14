(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)
               (set)))

(defn all-possible-char-shifts-from-word [word]
  (loop [index 0 char-shifted-words []]
    (if (>= index (count word)) char-shifted-words
        (let [char-int (int (.charAt word counter))]
        ;; charAt doesn't account for all potential locations of char!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          (let [upchar-the-word-at-index (str )
                downcar-the-word-at-index ()]
            (recur (inc index) (conj char-shifted-words upchar-the-word-at-index downchar-the-word-at-index)))))))

(defn all-recognized-words-from-set)

(defn doublets [word1 word2]
  (if (not= (count word1) (count word2)) []
      (loop [pathed-word word1 destination-word word2 doublets-found []]
        (if (= pathed-word destination-word) (conj doublets-found destination-word)
            (let [next-word (word-with-single-char-change pathed-word doublets-found)]
              (if (nil? next-word) []
                  (recur next-word destination-word (conj doublets-found pathed-word))))))))
