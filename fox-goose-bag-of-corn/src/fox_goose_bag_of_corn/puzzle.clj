(ns fox-goose-bag-of-corn.puzzle)

(def start-pos [[[:fox :goose :corn :you] [:boat] []]])

(defn all-present? [bank items-to-check]
  (loop [items items-to-check result true]
    (if (empty? items) result
        (recur (rest items) (and result (some #(= (first items) %) bank))))))

(defn invalid-river-bank? [bank]
  (and (not (all-present? bank [:you]))
       (some #(all-present? bank %) '([:fox :goose] [:corn :goose]))))

(def valid-river-bank? (complement invalid-river-bank?))

(defn find-all-moveable-items [direction [left-bank-items right-bank-items]]
  (cond (= direction :left) (if (invalid-river-bank? right-bank-items) [:goose] [nil])
        (= direction :right) (cond (all-present? left-bank-items [:fox :goose :corn]) [:goose] 
                                   (all-present? left-bank-items [:fox :goose]) [:fox]
                                   (all-present? left-bank-items [:goose :corn]) [:corn]
                                   :else left-bank-items)))

(defn gen-move-option [left-bank-items right-bank-items direction item]
  (cond (= :left direction) (if (nil? item) {:on-boat-pos [left-bank-items [:boat :you] right-bank-items]
                                             :on-bank-pos [(into [:you] left-bank-items) [:boat] right-bank-items]}
                                (let [right-bank-option (vec (filter #(not (= item %)) right-bank-items))]
                                  {:on-boat-pos [left-bank-items [:boat :you item] right-bank-option]
                                   :on-bank-pos [(into [:you item] left-bank-items) [:boat] right-bank-option]}))
        (= :right direction) (let [left-bank-option (vec (filter #(not (= item %)) left-bank-items))]
                               {:on-boat-pos [left-bank-option [:boat :you item] right-bank-items]
                                :on-bank-pos [left-bank-option [:boat] (into [:you item] right-bank-items)]})))

(defn gen-move-options [[left-bank boat right-bank :as current-move]]
  (let [direction (if (some #(= :you %) left-bank) :right :left)
        items-only (fn [bank] (vec (filter #(not (= :you %)) bank)))
        left-bank-items (items-only left-bank)
        right-bank-items (items-only right-bank)]
    (loop [moveable-items (find-all-moveable-items direction [left-bank-items right-bank-items]) options []]
      (if (empty? moveable-items) options
          (let [generated-move-option (gen-move-option left-bank-items right-bank-items direction (first moveable-items))]
            (recur (rest moveable-items) (conj options generated-move-option)))))))

(defn build-all-plan-variants [[left-bank boat right-bank :as current-move]]
  (if (= 4 (count right-bank)) [[current-move]] ;; success base case, begin to return a list all crossing plans
                                                ;; at next stack level, becomes [[previous-move-to-left-bank previous-move-to-boat current-move]]
                                                ;; when joined with other variants [[A-first-move A-second-move A-third-move]
                                                ;;                                  [B-first-move B-second-move B-third-move]]
      (let [move-options (gen-move-options current-move)]
        (loop [moves move-options recursed-answers []]
          (if (empty? moves) recursed-answers
              (let [move (first moves)
                    boat-pos (get move :on-boat-pos)
                    bank-pos (get move :on-bank-pos)
                    zero-or-many-plans (build-all-plan-variants bank-pos)
                    processed-plans (if (empty? zero-or-many-plans) recursed-answers
                                        (vec (map #(into [current-move boat-pos] %) zero-or-many-plans)))]
                (recur (rest moves) processed-plans)))))))

(defn pick-shortest-crossing-plan [all-plans]
  (reduce #(if (< (count %1) (count %2)) %1 %2) all-plans))

(defn river-crossing-plan
  ([] (->> (first start-pos)
           (build-all-plan-variants)
           (pick-shortest-crossing-plan)))
  ([custom-start-pos] (->> custom-start-pos
                           (build-all-plan-variants)
                           (pick-shortest-crossing-plan))))
