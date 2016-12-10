(ns tiny-maze.solver)

(defn find-position-for-sym [maze-rows sym]
  (loop [row-index 0]
    (if (>= row-index (count maze-rows)) nil
        (let [maze-row (get maze-rows row-index)
              col-index (.indexOf maze-row sym)]
          (if (>= col-index 0) [row-index col-index]
              (recur (inc row-index)))))))

(defn maze-start [maze]
  (find-position-for-sym maze :S))

(defn maze-end [maze]
  (find-position-for-sym maze :E))

(defn all-relative-positions-for [[row-index col-index]]
  (loop [paths [[0 -1] [1 0] [0 1] [-1 0]] positions []]
    (if (nil? (first paths)) positions
        (let [path (first paths)
              new-row-index (+ row-index (get path 0))
              new-col-index (+ col-index (get path 1))]
          (recur (rest paths) (conj positions [new-row-index new-col-index]))))))

(defn valid-route? [maze-rows [row-index col-index]]
  (let [column-length (count (get maze-rows 0))
        row-length (count maze-rows)]
    (cond (or (< col-index 0) (< row-index 0))     false
          (>= col-index column-length)             false
          (>= row-index row-length)                false
          (= ((maze-rows row-index) col-index) 1)  false
          (= ((maze-rows row-index) col-index) :x) false
          :else                                    true)))

(defn mark-maze [maze-rows [row-index col-index] marker]
  (let [orig-row (maze-rows row-index)
        new-row (assoc orig-row col-index marker)]
    (assoc maze-rows row-index new-row)))

(defn make-move [maze-rows [row-index col-index]]
  (let [start-pos (maze-start maze-rows)
        maze-with-start-marked-x (mark-maze maze-rows start-pos :x)]
    (mark-maze maze-with-start-marked-x [row-index col-index] :S)))

(defn solve-maze [maze]
  (let [start-pos (maze-start maze)
        end-pos (maze-end maze)]
    (if (nil? end-pos) (mark-maze maze start-pos :x) ;; start-pos has overwritten/reached end-pos sym on maze, successfully completed
        (let [all-pos (all-relative-positions-for start-pos)
              all-valid-pos (filter #(valid-route? maze %) all-pos)]
          (loop [positions all-valid-pos result nil]
            (let [position (first positions)]
              (if (nil? position) result ;; all valid relative positions explored, result may or may not possess completed maze
                  (let [next-maze (make-move maze position)]
                    (recur (rest positions) (or result (solve-maze next-maze)))))))))))
