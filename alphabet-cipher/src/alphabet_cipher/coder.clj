(ns alphabet-cipher.coder)

(def alphabet (vec "abcdefghijklmnopqrstuvwxyz"))

(defn shift-alphabet-by [x]
  (loop [acc []]
    (if (= (count alphabet) (count acc)) acc
        (recur (conj acc (get alphabet (mod (+ x (count acc)) (count alphabet))))))))

(defn start-alphabet-with-letter [letter]
  (shift-alphabet-by (.indexOf alphabet letter)))

(defn encode-letter [unencoded-letter keyword-letter]
  (get (start-alphabet-with-letter keyword-letter) (.indexOf alphabet unencoded-letter)))

(defn decode-letter [encoded-letter keyword-letter]
  (get alphabet (.indexOf (start-alphabet-with-letter keyword-letter) encoded-letter)))

(defn ciphertext-message-conversion [keyword text convert-letter-fn]
  (loop [original text result ""]
    (let [keyword-letter (get keyword (mod (count result) (count keyword)))]
      (if (zero? (count original)) result
          (recur (rest original)
                 (str result
                      (convert-letter-fn (first original) keyword-letter)))))))

(defn encode [keyword message]
  (ciphertext-message-conversion keyword message encode-letter))

(defn decode [keyword ciphertext]
  (ciphertext-message-conversion keyword ciphertext decode-letter))

(defn get-keyword-letter [ciphertext-letter message-letter]
  (loop [keyword-letter (first alphabet)]
    (let [decoded-letter (decode-letter ciphertext-letter keyword-letter)]
      (if (= message-letter decoded-letter) keyword-letter
          (recur (get alphabet (inc (.indexOf alphabet keyword-letter))))))))

(defn full-cipher [ciphertext message]
  {:pre [(= (count ciphertext) (count message))]}
  (loop [ciphertext ciphertext message message result ""]
    (if (zero? (count ciphertext)) result
        (recur (rest ciphertext)
               (rest message)
               (str result
                    (get-keyword-letter (first ciphertext) (first message)))))))

(defn decipher [ciphertext message]
  (let [full-cipher (full-cipher ciphertext message)]
    (loop [i 1]
      (let [cipher (apply str (take i full-cipher))]
        (if (= ciphertext (encode cipher message)) cipher
            (recur (inc i)))))))
