(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base expr acc]
                 (cond
                  (= expr 0) 1
                  (= expr 1) acc
                  :else (recur base (dec expr) (* acc base))))]
    (helper base exp base)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (not= (count a-seq) (count b-seq)) false
                   (and (= (count a-seq) 0) (= (count b-seq) 0)) true
                   (not= (first a-seq) (first b-seq)) false
                   :else (recur (rest a-seq) (rest b-seq))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
        seq a-seq ]
  (cond
    (empty? seq) nil
    (pred (first seq)) index
    :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [index 0
        acc 0
        seq a-seq ]
  (cond
    (empty? seq) (if (zero? index) acc (/ acc index))
    :else (recur (inc index) (+ acc (first seq)) (rest seq)))))

(defn parity [a-seq]
  (loop [acc #{}
        seq a-seq
        elem (first seq)]
    (if (nil? elem)
      acc
      (recur
        (if (contains? acc elem)
          (disj acc elem)
          (conj acc elem)) (rest seq) (first (rest seq))))))

(defn fast-fibo [n]
  (loop [index 1
         acc 0
         a 1
         b 1]
    (cond
      (= n 0) 0
      (<= n 2) 1
      (= index (- n 1)) acc
      :else (recur (inc index) (+ a b) (+ a b) a ))))

(defn cut-at-repetition [a-seq]
  (loop [seq a-seq
         acc []]
    (let [elem (first seq)
          has? (fn [x] (= elem x))]
      (cond
      (empty? seq) acc
      (= (count (filter has? acc)) 1) acc
      :else (recur (rest seq) (conj acc elem))))))
