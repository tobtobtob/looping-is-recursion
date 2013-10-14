(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp] (if (zero? exp) acc
                                    (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (= [] (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [loop-seq a-seq
         index 0]
    (cond (empty? loop-seq) nil
          (pred (first loop-seq)) index
          :else (recur (rest loop-seq) (inc index)))))

(defn avg [a-seq]
  (loop [loop-seq a-seq
         amount 0
         sum 0]
    (if (empty? loop-seq) (/ sum amount)
        (recur (rest loop-seq) (inc amount) (+ sum (first loop-seq))))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                     (disj a-set elem)
                     (conj a-set elem)))]
    (loop [parity-set #{}
         loop-seq a-seq]
    (if (empty? loop-seq) parity-set
      (recur (toggle parity-set (first loop-seq)) (rest loop-seq))))))

(defn fast-fibo [n]
  (cond (<= n 0) 0
        (== n 1) 1
        :else
        (loop [prev 1
               prev-prev 0
               index (- n 2)]
          (if (== 0 index)
            (+ prev prev-prev)
            (recur (+ prev prev-prev) prev (dec index))))))

(defn cut-at-repetition [a-seq]
  (loop [new-seq []
         loop-seq a-seq]
    (cond (some #(= (first loop-seq) %) new-seq) new-seq
          (empty? loop-seq) new-seq
          :else (recur (conj new-seq (first loop-seq)) (rest loop-seq)))))

