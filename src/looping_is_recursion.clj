(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2))
   true
   (or (empty? seq1) (empty? seq2))
   false
   (not (== (first seq1) (first seq2)))
   false
   :else
   (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         a-seq a-seq]
    (cond
     (empty? a-seq)
     nil
     (pred (first a-seq))
     index
     :else
     (recur (inc index) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         seq-count (count a-seq)
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum seq-count)
      (recur (+ sum (first a-seq)) seq-count (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [a-set #{}
           a-seq a-seq]
      (if (empty? a-seq)
        a-set
        (recur (toggle a-set (first a-seq)) (rest a-seq))))))

(defn fast-fibo [n]
  (loop [k n
         n 1
         n-1 0]
    (cond
     (zero? k)
     0
     (== k 1)
     n
     :else
     (recur (dec k) (+ n n-1) n))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         a-vec []
         elems #{}]
    (let [fir (first a-seq)]
      (if (or (empty? a-seq) (contains? elems fir))
        a-vec
        (recur (rest a-seq) (conj a-vec fir) (conj elems fir))))))

