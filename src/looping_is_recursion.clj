(ns looping-is-recursion)

(defn power [base exp]
  (let [aux (fn [res base2 exp2]
             (cond
              (= 0 exp2) res
              (= 1 exp2) (* res base2)
              (= 0 (mod exp2 2)) (recur res (* base2 base2) (int (/ exp2 2)))
              :else (recur (* res base2) base2 (dec exp2))))]
    (aux 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq)) (first a-seq)
    (last-element (rest a-seq))))


(defn seq= [seq1 seq2]
  (cond (empty? seq1) (empty? seq2)
        (empty? seq2) false
        (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
        :else false))


(defn find-first-index [pred a-seq]
  (loop [idx 0
         seqi a-seq]
    (cond
     (empty? seqi) nil
     (pred (first seqi)) idx
     :else (recur (inc idx) (rest seqi)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         seqi a-seq]
    (if (empty? seqi) (/ sum n)
      (recur (+ sum (first seqi)) (inc n) (rest seqi)))))

(defn parity [a-seq]
  (let [toggle (fn [s v] (if (contains? s v) (disj s v) (conj s v)))]
    (loop [res #{}
           seqi a-seq]
      (if (empty? seqi) res
        (recur (toggle res (first seqi)) (rest seqi))))))


(defn fast-fibo [n]
  (if (<= n 1) n
    (loop [f_n1 1
           f_n 1
           v 2]
      (if (= n v) f_n
        (recur f_n (+ f_n f_n1) (inc v))))))


(defn cut-at-repetition [a-seq]
  (loop [iseq a-seq
         s #{}
         resi '()]
    (if (or (empty? iseq) (contains? s (first iseq))) (reverse resi)
            (recur (rest iseq) (conj s (first iseq)) (cons (first iseq) resi)))))




