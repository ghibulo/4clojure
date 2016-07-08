(ns ctvrtepokusy.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;--------------------------------
Equivalence Classes #98 - rozgrupovani mnoziny pomoci funkce 

(defn ec [f s] (set (map #(set (map last %)) (vals (group-by first (map #(vector (f %) %) s))))))
(#(set (map set (vals (group-by %1 %2)))) 
 #(* % %) #{-2 -1 0 1 2}); -> #{#{0} #{1 -1} #{2 -2}})     ([[0 0]] [[1 1] [1 -1]] [[4 -2] [4 2]]) 

;takhle to melo byt :-)
#(set (map set (vals (group-by %1 %2)))) 


;--------------------------------

;Identify keys and values #105... rozdelit list na mapu podle pozice prvku
(defn ikv [v] 
  (reduce #(assoc %1 (first (first %2)) (vec (last %2)))
          {} (map 
               #(vector (first %) (filter number? (last %))) 
               (partition 2 (partition-by keyword? (interleave  v (repeat (count v) [] )))))))
;(ikv [:a 1 2 3 :b :c 4]);->{:c [4], :b [], :a [1 2 3]} 
;alternativa rekurzi
(fn mf [s]
  (if (seq s)
    (merge {(first s) (take-while (complement keyword?) (rest s))}
           (mf (drop-while (complement keyword?) (rest s)))) {} ))

;--------------------------------

;Digits and bases #137 ...prevod do jine (y) soustavy
(defn konv [x y] 
  (let [f (atom x) v (atom [])] 
    (do (println "f=" @f "v=" @v) (while (> @f 0) (do (swap! v conj (rem @f y)) (reset! f (quot @f y)))) (if (empty? @v) [0] (reverse @v))))) 
;(konv 1020 16);->(3 15 12)  
;takhle jsem to mel udelat :-)
(fn [x b]
  (reverse
    (map #(mod (quot x %) b) (cons 1 (take-while #(<= % x) (iterate #(* b %) b))))))

;--------------------------------

;Sequence of pronunciations #110 - vyslovnost seznamu 'tri jednicky jedna dvojka...'
(defn pron [p]
  (letfn [(pl [x] (->> x 
                  (partition-by identity)
                  (mapcat #(vector (count %) (first %)) )
                  vec
                  ))]
  (iterate pl (pl p))))
(take 4 (pron  [1 1 1 4 4]));->([1 1 1 4 4] [3 1 2 4] [1 3 1 1 1 2 1 4] [1 1 1 3 3 1 1 2 1 1 1 4]) 
(take 3 (pron [1]))
;alternativa s juxt:
(fn [s]
  (rest
  (iterate
   (fn [t]
     (flatten (map (juxt count first) (partition-by identity t))))
   s)))

;--------------------------------

;Oscilrate #144 ... strida postupne fce na mezivysledku a pak znovu
;aneb jak si uchovavat v umelem parametru stav lazyseq

(def posl 
  (letfn [(posl [e pf t] (if (empty? t) (posl e pf pf) (lazy-seq (cons e (posl ((first t) e) pf (rest t))))))]
       (fn[a & b] (posl a b b))))

;(= (take 12 (posl 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
;kdyby clovek znal "reductions" :-)
(fn [v & f] (reductions #(%2 %) v (cycle f)))
;--------------------------------

(defn malgr [& vicoj] 
  (letfn [ (eEnV [e p] (= (first (drop-while  #(< % e) p)) e))
           (eEnVj [e sp] (every? #(eEnV e %) sp ))] ;(eEnVj -10 [(drop 5 (range)) (range)  (drop 8 (range))]) ;-> false
  (let   [cxiu (apply interleave vicoj)]
         (first (filter #(eEnVj % vicoj) cxiu)))))
;alternativa
(fn [& s]
  (loop [s s]
    (if (every? #(= (first %) (ffirst s)) s) ;vsechny posloupnosti maji prvni prvek stejny
      (ffirst s)
      (let [t (sort-by first s)]; serad podle prvniho
            (recur (cons (rest (first t)) (rest t))))))) ;znovu loop - uz bez toho prvniho prvku
;dalsi alternativa...
(fn [& sqs]
    (let [fsq (map first sqs) ;ze seznamu posloupnosti, seznam prvnich prvku
          mfs (apply min fsq)];ktery je z nich nejmensi? = mfs
      (if (apply = fsq) mfs ;nejsou si vsichni rovni?
          (recur (map (fn [sq] (drop-while #(= mfs %) sq)) sqs))))) ;znovu, ale ze zacatku vsech seqenci vyhod uz nespravny mfs


;(malgr (drop 5 (range)) (range)  (drop 8 (range)));-> 8
;(malgr [3 4 5])
;(= 4 (malgr [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
;(= 7 (malgr (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;(= 64 (malgr (map #(* % % %) (range)) ;; perfect cubes
;          (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;          (iterate inc 20))) ;; at least as large as 20


;--------------------------------

;Decurry #158 ... z neznameho poctu vnorenych jednoparametrickych funkci vytvorit jednu normalni
 
(= 25 ((m (fn [a]
             (fn [b]
               (* a b))))
       5 5))
 
;(defn m[x  ] (partial x ))
;((m *) 10 20)

;(((fn [a] (fn [b] (* a b))) 10 ) 20)



