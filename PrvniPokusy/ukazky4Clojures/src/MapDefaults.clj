(in-ns 'user)
(ns MapDefaults)
(defn mapdef
  [df klice]
    (zipmap   klice (repeat df) ))

(+ 1 1)
(mapdef 1 [:a :b :c])


(defn nTyPrvek [[x & y] i] (if (zero? i) x (nTyPrvek y (dec i))))

(nTyPrvek [1 2 3 6] 2)

(nTyPrvek [1 2 8 2] 3)

(defn nTyPrvek2 [x i] ((zipmap (range) x) i))

(nTyPrvek2 [1 2 3 6] 2);3

(defn pocet ([x] (pocet x 0)) 
            ([x i] (if (empty? x) i (pocet (rest x) (inc i)))))

(pocet [:a :b :c :eee] ) ;3

(defn mojeReverse [ceho] 
     ((fn ob [a b] 
        (if (empty? b) a (ob (conj a (last b)) (butlast b))) ) [] ceho))


(def palindrom? #(if (string? %) (= (seq %) (reverse (seq %))) (= % (reverse  %))))


(palindrom? "ahojjoha") ;true

;------------- zbytecne slozite-------------
;dvojice z fibonanciho posloupnosti
(defn fibposl [[x y]] ( vector (+ x y) (+ x y y)))
;nekonecna iterace
(def fibiterate (iterate fibposl [1 1]))
(take 3 fibiterate);([0 1] [1 2] [3 5])
(defn fibposlPoradi [x] (if (even? x) 
                          (last (nth fibiterate (/ x 2)))
                          (first (nth fibiterate (/ (inc x) 2)))))
;do prvního seznamu připoj druhý
(defn spoj [x y]  (conj (conj x (first y)) (last y)))
(reduce spoj (take 5 fibiterate)); [1 1 2 3 5 8 13 21 34 55]
;---------------------------------------------

(defn fibPoslLazy [x y] (lazy-seq 
                          (cons x (fibPoslLazy y (+ x y)))))
(take 10 (fibPoslLazy 1 1));(1 1 2 3 5 8 13 21 34 55)


(defn mymax [x & y] (if (= y nil) 
                      x 
                      (if (> x (first y)) 
                        (apply mymax x (next y)) 
                        (apply mymax (first y) (next y)))))
;lepsi...
(defn mymax2 [& xs] (reduce #(if (> % %2) % %2) xs))

(mymax 1 10 20 30 4 100 6 ); 100

(defn jenVelke [s] (apply str (for [i (range 0 (count s))](let [z (.charAt s i)] (if (Character/isUpperCase z) z nil)))))

(defn jenVelke2 [s] (reduce str (filter #(Character/isUpperCase %) s)))

(count "ahoj")
(range 1 (count "ahoj jirko"))
(subs "ahoj jirko" 5 (inc 5))

(jenVelke "AHoJ123jijijiAAABKoneC")   ;"AHJAAABKC" 

;v seznamu vse dvakrat!
(defn zdvojeni [a] (list a a ))
(defn spoj [x y]  (conj (conj x (first y)) (last y)))
(reverse (reduce spoj (map zdvojeni [1 4 5 8 10])))
(reverse (reduce #(conj (conj %1 (first %2)) (last %2)) (map #(list %1 %1) [1 4 5 8 10])))
;lepe...
(defn zdvojeni2[a] (reduce #(conj %1 %2 %2) [] a))
(zdvojeni2 [1  4  8 9])
;nebo taky lepe...
(defn f [x] (if (empty? x) x (conj (f (rest x)) (first x) (first x)    ) ))
(f [1  4  8 9])
; nebo taky...
(#(interleave % %) [1 2 3 4])

;muj range.. pomocí lazy
(#(take (- %2 %1) ((fn rang [x] (lazy-seq (cons x (rang (inc x))))) %1)) 10 15)

;pomocí map-indexed
(fn [a b]
  (map-indexed + (repeat (- b a) a)))
;pomocí iterate take-while
(fn [from to]
  (take-while #(< % to)
              (iterate inc from)))

(dotimes [n 3]
  (print "ahoj")
  (print n)
  )

(defn dejPrvky [x] ((if (counted? (first x))
                    ( (dejPrvky (first x))

(def sek  '(([1] 2) 3 [4 [5 6]])) 

(def sek2 (concat (first sek) (rest sek)))

(def sek3 (concat (first sek2) (rest sek2)))

(concat (list (first sek3)) (rest sek3))
                       
;moje vlastni odstraneni vnorenych seznamu, definovano pomoci anonymni funkce
(defn myFlatten [ceho] ((fn procisti [vysledek co] (if (empty? co) 
                                    vysledek
                                    (let [akt (last co)]
                                      (if (sequential? akt)
                                          (procisti  (concat (procisti [] akt) vysledek) (butlast co))
                                          (procisti  (into (vector akt) vysledek ) (butlast co))
                                      )))) [] ceho))

;lepe...
((fn flatten [x]
  (if (coll? x)
    (mapcat flatten x)
    [x])) [[3 0 5 [7 [8]]] 5 [[6]]])


;testy myFlatten
(myFlatten [[3 [7 [8]]] 5 [[6]]])
(myFlatten [3 [7]])
(myFlatten [3 7 21  [15] 10 12] )
(myFlatten [7 [8]] )
(myFlatten [3 [8] 7] )
(myFlatten [[8] 7] )
(myFlatten [2 3 10 [8] 7] )
(myFlatten ['(1 [2 [8] 9]) 3 [4 [5 6]]])
(myFlatten ['(1 [2 9])] )
(myFlatten [[[1 [2 9]]]] )

;vlastni interleave 
(#(flatten (reverse (zipmap %1 %2))) [1 2 3] [:a :b :c])
;'(1 :a 2 :b 3 :c)
;lepe...
(mapcat list  [1 2 3] [:a :b :c])

;factorial...
((fn [to]
  (reduce * (take-while #(<= % to)
              (iterate inc 1)))) 0)
;factorial lepe...
#(->> (iterate inc 1)
      (take %)
      (reduce *))
;nebo..
#(reduce * (range 1 (inc %)))

;vyhodit nasobne vyskyty
(defn f [res x] (if (empty? x)
                     res
                     (f (into res (vector (first x)) )(drop-while #(= (first x) %) x))
                     ))
;lepe...
#(map first (partition-by identity %))

                    
                
(f [] [1 1 2 3 3 2 2 3])
(partition-by even? [1 1 2 3 3 2 2 3])
(f [] [[1 2] [1 2] [3 4] [1 2]])
(= (apply str (f [] "Leeeeeerrroyyy")) "Leroy")


;tisk systemovych promennych
(doseq [var (sort (keys (System/getenv)))]
  (printf "%s=%s\n" var (get (System/getenv) var)))

(defn mujOpakovac [sezn n] (mapcat #(repeat n %) sezn))
;(= (mujOpakovac [1 2 3] 2) '(1 1 2 2 3 3))
;(= (mujOpakovac [4 5 6] 1) '(4 5 6))
;(= (mujOpakovac [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
;(= (mujOpakovac [44 33] 2) [44 44 33 33])

