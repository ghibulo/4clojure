(ns DruhePokusy.core 
  (:require [clojure.set :as cs]))

;prolozeni prvnim prvkem (interpose)
;#(interleave (repeat %1) %2)
(= (#(rest (interleave (repeat %1) %2)) 0 [1 2 3]) [1 0 2 0 3])

(#(map (fn  [x y] (if (= x y) x [x :posledni])) %1 (rest %1)) [0 1 2 1 1 1 3 3 4])
(partition-by identity [0 1 2 1 1 1 3 3 4])

;vyhodit kazdy n-ty...
((fn [x y] (filter #(not (nil? %)) (map-indexed #(if (= (mod (inc %1) y) 0) nil %2) x))) [1 2 3 4 5 6 7 8 9 10] 4)
;lepe...
(#(apply concat (partition-all (dec %2) %2 %1)) [1 2 3 4 5 6] 4)
;nebo
((fn [coll n] (keep-indexed #(if (not= (mod (inc %1) n) 0) %2) coll)) [1 2 3 4 5 6] 4)

(#(list (take %1 %2) (drop %1 %2)) 3 [2 4 6 7 9 10 13 15])
;lepe
((juxt take drop) 3 [2 4 6 7 9 10 13 15])

;same true -> false, jinak jako or
((fn myf [ & a ] (if (= true (reduce #(and %1 %2) a)) false (reduce #(or %1 %2) a))) false true false )


;slovnik ze dvou seznamu
(apply assoc {} (interleave [:fruit :color :temp] ["grape" "red" "hot"]))
(#(apply assoc {} (interleave %1 %2)) [:fruit :color :temp] ["grape" "red" "hot"])
(#(map (fn [x y] {x y}) %1 %2) [:fruit :color :temp] ["grape" "red" "hot"])

;muj euklid algoritmus
(defn mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb))))
(mygcd 120 69)

;muj prunik
((fn [x y] (set (filter #(not (nil? (x %))) y))) #{0 1 2 3} #{2 3 4 5})
;lepe...
((comp set filter)  #{0 1 2 3} #{2 3 4 5}) ;-> (set (filter #{0 1 2 3} #{2 3 4 5}))
;alias... -> (#(-> (filter %1 %2) set) #{0 1 2 3} #{2 3 4 5})


(defn fibPoslLazy [x y] (lazy-seq (cons x (fibPoslLazy y (+ x y)))))
(take 10 (fibPoslLazy 1 1));(1 1 2 3 5 8 13 21 34 55)


;operaci (%1) opakuj na prvním prvku (%2)
(defn lazySeq [x y] (lazy-seq (cons y (lazySeq x (x y)))))
(take 5 (lazySeq #(* 2 %) 1))

;nebo...
(fn my-iterate [f x] (->> (my-iterate f (f x)) (cons x) lazy-seq))

;closure
(defn prictiNeco [co] (fn [x] (+ co x)))
(defn naNekolikatou [jakou] (fn [x] (Math/pow jakou x)))
(def pricti5 (prictiNeco 5))
(pricti5 10)
((naNekolikatou 2) 3)
(((fn [jakou] (fn [x] (Math/pow x jakou))) 2) 16)

(( (fn [y] (fn [x] (Math/pow x y))) 2) 3)
;lepe... (( partial #(Math/pow %2 %1) 2) 3)

(= 256 (( (fn [y] (fn [x] (int (Math/pow x y)))) 2) 16))

;rozklad vysledku soucinu na cifry
(map str (Integer/toString 10))
((fn [x y] (map #(- (int %) 48)(Integer/toString (* x y)))) 10 20)
(= (#(vec (map #(- (int %) 48) (Integer/toString (* %1 %2))))  1 1) [1])

;kartezsky soucin
((fn [x y] (let [a (mapcat #(repeat (count y) %) x) b (->> (vec y) (repeat (count x)) (apply concat))] (map  vector a b ))) #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;lepe...
((fn [xs ys]
  (set
    (mapcat
      (fn [i] (map #(list i %) ys))
      xs))) #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;pomocne zkousky
(->> (vec #{"♠" "♥" "♦" "♣"}) (repeat 5) (apply concat))
(map vector [1 2 5] [2 65 5])



(merge {"Apple" "Mac" "Microsoft" "Windows"} {"Apple" 2})

(into {} [[:a 1] [:b 2] [:c 3] [:a 5]])

((fn [m [k v]] (let [h (get m k)] (if (= h nil) 
                                       (assoc m k (list v))
                                       (assoc m k (conj h v))))) 
 {:a '("ahoj") :b "cau"} [:a "hallo"])


(reduce (fn [m [k v]] (let [h (get m k)] (if (= h nil) 
                                         (assoc m k (list v))
                                         (assoc m k (conj h v))))) 
 [{} [:a "ahoj"] [:b "cau"] [:a "hallo"]])




;podle výsledku funkce rozgrupovat cleny seznamu
((fn [f s]  (reduce (fn [m [k v]] (let [h (get m k [])] (assoc m k (conj h v))))
            (cons {} (map vector (map f s) s)))) #(> % 5) [1 3 6 8])

;elegantneji
((fn [f s]
  (apply merge-with concat (map #(hash-map (f %1) [%1]) s)))
                                                            #(> % 5) [1 3 6 8])



;symetricky rozdil mnozin
(#(clojure.set/difference (clojure.set/union %1 %2) (clojure.set/intersection %1 %2)) #{1 2 3 4 5 6} #{1 3 5 7})

;prevod do dvojkove
((fn [bin] (reduce + (map (fn[[a b]] (if (= b \1) a 0)) (map-indexed (fn [idx itm] [(Math/pow 2 (- (dec (count bin)) idx)) itm]) bin)))) "111")
;lepe
((fn [s] (reduce + (map-indexed #(if (= %2 \0) 0 (bit-shift-left 1 %1)) (reverse s)))) "101101")
;nebo
((fn [s] (read-string (str "2r" s))) "1011")


;soucet soucinu
(#(reduce + (map * %1 %2 )) [1 2 3] [4 5 6])
;nebo
(#(->> (map * %1 %2) (reduce +))[1 2 3] [4 5 6])

;moje infix kalkulacka
((fn myinf [a b c & d] (if (nil? d) (b a c) (apply myinf (cons (b a c) d)))) 1 + 2 - 3 + 10 / 5)
;alternativa...
((fn [& xs]
  (reduce #((first %2) %1 (last %2)) (first xs) (partition 2 (rest xs)))) 1 + 2 - 3 + 10 / 5)


;moje kombinacni cislo
(defn C [n k] (/ (reduce * (range (inc (- n k)) (inc n))) (reduce * (range 1 (inc k)))))
;(C 0 0)

;radek pascal trojuhelnika
(defn PasTr [r]  (map #(/ (reduce * (range (inc (- (dec r) %)) r)) (reduce * (range 1 (inc %)))) (take r (iterate inc 0))))
(PasTr 11)
;alternativa
(
 (fn [r]
  (reduce #(cons
    (* (first %1)
       (/ (- r %2) %2))
  %1) [1] (range 1 r)))
11)

;muj vlastni map
(defn myMap [f [x & y]] (lazy-seq
                          (if (empty? y) [(f x)] (cons (f x) (myMap f y)))))
;(myMap dec [2 3 4 5 6])
;(take 10 (myMap dec (range)))
;((fn myMap [f [x & y]] (if (empty? y) [(f x)]  (concat [(f x)] (myMap f y) ))) (fn [_] nil) (range 10))
;(= (repeat 10 nil)
;   ((fn myMap [f [x & y]] (if (empty? y) [(f x)]  (concat [(f x)] (myMap f y) ))) (fn [_] nil) (range 10)))
;(= [1000000 1000001]
;   (->> (map inc (range))
;        (drop (dec 1000000))
;        (take 2)))


;je to binarni strom?
(defn isTree [[r l p & zb :as vst]] (do (println "vst=" (count vst) "r=" r "l=" l "p=" p "zb=" (empty? zb)) 
                        (if (= (count vst) 3)
                           (cond
                             (coll? r) false
                             (nil? r) false
                             (and (not (coll? l)) (not (nil? l)) ) false
                             (and (not (coll? p)) (not (nil? p)) ) false
                             (and (not (coll? r)) (nil? l) (nil? p)) true
                             (and  (coll? l) (nil? p)) (isTree l)
                             (and  (nil? l) (coll? p)) (isTree p)
                             :else (and (isTree l) (isTree p)))
                           false)))

;(isTree [1 nil [2 [3 nil nil] [4 nil nil]]])
;(isTree [1 [2 nil nil] [3 nil nil] [4 nil nil]])
;(tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
;(isTree [1 [2 [3 [4 false nil] nil] nil] nil])
;(isTree '(:a nil ()))
;alternativa
(defn tree? [t]
  (if (= 3 (count t))
    (every? #(or (nil? %) (and (sequential? %) (tree? %))) (rest t))
    false))

;je strom symetricky?
(defn eqTree? [[v1 l1 p1] [v2 l2 p2]] 
  (cond
    (or (nil? v1) (nil? v2)) (and (nil? v1) (nil? v2))
    :else (and (= v1 v2) (eqTree? l1 p2)  (eqTree? p1 l2))))

(defn treeSym? [[tv tl tp]] (eqTree? tl tp))
;(treeSym? '(:a (:b nil nil) (:b nil nil)))
;(treeSym? '(:a (:b nil nil) nil))
;(treeSym? '(:a (:b nil nil) (:c nil nil)))

;alternativa :-)
#(let [t (fn t [[v l r]] [v (if r (t r)) (if l (t l))]);otoci pravou cast stromu
       [_ l r] %]
    (= l (t r)));pak musi platit rovnost



;kolik cisel ze seznamu ma mensi hodnotu nez je soucet druhych mocnin jejich cifer?
((fn [s] (count (filter (fn [x] (< x (reduce + (map #(Math/pow (- (int %) 48) 2)(Integer/toString x ))))) s ))) (range 1000))
;alternativa...
(fn [numCol]
   (->> numCol
     (filter #(->> %1 ;take a number
               str ;make it a string
               seq ;which is a sequence of chars
               (map str) ;make it to a sequence of String for conversion
               (map (fn [y] (Integer. y))) ;convert Strings to longs
               (map (fn [y] (* y y))) ;map those to sequence of squares
               (reduce +) ;sum it up
               (< %1))) ;is the number smaller than the sum?
     count))


(defn cards [x] (let [trans {\D :diamond \H :heart \C :club \S :spades \A 12 \K 11 \Q 10 \J 9 \T 8 \9 7 \8 6 \7 5 \6 4 \5 3 \4 2 \3 1 \2 0}] 
                  {:suit (trans (first x)) :rank (trans (last x))}))
(cards "CA")
(= (range 13) (map (comp :rank cards str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))
;alternativa...
(fn [[s r]]
    { :suit ({\S :spade \H :heart \D :diamond \C :club} s)
      :rank ((zipmap "23456789TJQKA" (range)) r) })


;nasobeni neomezeneho poctu argumentu
(defn mymulti [x & y] (do (println "x-" x "y-" y)(if (empty? y) x (* x (apply mymulti y)))))
;(mymulti 1/3 2/5)

;--------
;nejmensi spolecny nasobek
(defn mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb))))
(defn mylcm [x & r] (do (println x r) (if (empty? r) x (apply mylcm (conj  (rest r) (/ (* x (first r)) (mygcd x (first r))))) )))

(mylcm 7 5/7 2 3/5 )
(fn mylcm [x & r] (letfn [(mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb))))] 
                    (if (empty? r) x (apply mylcm (conj  (rest r) (/ (* x (first r)) (mygcd x (first r))))) )))
;alternativa... misto rekurze reduce:
;(fn lcm [& args]
;  (letfn [
;    (gcd [a b] 
;      (if (< a b) (recur b a)
;        (if (zero? b) a (recur b (mod a b)) ))) ]
;     (reduce #( / ( * % %2) (gcd % %2) ) args  )))

;dalsi alternativy 
(fn [& ns]
    (reduce
      (fn [x y] (first (drop-while #(pos? (rem % x)) (iterate (partial + y) y))))
      ns))


(fn [& xs]
  (/ (apply * xs)
    (reduce #(if (zero? %2) % (recur %2 (mod % %2))) xs))) ;eukl alg?
;--------

;lazyseqence na pascaluv trojuhelnik, prvni radek zadany libovolne
(defn dalsRadekPascal [x] (let [z (partition 2 (concat [0] (mapcat #(list %1 %1) x) [0] ))] (map #(+' (first %) (last %)) z)))
;plus s apostrofem -> biginteger
(defn lazyPascal [x] (lazy-seq (cons x (lazyPascal (dalsRadekPascal x))) ))
(nth (lazyPascal [2 4 2]) 100)

;alternativa
(fn [row]
  (let [next-row #(map +' (concat [0] %) (concat % [0]))] ;secist dva seznamy vzajemne posunute, drahy Watsone :-)
    (iterate next-row row)))

(defn extMap [x m] (clojure.set/rename-keys m (apply hash-map (mapcat #(vector % (vector x %)) (keys m)))))
(defn flatMap [x] (reduce merge (map #(extMap (first %) (last %)) x)))
;(extMap :x {:a 1, :b 2})
;(def mapa '{a {p 1, q 2} b {m 3, n 4}})
;((flatMap mapa) '[b m])

;alternativa
(fn [m]
  (->>
   (for [[k1 v1] m
   	     [k2 v2] v1]
     {[k1 k2] v2})
   (apply merge)))

;ze systemu mnozin zjisti, jestli nektere dve nemaji prunik
(defn prunik? [mn] (empty? (for [i mn :let [ost (apply clojure.set/union (disj pr i)) inter (clojure.set/intersection i ost)] :when (not (empty? inter))] inter)))
(def pr #{#{:a :b :c :d :e} #{:a :b :c :d} #{:a :b :c} #{:a :b} #{:a}})
;(prunik? pr)
;alternativa...
(fn [s] (let [l (reduce into '() s)] (= (count l) (count (set l)))));kdyz se nic neopakuje, musi byt seznamy stejne dlouhe jako mnoziny

;slozeni dvou funkci
((comp (partial * 2) (partial + 1)) 1)
;otoceni poradi operatoru pomoci partial
((partial #(%1 %3 %2) -) 4 5)




(def person {
  :name "Mark Volkmann"
  :address {
    :street "644 Glen Summit"
    :city "St. Charles"
    :state "Missouri"
    :zip 63304}
  :employer {
    :name "Object Computing, Inc."
    :address {
      :street "12140 Woodcrest Executive Drive, Suite 250"
      :city "Creve Coeur"
      :state "Missouri"
      :zip 63141}}})

(get-in person [:employer :address :city])
(-> person :employer :address :city) ; explained below
(reduce get person [:employer :address :city]) ; explained below

;---------------------
;black and white koule, odeberu jednu jedne barvy a pridam misto toho kouli druhe barvy
;zopakuji n-krat
;jaka je pravdepodobnost, ze vytahnu bilou?

(def startPr [{:p 1 :b 2 :w 1}])

(defn zpracujPozici [poz]
(letfn 
[(zprPozB[poz vysl] (if (> (:b poz) 0) 
                            (zprPozW poz [{:p (* (:p poz) (/ (:b poz) (+ (:b poz) (:w poz)))) :b (dec (:b poz)) :w (inc (:w poz))}])
                            (zprPozW poz vysl)))

(zprPozW[poz vysl] (if (> (:w poz) 0) 
                            (cons {:p (* (:p poz) (/ (:w poz) (+ (:b poz) (:w poz))))  :b (inc (:b poz)) :w (dec (:w poz))} vysl)
                            vysl))]
(zprPozB poz [])))

(mapcat zpracujPozici (mapcat zpracujPozici [{:p 1 :b 2 :w 1}]))
(defn pocitejKoule [start n] (if (pos? n)
                               (pocitejKoule (mapcat zpracujPozici start) (dec n))
                               {:vysledek (apply + (map #(* (:p %) (/ (:w %) (+ (:b %) (:w %))))  start)) :pozice start} ))

(pocitejKoule startPr 1)
