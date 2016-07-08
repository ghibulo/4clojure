(ns tretipokusy.core
  (:gen-class))
(require '[clojure.string :as str])

(map-indexed (fn [idx itm] [idx itm]) [1 2 3 4 5] )
(defn ocislovat [x s]  (map-indexed (fn [idx itm] [(mod (- idx x) (count s)) itm]) s ))
(defn seradit [co] (sort-by #(first %) co))
(defn vycistit [co] (map #(last %) co))
(vycistit (seradit   (ocislovat 2 '[a b c d e])))
(defn rotace[x s] ((comp vycistit seradit (partial ocislovat x)) s))
;(rotace -2 '[a b c d e])
;predelano pro 4clojure...
(defn rot [a b] 
(letfn [
(numerigi [x s]  (map-indexed (fn [idx itm] [(mod (- idx x) (count s)) itm]) s ))
(ordoni [co] (sort-by #(first %) co))
(purigi [co] (map #(last %) co))]
((comp purigi ordoni (partial numerigi a)) b ))) 

;alternativa
(fn [n xs]
  (let [i (mod n (count xs))]
    (concat (drop i xs) (take i xs)))) ;staci prohodit (pred prvkem) a (za prvkem) ;-)

;rozdat karty na prislusny pocet skupin
(defn numerigi [a b] (map vector (cycle (range b)) a ))
(defn grupigi [c] (apply merge-with concat  (map #(hash-map (first %) [(last %)]) c))) 
(vals (grupigi (numerigi [1 2 3 4 5 6] 2)))
;predelano pro 4clojure...
(defn intleave [x y] 
(letfn [
(numerigi [a b] (map vector (cycle (range b)) a ))
(grupigi [c] (apply merge-with concat  (map #(hash-map (first %) [(last %)]) c))) ]
(-> x (numerigi y) grupigi vals))) 
;(intleave [1 2 3 4 5 6] 3)
;alternativy...
(fn [xs n] (apply map vector (partition n xs)))
(fn [s x] 
  (reduce (partial map conj) 
          (repeat x []) 
          (partition x s)))


;rozdelit do mnoziny seznamu podle typu
(#(set (vals (group-by type %))) [:a "foo"  "bar" :b])

(defn occur [a] (letfn [
(grupigi[x] (partition-by identity (sort x)))]
(reduce into {} (map #(hash-map(first %) (count %)) (grupigi a)))))
;(occur [1 3 4 2 3 1 4 2])
;alternativa
  #(let [m (group-by identity %)] (zipmap (keys m) (map count (vals m))))

;vybrat duplikace a zachovat poradi
(defn almeti [x y] (if (.contains x y) x (conj x y)))
(reduce almeti [] [:a :a :b :b :c :c])
;alternativa
;(partial reduce (fn [c v] (if (get (set c) v) c (conj c v))) [])
;#(sort-by (fn [i] (.indexOf % i)) (map first (group-by identity %))) ... seradit podle puvodniho indexu
;(almeti [1 2 3] 4)
;(some #(= 5 %) '(1 2 3)) ... misto .contains


(defn kunmetiFun [x  y] (fn [ & argj]   (x (apply y argj)) ))
(defn reduceKunmetiF [& z] (reduce kunmetiFun z)) 


;(defn reduceKunmetiF [& z] (reduce (fn [x  y] (fn [ & argj]   (x (apply y argj)) )) z)) 
;((reduceKunmetiF inc inc inc inc (partial * 2)) 10)
;alternativa
;(fn [& fs]
;  (fn [& xs]                                                    ;vrati funkci, ktera... ->
;     (first (reduce #(vector (apply %2 %1)) xs (reverse fs))))) ;aplikuj prvni fci na vektor xs, z vysledku vektor a znovu reduce...
                      

;rozdelit (fakigi) posloupnost po danem poctu #54 
(defn elekti [i p] (filter #(= (mod (.indexOf p %) i) 0) p))      
(defn fakigi [x y] (apply map vector (map (partial elekti x) (take x (iterate rest y)))))
(fakigi 3 (range 9))
;(def posl (take 5 (iterate rest (range 9))))  ;-> ((0 1 2 3 4 5 6 7 8) (1 2 3 4 5 6 7 8) (2 3 4 5 6 7 8) (3 4 5 6 7 8) (4 5 6 7 8))                   
;rekurzni alternativa...
(fn f [n l] (if (< (count l) n)
            (list)
            (cons (take n l) (f n (drop n l)))
          )
)


(apply count  ["hello"])
(apply + [2 3 5 1 6 4])
(apply :a  [{:a 2, :b 4, :c 6, :d 8 :e 10}])

;aplikuje postupne sadu funkci na jednu sadu parametru a vysledkem je vektor #59
(defn mapaFci [& f] (fn [ & argj] (map #(apply % argj) f)))
;((mapaFci + max min) 2 3 5 1 6 4) ;... -> [21 6 1]
;alternativa pomoci for...
(fn [& fns]
  (fn [& args]
    (for [f fns]
      (apply f args))))

;---------------------------
;seradit slova v retezci #70
(defn vicigi [x] (sort-by clojure.string/lower-case (clojure.string/split (.replaceAll x "[.!?]" "") #" ")))
;(fn [x] (sort-by #(.toLowerCase %) (.split (.replaceAll x "[^a-zA-Z ]" "") " "))) ;-> neni treba tak dlouhe nazvy fci
;(.replaceAll "a++ěhoj" "[^a-zA-Z0-9]" "") ;...-> vyhodit nechtene znaky

;moje Eratosthenovo síto pomocí rekurze, vstupem je pocet zadanych prvocisel 
(defn ErastS  ([nom] (take nom (trampoline ErastS [] (let [lnNom (Math/log nom) lnlnNom (Math/log lnNom)] (range 2 (+ 3 (* nom (+ lnNom lnlnNom)))))))) 
              ([prim rang] (let [fe (first rang) ]  (if (< (* 2 fe) (last rang)) 
                                                           #(ErastS (conj prim (first rang)) (remove (fn [t] (.contains (range fe (last rang) fe) t)) rang))
                                                                  (concat prim rang)))) )

(last (ErastS 500)) ;-> 500.prvocislo = 3571

;moje Eratosthenovo síto pomocí mapy nasobku k odebrani - silena verze (znovu a znovu odebiram uz odebrane nasobky O:)
(defn ers [nom] (let [lnNom (Math/log nom) lnlnNom (Math/log lnNom) maxo (+ 3 (* nom (+ lnNom lnlnNom)))]
                   (take nom (reduce (fn [q w]  (remove #(.contains w %) q)) (range 2 maxo) 
                                      (map #(rest (range % maxo %))  (range 2 (/ maxo 2)))))))

;(ers 5)

;elegance bez Erast.sita...
(last ((fn prime [n] (take n 
  (filter (fn [x] (every? #(not= 0 (mod x %)) (range 2 (inc (/ x 3))))) (iterate inc 2)))) 5))
;---------------------------

;dokonale ctverce #74
(defn pfSq [a] (reduce #(str %1 "," %2) (filter (fn [x] (let [odm (Math/sqrt x) ro (int odm)] (= 0.0 (- odm ro)))) (map #(Integer/parseInt %) (clojure.string/split   a #",")))))
;(= (pfSq "4,5,6,7,8,9") "4,9")
;alternativy
(fn [s] (clojure.string/join ","
  (filter #(= 0.0 (mod (Math/sqrt %) 1)) 
    (read-string (str "[" s "]")) )) )

(fn [strs]
  (apply str
    (interpose ","
      (filter
        #(= 0.0 (mod (Math/sqrt %) 1))
        (map #(Integer/parseInt %) (.split strs ","))))))

;regulární výrazy
(def http-regexp
    #"https?:\/\/(?:www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)")
(re-find http-regexp "Podivejte se na http://root.cz?r=5&a=8!")
(defn create-links
    [string]
    (let [href (re-find http-regexp string)]
        (if href
            (str/replace string http-regexp (str "<a href='" (first href) "'>" (first href) "</a>"))
            string)))

(create-links "Podivejte se na http://www.root.cz?a=1!")

(str/split-lines (slurp "/etc/passwd"))
(for [line (str/split-lines (slurp "/etc/passwd"))]
                            (first (str/split line #":")))

(sort (map #(first (str/split % #":")) (str/split-lines (slurp "/etc/passwd"))))


;rozeznavat vector/map/set/list bez class/type/sequential?/getClass/...
(defn kiu? [x] (let [z (vector :e1 :e2) tmp0 (conj x (vector :e2 :e1)) tmp1 (conj tmp0 z) ] 
                     (if (= (concat tmp1 [z]) (conj tmp1 z)) 
                         :vector 
                         (if (= tmp1 (conj tmp1  z)) 
                           (if (= (tmp1 :e1) :e2)
                               :map
                               :set)
                           :list))))


;(kiu? {:a 1, :b 2})
;(kiu? [1 2 3])
;(kiu? '()) 
;(kiu? #{10 (rand-int 5)}) 
;(map kiu? [{} #{} [] ()])
;alternativa pres konverzi do string:
;(def kiu? (comp {\# :set \{ :map \[ :vector \c :list} first str))

------------

;Perfect Numbers- soucet delitelu je roven cislu. #80
(defn pn [x] (= x (apply +(filter #(= (mod x %) 0) (range 1 (inc (/ x 2)))))))
;(pn 8128)
-------

;rozdelit na mnoziny anagramu #77
;(= (frequencies "aoaj") (frequencies "ahoja"))
((defn anagram [x] 
 (disj (set (map #(if (= (count %) 1) nil (into #{} %)) (vals (reduce #(assoc %1 (frequencies %2) (conj  (%1 (frequencies %2))  %2)) {} x)))) nil))
["meat" "mat" "team" "mate" "eat"])

;pomoci group-by sort...
(defn anagram2 [w]
  (->> w
       (group-by sort)
       vals
       (filter #(> (count %) 1))
       (map #(apply hash-set %))
       (apply hash-set)))

;(anagram2 ["veer" "lake" "item" "kale" "mite" "ever"])
;(group-by sort  ["veer" "lake" "item" "kale" "mite" "ever"])


;reduce s mezivysledky #60
(defn myreduction 
  ([x y]    (myreduction x (first y) (rest y)))
  ([x y z]  (if (empty? z) (vector y) (cons  y (lazy-seq (myreduction x (x y (first z)) (rest z))) )))  )
;(= (take 5 (myreduction + (range))) [0 1 3 6 10])
;(myreduction conj [1] [2 3 4])
;(last (myreduction * 2 [3 4 5]))

;-------------------------------------------------
;merge with function #69
;funkce a libovolnej pocet map, ktere se zupdatuji do jedne, na hodnoty stejnych klicu se pouzije funkce

;update jedne mapy na zaklade pridane dvojice [klic hodnota]
((defn upd [f u d] (let [d1 (first d)]
                (if (nil? (u d1)) (apply assoc u d) (assoc u d1 (f (u d1) (last d)) )))) + {1 2 4 3} [1 5]  )
;jako upd, jen dvojice [klic hodnota] bere z druhe mapy
(defn mapop [fm um dm]  (reduce #(do (println %1 %2)(upd fm %1 %2)) um dm)) 
;(mapop + {:a 2, :b 3, :c 4} {:x 2 :c 8 :y 5 :a 6}) ;-> {:x 2, :y 5, :c 12, :b 3, :a 8} 

;konecna verze na libovolny pocet map pomoci reduce
((fn mergeMapF [x & y] (reduce #(mapop x %1 %2) y)) * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})

;konecna verze na libovolny pocet map pomoci reduce
((fn [x & y] (reduce (fn [a b] (reduce (fn [e f]  (let [f1 (first f)] (if (nil? (e f1)) (apply assoc e f) (assoc e f1 (x (e f1) (last f)) )))) a b) ) y)) * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})

;alternativa
(fn [f & l]
  (let [g (group-by first (apply concat (map #(apply list %) l)))]
    (zipmap (keys g) (map #(reduce f (map second %)) (vals g)))))

;-------------------------------------------------
;intoCamelCase #102
(defn intoCamelCase [x] (reduce str (conj  (map clojure.string/capitalize (rest (clojure.string/split x #"-" ))) (first (clojure.string/split x #"-" )))))
;(intoCamelCase "multi-word-key") ;-> "multiWordKey"
;alternativa
(fn [s] (clojure.string/replace s #"-(\w)" #(str (.toUpperCase (% 1)))))
;-------------------------------------------------

;provest funkci nad vsema hodnotama v map
(defn update-values [x f] (into {}  (mapcat #(hash-map (first %) (f (last %))) x)))
(update-values {:a 1 :b 2 :c 3} inc)
(update-values {:a 1 :b 2 :c 3} (partial + 10))
(update-values {:a {:z 1} :b {:z 1} :c {:z 1}} (partial #(dissoc % :z)))

;-------------------------------------------------
;
(defn mygcd [x y] (let [zb (mod x y)] (if (= zb 0) y (mygcd y zb))))
(fn mygcd [a b] (let [zb (mod a b)] (if (= zb 0) b (mygcd b zb))))
(mygcd 10 15)

;pocet nesoudelnych cisel mensi nez x Euler's Totient Function #75
(defn etf [x] (if (= x 1) 1 (count (filter #(= % 1) (map (partial (fn mygcd [a b] (let [zb (mod a b)] (if (= zb 0) b (mygcd b zb)))) x) (range 1 x))))))
(etf 10)


;#86 ... najdi stastne cislo (opakovane vypocitavat soucet druhych mocnin cifer, dokud nevyjde 1 (stastne)
;nebo se to nezacykli (smutne) ... vzal jsem jen prvnich 50 a podival se esli je uz jedna
(defn sumdigits [x] (apply + (map #(Math/pow (- (int %) 48) 2) (vec (str (int x))))))
(defn hn[c] (if (= 1 (int (last (take 50 (iterate sumdigits c))))) true false))
;pro 4clojure:
(letfn [(sumdigits [x] (apply + (map #(Math/pow (- (int %) 48) 2) (vec (str (int x))))))]
(fn hn[c] (if (= 1 (int (last (take 50 (iterate sumdigits c))))) true false)))
;(sumdigits  49.0)
;(hn 986543210 )
;spravne by se melo resit rekurentne:
;pocita dalsi, dokud novy vypocet neni jiz v seznamu videnych, nebo nevyjde 1
(fn happy?
  ([n] (happy? [] n))
  ([seen n]
    (cond
      (= 1 n)
        true
      (contains? seen n)
        false
      true
        (recur (conj seen n)
          (->> n
            (str)
            (map #(- (int %) (int \0)))
            (map #(* % %))
            (reduce +) )))))


;-------------------------------------------------



;vlastni trampolina...
(defn mytramp [x y] (let [f (atom (x y))] (do (println "f=" f) (while (fn? @f) (reset! f (@f))) @f))) 

(= (letfn [(triple [x] #(sub-two (* 3 x)))
          (sub-two [x] #(stop?(- x 2)))
          (stop? [x] (if (> x 50) x #(triple x)))]
    (mytramp triple 2))
  82)

;alternativa...
(fn [& as]
  (let [r (apply (first as) (rest as))]
    (if (fn? r) (recur [r]) r)))

;-------------------------------------------------

;Power Set #85 ... mnozina vsech podmnozin

(ps #{1 :a}) ;... ->#{#{1 :a} #{:a} #{} #{1}} 

(defn aldoniEl [e aa]  (set (for [i  aa ] (conj  i e) ))  )
;(aldoniEl 10 #{#{1 :a} #{:a} #{} #{1}})

(defn ps [s] (cond
                  (empty? s) #{#{}}
                  true (clojure.set/union (aldoniEl (first s) (ps (rest s))) (ps (rest s)))))


;(aldoniEl [e aa]  (set (for [i  aa ] (conj  i e) ))  )
;(aldoniEl 10 #{#{1 :a} #{:a} #{} #{1}}) ;->... #{#{1 10 :a} #{10} #{10 :a} #{1 10}} 
(defn ps [s] (cond
                  (empty? s) #{#{}}
                  true (clojure.set/union (aldoniEl (first s) (ps (rest s))) (ps (rest s)))))
;alternativa
(fn f [s]
  (if (empty? s)
    #{#{}}
    (set (concat (map #(conj % (first s)) (f (next s))) (f (next s))))))

;-------------------------------------------------

;The Balance of N #115 ... ciferne soumerne cislo 
(defn sim [x] (let [duono (unchecked-divide-int (count (str x)) 2) 
                    listo (map #(- (int %) 48)  (str x) ) ] ;stacilo by (map int (str x)) - scitat muzu i ascii kody
                (= (apply +(take duono listo)) (apply + (take duono (reverse listo))))))
(quot 5 2)
