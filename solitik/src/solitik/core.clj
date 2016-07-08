(ns solitik.core
  (:gen-class))


(def pole1 [[nil nil 1 1 1 nil nil]
            [nil 1   1 1 1 1   nil]
            [nil 1   1 0 1 1   nil]
            [nil 1   1 1 1 1   nil]
            [nil nil 1 1 1 nil nil]
          ])

(def pole2 [[nil nil 0 0 0 nil nil]
            [nil 0   1 0 1 0   nil]
            [nil 1   1 0 1 1   nil]
            [nil 0   0 0 1 0   nil]
            [nil nil 0 0 0 nil nil]
          ])


(def pole3 [[nil nil 0 0 0 0 nil]
           [nil 0   0 0 1 1   nil]
           [nil 0   0 0 0 0   nil]
           [nil 0   0 1 0 0   nil]
           [nil nil 0 0 0 nil nil]
          ])


(def tah {:odradka 1 :odsloupec 5 :kamradka 1 :kamsloupec 3})

(defn zmenaPrvku [seznam poradi hodnota] 
      (vec (concat (take poradi seznam) (vector hodnota) (drop (inc poradi) seznam))))
;(zmenaPrvku [nil 1   1 1 1 1   nil] 2 0) ;-> [nil 1 0 1 1 1 nil]

;podle tahu na pozici 'od' da nulu, na pozici 'kam' da jednicku
(defn aplikujTah [jaky pole] 
               (let [mezrad (/ (+ (jaky :kamradka) (jaky :odradka)) 2) mezsl (/ (+ (jaky :kamsloupec) (jaky :odsloupec)) 2)]
                (->> pole  (#(zmenaPrvku % (jaky :odradka) (zmenaPrvku (nth % (jaky :odradka)) (jaky :odsloupec) 0)))
                          (#(zmenaPrvku % (jaky :kamradka) (zmenaPrvku (nth % (jaky :kamradka)) (jaky :kamsloupec) 1)))
                          (#(zmenaPrvku % mezrad (zmenaPrvku (nth % mezrad) mezsl 0)))
                          )))
;(aplikujTah tah pole2)

(defn moznyTahZapad [seznam odkud]
  (let [sx (- (odkud :odsloupec) 2)
        sy (odkud :odradka)
        radka (nth seznam sy) 
        cilmisto (get radka sx)
        hopmisto (get radka (inc sx))]
       (if (and (= cilmisto 0) (= hopmisto 1))
         (assoc odkud :kamradka sy :kamsloupec sx) 
       ;;else 
         nil)))
;(moznyTahZapad pole2 {:odradka 2 :odsloupec 5});-> {:kamsloupec 3, :kamradka 2, :odsloupec 5, :odradka 2} 

(defn moznyTahVychod [seznam odkud]
  (let [sx (+ (odkud :odsloupec) 2)
        sy (odkud :odradka)
        radka (nth seznam sy) 
        cilmisto (get radka sx)
        hopmisto (get radka (dec sx))]
       (if (and (= cilmisto 0) (= hopmisto 1))
         (assoc odkud :kamradka sy :kamsloupec sx) 
       ;;else 
         nil)))
;(moznyTahVychod pole2 {:odradka 2 :odsloupec 1});-> {:kamsloupec 3, :kamradka 2, :odsloupec 5, :odradka 2} 

(defn moznyTahSever [seznam odkud]
  (let [sy (- (odkud :odradka) 2)
        sx (odkud :odsloupec)
        cilmisto (get (get seznam sy) sx)
        hopmisto (get (get seznam (inc sy)) sx)]
       (if (and (= cilmisto 0) (= hopmisto 1))
         (assoc odkud :kamradka sy :kamsloupec sx) 
       ;;else 
         nil)))
;(moznyTahSever pole {:odradka 4 :odsloupec 3});->{:kamsloupec 3, :kamradka 2, :odsloupec 3, :odradka 4} 

(defn moznyTahJih [seznam odkud]
  (let [sy (+ (odkud :odradka) 2)
        sx (odkud :odsloupec)
        cilmisto (get (get seznam sy) sx)
        hopmisto (get (get seznam (dec sy)) sx)]
       (if (and (= cilmisto 0) (= hopmisto 1))
         (assoc odkud :kamradka sy :kamsloupec sx) 
       ;;else 
         nil)))
;(moznyTahJih pole {:odradka 0 :odsloupec 3});->{:kamsloupec 3, :kamradka 2, :odsloupec 3, :odradka 4} 



(defn mozneTahyZPozice [seznam odkud]
  (remove nil? (map #(% seznam odkud) (list moznyTahZapad moznyTahVychod moznyTahSever moznyTahJih)))) ;seznam odkud do vsech funkci

(mozneTahyZPozice pole2 {:odradka 2 :odsloupec 4})
;-> ({:kamsloupec 5, :kamradka 0, :odsloupec 3, :odradka 0} {:kamsloupec 3, :kamradka 2, :odsloupec 3, :odradka 0})


;(map-indexed vector (map #(map-indexed vector %) pole))
;dostanu seznam prvku: [0 ([0 nil] [1 nil] [2 1] [3 1] [4 1] [5 0] [6 nil])]
;vytvorim funkci, ktera druhe jednicky nahradi y-souradnici (prvni prvek)

;------
;vybere dvojice s jednickou na konci a misto teto jednicky vlozi prvni cislo - tedy y-souradnici
(defn nahradDruhe [[cim radka]] (map #(list cim (first %)) (filter #(= (last %1) 1) radka)))
;(nahradDruhe  [0 '([0 nil] [1 nil] [2 1] [3 1] [4 1] [5 0] [6 nil])] );->((2 0) (3 0) (4 0)) 
;------

;kazdou jednicku v poli nahradi jejimi souradnicemi -> [odradka odsloupec]
(defn souradniceFigurekZPole [p] (reduce concat (map nahradDruhe  (map-indexed vector (map #(map-indexed vector %) p)))))
;(souradniceFigurekZPole pole2);->((1 3) (1 5) (3 5))
;-----

(nth [1 2 5 7] 3)
(partition  5 5 nil (range 1 20))
(mozneTahyZPostaveni pole3 2 1)
;z pole figurek vytvori seznam moznych tahu
(defn mozneTahyZPostaveni 
  ([postaveni ng o] (let [mt (mozneTahyZPostaveni postaveni)]
                      (if (empty? mt) '() (nth (partition ng ng nil mt) o '())))) ;rozdeleni pro concurent zpracovani
  ([postaveni] 
  (reduce concat (map #(mozneTahyZPozice postaveni {:odradka (first %) :odsloupec (second %)}) (souradniceFigurekZPole postaveni)))))

(mozneTahyZPostaveni pole5) ;-> ({:kamsloupec 3, :kamradka 3, :odradka 1, :odsloupec 3} {:kamsloupec 3, :kamradka 3, :odradka 3, :odsloupec 1} {:kamsloupec 3, :kamradka 3, :odradka 3, :odsloupec 5} {:kamsloupec 3, :kamradka 3, :odradka 5, :odsloupec 3}) 
(mozneTahyZPostaveni pole5 2 1) ;-> ({:kamsloupec 3, :kamradka 3, :odradka 3, :odsloupec 5} {:kamsloupec 3, :kamradka 3, :odradka 5, :odsloupec 3})
(mozneTahyZPostaveni pole5 2 0) ;-> ({:kamsloupec 3, :kamradka 3, :odradka 1, :odsloupec 3} {:kamsloupec 3, :kamradka 3, :odradka 3, :odsloupec 1}) 
(nth (partition 2 2 nil ({:kamsloupec 5, :kamradka 1, :odradka 1, :odsloupec 3} {:kamsloupec 2, :kamradka 1, :odradka 1, :odsloupec 4})) 1) 


;------

;je vyreseno?
(defn pocetFigurek [postaveni] (count (filter #(= 1 %) (reduce concat postaveni))))
(defn jeKonecHry? [postaveni]  (= (pocetFigurek postaveni) 1))
;(pocetFigurek pole2);->7
;(jeKonecHry? pole3);->true

(defn compute-across [func elements value]
  (if (empty? elements)
    value
    (recur func (rest elements) (func (first elements) value ))))

;nefunkcni
;(defn hledejReseni [postaveni reseni dalsipostaveni]  
  ;(do (println "postaveni>" postaveni "dalsipostaveni>" dalsipostaveni)
  ;(if (jeKonecHry? postaveni)
      ;reseni ;else
      ;(let [mozneTahy           (mozneTahyZPostaveni postaveni)]
        ;(if (empty? mozneTahy)
            ;;(do (println "moznetahy>" (mozneTahyZPostaveni postaveni) "z postaveni>" postaveni)
            ;(if (empty? dalsipostaveni)
              ;"nema reseni";else
              ;#(hledejReseni (first dalsipostaveni) reseni (rest dalsipostaveni)));)
            ;else
            ;(let [dalsimoznepostaveni (concat (map #(aplikujTah % postaveni) mozneTahy) dalsipostaveni)]
                 ;#(hledejReseni (first dalsimoznepostaveni) (conj reseni (first mozneTahy)) (rest dalsimoznepostaveni)))))));)

;(def mt [[nil nil 0 0 0 nil nil] [nil 0 1 1 0 0 nil] [nil 0 0 0 0 0 nil] [nil 0 0 0 0 0 nil] [nil nil 0 0 0 nil nil]])
;(mozneTahyZPostaveni mt)

;(first (into [] (concat (map #(aplikujTah % pole2) (mozneTahyZPostaveni pole2)) [3 4])))
(defn -main [& args] (trampoline hledejReseni pole2 [] []))


;(some #(when (= 3 %) %) [1 2 3 4 5])

(get {1 5 2 3} 2)
;struktura trebaResit -> {1 [postaveni [moznetahy]] 2 [postaveni [mozneTahy]] ... }

 (conj [] {:kamsloupec 4, :kamradka 3, :odradka 1, :odsloupec 4})


(hledejReseni1 pole4)
;(hledejReseni2 pole5)

;klasika jednoprocesorove
(defn hledejReseni2 [postaveni ]  
  (loop [post postaveni mozneTahy (mozneTahyZPostaveni post) reseni [] trebaResit {} uroven 0]
  (if (jeKonecHry? post)
      reseni;(do (println post "->" reseni) reseni) ;else
      (if (empty? mozneTahy)
        (if (= uroven 0) 
          (do (println "nema reseni") nil) 
          (let [predur (dec uroven) predTrebaResit (first (get trebaResit predur))] 
               (recur (first predTrebaResit)  (last predTrebaResit) (butlast reseni) (dissoc trebaResit predur) predur)))
        (let [dalsitah (first mozneTahy) zbTahy (rest mozneTahy) dalsipoz (aplikujTah dalsitah post)] (do (println "zbT:" zbTahy)
             (recur dalsipoz  (mozneTahyZPostaveni dalsipoz) (conj reseni dalsitah) 
                    (assoc-in trebaResit [uroven post] zbTahy) (inc uroven))))))))

(defn hledejReseni1 [postaveni ]  
  (loop [post postaveni mozneTahy (mozneTahyZPostaveni post) reseni [] trebaResit {} uroven 0 fut []]
    (let [paral (group-by future-done? fut) parhot (some #(not (nil? %) % ) (map deref (get paral true))) dalfut (get paral false)]
  (if (or (not (nil? parhot)) (jeKonecHry? post))
      (if (nil? parhot) (do (println "reseni:" reseni) reseni) (do (println "parael:" parhot) parhot));(do (println post "->" reseni) reseni) ;else
      (if (empty? mozneTahy)
        (if (= uroven 0) 
          (do (println "nema reseni") nil) 
          (let [predur (dec uroven) predTrebaResit (first (get trebaResit predur))] 
               (recur (first predTrebaResit)  (last predTrebaResit) (butlast reseni) (dissoc trebaResit predur) predur fut)))
        (let [dalsitah (first mozneTahy) zbTahy (rest mozneTahy) 
              dalsipoz (aplikujTah dalsitah post) dalmoznetahy (mozneTahyZPostaveni dalsipoz) dalres (conj reseni dalsitah) zbThproFut (/ (count zbTahy) 2)] 
          (do (println "zbT:" zbTahy)
             (if (> zbThproFut 1) 
               (let [zbTahy2 (take-last zbThproFut zbTahy) 
                     f (future (resPodstrom (aplikujTah (first zbTahy2) post) (rest zbTahy2) (conj reseni (first zbTahy2)) {} 0)) ]
               (recur dalsipoz  dalmoznetahy dalres (assoc-in trebaResit [uroven post] zbTahy) (inc uroven) (doall (conj dalfut f))))

               (recur dalsipoz  dalmoznetahy dalres (assoc-in trebaResit [uroven post] zbTahy) (inc uroven) dalfut)))))))))

(defn resPodstrom [postaveni mozneTahy  reseni  trebaResit uroven ]
  (if (jeKonecHry? postaveni)
      reseni ;(do (println postaveni "->" reseni) (conj reseni "vysledek")) ;else
      (if (empty? mozneTahy)
        (if (= uroven 0) 
          nil;(do (println "nema reseni") nil) 
          (let [predur (dec uroven) predTrebaResit (first (get trebaResit predur))] 
               (recur (first predTrebaResit)  (last predTrebaResit) (butlast reseni) (dissoc trebaResit predur) predur)))
        (let [dalsitah (first mozneTahy) zbTahy (rest mozneTahy) dalsipoz (aplikujTah dalsitah postaveni)] 
             (recur dalsipoz  (mozneTahyZPostaveni dalsipoz) (conj reseni dalsitah) 
                    (assoc-in trebaResit [uroven postaveni] zbTahy) (inc uroven))))))


(time (doall (map deref (for [i (range 65)] (future (Thread/sleep 1000))))))
(time (doall (map deref (for [i (apply list (range 65))] (future (Thread/sleep 1000))))))
(def f (future (Thread/sleep 5000) (inc 0)))
(map future-done? (doall (conj [(future (Thread/sleep 5000) (inc 0))] (future (Thread/sleep 10000) (inc 1)))))
(future-done? f)
(def x (doall (for [i (range 65)] (future (Thread/sleep (* i 100)) (inc i)))))
(conj x (future (Thread/sleep 1000000) (inc 5000)))
(map deref x)
(map future-done? x)
(time (some #(not (> % 10)) (map future-done? (doall (for [i (range 65)] (future (Thread/sleep (* i 100)) (inc i)))))))
(for [i (apply list (range 65))] i)
(for [i (range 65)] i)
(def a (conj [] (future (reduce + (range 0 40000000)))))
(def b (conj a (future (reduce + (range 0 10000000)))))
(map realized? b)


(defn hledejReseni [postaveni procesu] 
  (some #(when (not (nil? %)) %) (pmap (fn[[a b]] (hledejReseni1 postaveni a b)) (map vector (repeat procesu) (range 0 procesu)))))


(resPodstrom pole4  (mozneTahyZPostaveni pole4 ) [] {} 0)


(hledejReseni2 pole4) ->
(hledejReseni pole4 2) ->
;({:kamsloupec 5, :kamradka 3, :odradka 3, :odsloupec 3} 
; {:kamsloupec 4, :kamradka 3, :odradka 1, :odsloupec 4} 
; {:kamsloupec 4, :kamradka 2, :odradka 2, :odsloupec 2} 
; {:kamsloupec 3, :kamradka 2, :odradka 2, :odsloupec 5}) 
(hledejReseni1 pole4 2 0)
(mozneTahyZPostaveni pole3 1 1)
(mozneTahyZPostaveni pole4 )
(mozneTahyZPostaveni  [[nil nil 0 0 0 nil nil] [nil 0 0 1 1 0 nil] [nil 0 1 0 0 0 nil] [nil 0 0 0 0 0 nil] [nil nil 0 0 0 nil nil]] 2 1)



      ;(first (drop-while empty? (pmap #(hledejReseni2 (aplikujTah % postaveni) (cons % reseni))  mozneTahy))))))


;(some  #(when (not (nil? (hledejReseni2 (aplikujTah % postaveni) (cons % reseni))))  %) mozneTahy))))

(def pole4 [[nil nil 0 0 0 nil nil]          
            [nil 0   0 0 1 0   nil]          
            [nil 0   1 0 1 1   nil]          
            [nil 0   0 1 0 0   nil]          
            [nil nil 0 0 0 nil nil]          
                                      ])     

(def pole5 
           [[nil nil 1 1 1 nil nil]          
            [nil nil 1 1 1 nil nil]          
            [ 1   1  1 1 1  1   1 ]          
            [ 1   1  1 0 1  1   1 ]          
            [ 1   1  1 1 1  1   1 ]          
            [nil nil 1 1 1 nil nil]          
            [nil nil 1 1 1 nil nil]          
                                      ])     

(def pole3 [[nil nil 0 0 0 0 nil]
           [nil 0   0 0 0 1   nil]
           [nil 0   0 0 0 0   nil]
           [nil 0   0 0 0 0   nil]
           [nil nil 0 0 0 nil nil]
          ])


