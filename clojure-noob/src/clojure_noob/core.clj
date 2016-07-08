(ns clojure-noob.core
  (:gen-class))


(def pole1 [[nil nil 1 1 1 nil nil]
            [nil 1   1 1 1 1   nil]
            [nil 1   1 0 1 1   nil]
            [nil 1   1 1 1 1   nil]
            [nil nil 1 1 1 nil nil]
          ])



(def pole3 [[nil nil 0 0 0 0 nil]
           [nil 0   0 0 0 1   nil]
           [nil 0   0 0 0 0   nil]
           [nil 0   0 0 0 0   nil]
           [nil nil 0 0 0 nil nil]
          ])


(def tah {:odradka 1 :odsloupec 3 :kamradka 3 :kamsloupec 3})

(defn zmenaPrvku [seznam poradi hodnota] 
      (vec (concat (take poradi seznam) (vector hodnota) (drop (inc poradi) seznam))))
;(zmenaPrvku [nil 1   1 1 1 1   nil] 2 0) ;-> [nil 1 0 1 1 1 nil]
;(zmenaPrvku pole 3 [1]) -> (nil 1 0 1 1 1 nil) 
;                  -> ([nil nil 1 1 1 nil nil] [nil 1 1 1 1 1 nil] [nil 1 1 0 1 1 nil] [1] [nil nil 1 1 1 nil nil]) 

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
;(moznyTahZapad pole1 {:odradka 2 :odsloupec 5});-> {:kamsloupec 3, :kamradka 2, :odsloupec 5, :odradka 2} 

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
;(moznyTahVychod pole {:odradka 2 :odsloupec 1});-> {:kamsloupec 3, :kamradka 2, :odsloupec 5, :odradka 2} 

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

;(mozneTahyZPozice pole {:odradka 0 :odsloupec 3})
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

;z pole figurek vytvori seznam moznych tahu
(defn mozneTahyZPostaveni [postaveni] 
  (reduce concat (map #(mozneTahyZPozice postaveni {:odradka (first %) :odsloupec (second %)}) (souradniceFigurekZPole postaveni))))
;(mozneTahyZPostaveni pole2);->({:kamsloupec 3, :kamradka 1, :odradka 1, :odsloupec 5})
;(mozneTahyZPostaveni pole2);->({:kamsloupec 3, :kamradka 1, :odradka 1, :odsloupec 5})
;(mozneTahyZPostaveni [[nil nil 0 0 0 0 nil] [nil 0 1 1 0 0 nil] [nil 0 0 0 0 0 nil] [nil 0 0 0 0 0 nil] [nil nil 0 0 0 nil nil]])
;(mozneTahyZPostaveni pole2)
;------

;je vyreseno?
(defn pocetFigurek [postaveni] (count (filter #(= 1 %) (reduce concat postaveni))))
(defn jeKonecHry? [postaveni]  (= (pocetFigurek postaveni) 1))
;(pocetFigurek pole3);->1
;(jeKonecHry? pole3);->true

(defn compute-across [func elements value]
  (if (empty? elements)
    value
    (recur func (rest elements) (func (first elements) value ))))

(defn hledejReseni [postaveni reseni dalsipostaveni]  
  ;(do (println "postaveni>" postaveni "dalsipostaveni>" dalsipostaveni)
  (if (jeKonecHry? postaveni)
      reseni ;else
      (let [mozneTahy           (mozneTahyZPostaveni postaveni)]
        (if (empty? mozneTahy)
            ;(do (println "moznetahy>" (mozneTahyZPostaveni postaveni) "z postaveni>" postaveni)
            (if (empty? dalsipostaveni)
              "nema reseni";else
              #(hledejReseni (first dalsipostaveni) reseni (rest dalsipostaveni)));)
            ;else
            (let [dalsimoznepostaveni (concat (map #(aplikujTah % postaveni) mozneTahy) dalsipostaveni)]
              #(hledejReseni (first dalsimoznepostaveni) (conj reseni (first mozneTahy)) (rest dalsimoznepostaveni)))))));)


(defn hledejReseni2 [postaveni reseni dalsipostaveni]  
  (do (println "->postaveni>" postaveni) (println "->reseni>" reseni) (println "->dalsipostaveni>" dalsipostaveni) 
  (if (jeKonecHry? postaveni)
      reseni ;else
      (let [mozneTahy           (mozneTahyZPostaveni postaveni)]
        (if (empty? mozneTahy)
            ;(do (println "moznetahy>" (mozneTahyZPostaveni postaveni) "z postaveni>" postaveni)
            (if (empty? dalsipostaveni)
              "nema reseni";else
              #(hledejReseni2 (first (first dalsipostaveni)) (last (first dalsipostaveni)) (rest dalsipostaveni)));)
            ;else
            (let [dalsimoznepostaveni (concat (map #(list (aplikujTah % postaveni) (conj reseni %)) mozneTahy) dalsipostaveni)]
              #(hledejReseni2 (first (first dalsimoznepostaveni)) (last (first dalsimoznepostaveni)) (rest dalsimoznepostaveni))))))))


;(def mt [[nil nil 0 0 0 nil nil] [nil 0 1 1 0 0 nil] [nil 0 0 0 0 0 nil] [nil 0 0 0 0 0 nil] [nil nil 0 0 0 nil nil]])
;(mozneTahyZPostaveni mt)
;(map #(list (aplikujTah % pole2) %) (mozneTahyZPostaveni mt))


;(first (into [] (concat (map #(aplikujTah % pole2) (mozneTahyZPostaveni pole2)) [3 4])))

(range)

(def pole2 [[nil nil 1 1 1 nil nil]
            [nil 1   1 1 1 1   nil]
            [nil 1   1 0 1 1   nil]
            [nil 1   1 1 1 1   nil]
            [nil nil 1 1 1 nil nil]
          ])


;  (def pole2 [[nil nil 0 0 0 nil nil]
;            [nil 0   1 1 1 1   nil]
;            [nil 0   1 0 1 1   nil]
;            [nil 0   1 1 1 0   nil]
;            [nil nil 0 1 0 nil nil]
;          ])
(defn rmain [& args] (trampoline hledejReseni2 pole2 [] []))
(rmain)



