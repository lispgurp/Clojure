; My solutions to the 4clojure.com problem list
;
; testing note: 
; 4clojure doesn't like def or defn (probably because of the rooted memory problem) 
; but I like to use defn to send it to the REPL. So convert each on of these defns
; to (fn [] ..) to use it at the website.

; conventions
; commented out: most ideal, probably we are implementing our version
; my-*-classic: Solution based on classic top down recurson (e.g. loop/recur/trampoline)
; my-*: My attempt at relatively more idiomadic Clojure code (e.g. seq lib plus map/filter and/or seq lib plus for)

; nth
;(defn my-nth (sq)
;  (nth sq))

(defn my-nth-classic [sq n]
  (cond 
    (empty? sq) nil
    (= n 0) (first sq)
    :else    
    (recur (rest sq) (dec n))))

(defn my-nth [sq n]
  (last (take (inc n) sq)))

;count
; (defn my-nth (sq)
;   (count sq))

(defn my-count-classic [sq]
  (loop [coll sq
         n 0]
    (cond
      (empty? coll) n
      :else
      (recur (rest coll) (inc n)))))

(defn my-count [sq]
  (second
   (last
    (map (fn [ele i]
           (list ele i))
         sq
         (iterate inc 0)))))

; seems to go on forver
;(defn my-count-list-comp [sq]
;  (second 
;   (last
;    (for [i (iterate inc 0)
;          ele sq]
;      (list i ele))))) 