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

; nth ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;count ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

; nil map key  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the tests
(true?  (map-entry-nil? :a {:a nil :b 2}))
(false? (map-entry-nil? :b {:a nil :b 2}))
(false? (map-entry-nil? :c {:a nil :b 2}))

;the sol
(defn map-entry-nil? [ky mp]
  (let [ent (get mp ky :empty)]
    (and (not (= ent :empty))
         (nil? ent))))

; a half truth ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; at least one are true = true
; all are true = false
; all are false = false


;tests
(= false (half-truth? false false)
(= true (half-truth? true false))
(= true (__ false true false))

;sol
(defn half-truth? [& args]
  (and
   (not (nil? (some true? args)))
   (not-every? true? args)))

; learning about -> ;;;;;;;;;;;;;;;;;;;;;;;;;

; test
(defn threading-expr []
  (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] reverse rest sort last)
     5))

(threading-expr)

;note "last" is the answer

; summing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tests
(= (my-sum2 [1 2 3]) 6)

(= (__ (list 0 -2 5 5)) 8)

(= (__ #{4 2 1}) 7)

(= (__ '(0 0 -1)) -1)
 
(= (__ '(1 10 3)) 14)


;sol
(defn my-sum [sq]
  (apply + sq))

; destructure bucture
(defn my-sum2 [& args]
  (+ args))



  
        