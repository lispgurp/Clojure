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

; recurring theme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop [x 5
       result []]
  (if (> x 0)
    (recur (dec x) (conj result (+ 2 x)))
    result))
; x  result
; 5 (conj [] 7 )
; 4 (conj [7] 6 )      
; 3 (conj [7 6] 5)
; 2 (conj [7 6 5] 4)
; 1 (conj [7 6 5 4] 3)
; 0 [7 6 5 4 3]

; only odd numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-odd [sq]
  (filter #(odd? %) sq)) 

; learning about -> ;;;;;;;;;;;;;;;;;;;;;;;;;

; test
(defn threading-expr []
  (= (last (sort (rest (reverse [2 5 4 1 3 6]))))
     (-> [2 5 4 1 3 6] reverse rest sort last)
     5))

(threading-expr)

;note "last" is the answer

; rearranging code ->> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(= (apply + (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
   (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (apply +))
   11)

; note "apply +" is the answer 

; reversing a sequence ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; restrictions: rseq and reverse

(defn my-reverse-classic [sq]
  (loop [stepper sq
         result []]
    (if (empty? stepper) result
        (recur (rest stepper)
               (cons (first stepper)
                     result)))))

(defn my-reverse [sq]
  (list
   (butlast sq)
   (last sq)))

; fibbonaci numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; basis: F(0) = 0, F(1) = 1
; induction: For F(N), N > 1, F(N) = F(N-1) + F(N-2)
; (0 1 1 2 3 5 8 13 21 34)

(defn my-fibo-classic 
  "getting the nth, this totally blows the stack for large N dude, O(N) stack frames, are you kidding me?"
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (+ (my-fibo-classic (- n 1))
           (my-fibo-classic (- n 2)))))

(defn my-fibo-tail
  "getting the nth, using recur instead of self recursion "
  [n]
  (loop [fn1 0
         fn2 1
         i n]
    (if (zero? i)
      fn1
      (recur fn2 (+ fn1 fn2) (dec i)))))

;starting at i = 2
(defn my-fibo-tail-acc1
  "getting the stream of 0..n, using loop/recur instead of self recursion "
  [n]
  (loop [i 2
         acc '[0 1]]
      (if (>= i n)
        acc
        (let [fibbonaci-number (+ (nth acc (- i 1)) 
                                  (nth acc (- i 2)))]
          (recur (inc i)
                 (conj acc fibbonaci-number))))))

 
(defn my-fibo-tail-acc2*
  "getting the stream of 0..n, using fn call/recur instead of self recursion "
  [fn1 fn2 i acc]
    (if (zero? i)
      (cons 0 acc)
      (recur fn2 
             (+ fn1 fn2) 
             (dec i)
             (conj acc (+ fn1 fn2)))))

(defn my-fibo-tail-acc2 [n]
  (my-fibo-tail-acc2* 0 1 n '[]))
  

