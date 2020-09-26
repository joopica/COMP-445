; (ns jessicachan.core)

;; problem 1
(defn abs [n]
	(Math/sqrt (* n n))) 

;; problem 2
; take-square does not specify the arguments to be passed 
(defn take-square [x]
	(* x x))

; sum-of-squares should not be calling take-square
; when specifying the arguments --> put in tex
(defn sum-of-squares [x y]
	(+ (take-square x) (take-square y)))

;; problem 3
(defn exp-13-1 []
	(* 13 1))
(defn exp-13-2 []
	(+ 3 10))
(defn exp-13-3 []
	(+ (* 3 3) 4))
(defn exp-13-4 []
	(- 15 2))

;; problem 4
(defn third [l]
	(first (rest (rest l))))

;; problem 5
(defn compose [f g]
	(fn [x] (f (g x))))

;; problem 6
(defn first-two [l]
	(list (first l) (first (rest l))))

;; problem 7
(defn remove-second[l]
	(cons (first l) (rest (rest l))))

	;; problem 8
(defn add-to-end [l x]
		(concat l (list x)))

;; problem 9
(def reverse (fn [l]
  (if (empty? l)
    []
  (concat (reverse (rest l)) (list (first l))))))

;; problem 10
(def count-to-1 (fn [n]
  (if (= n 0)
    []
  (cons n (count-to-1 (- n 1))))))

;; problem 11
(defn count-to-n [m]
	(reverse (count-to-1 m)))

;; problem 12 
(def get-max (fn [l]
  (if (= 1 (count l))
    (first l)
  (if (> (first l) (first (rest l)))
  						 (get-max (remove-second l))
  						 (get-max (rest l))))))

;; problem 13
(defn greater-than-five? [l]
	(map (fn [x] (if (> x 5) true false)) l ))

;; problem 14
(defn concat-three [x y z]
	(concat x (concat y z)))

;; problem 15 
(def sequence-to-power (fn [x n]
  (if (= n 0)
    []
  (concat x (sequence-to-power x (- n 1))))))

;; problem 16
(defn in-L? [x] 
	(if (= x (sequence-to-power L (count x))) true false))


