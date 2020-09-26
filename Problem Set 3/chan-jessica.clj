;; pre-amble
(import java.lang.Math)

(def vocabulary '(call me ishmael))

(defn log2 [n]
    (/ (Math/log n) (Math/log 2)))

(defn score-categorical [outcome outcomes params]
	(if (empty? params)
		(throw (RuntimeException. (str "no matching outcome")))
		(if (= outcome (first outcomes))
			(first params)
			(score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
	(if (empty? lst)
		base
		(f (first lst)
			(list-foldr f base (rest lst)))))

(defn score-BOW-sentence [sen probabilities] 
	(list-foldr
		(fn [word rest-score]
		(+ (log2 (score-categorical word vocabulary probabilities))
				rest-score)) 
		0
		sen))

(defn score-corpus [corpus probabilities] 
	(list-foldr
		(fn [sen rst]
			(+ (score-BOW-sentence sen probabilities) rst))
		0 
		corpus))

(defn logsumexp [log-vals]
	(let [mx (apply max log-vals)]
		(+ mx 
			(log2
				(apply +
					(map (fn [z] (Math/pow 2 z))
					(map (fn [x] (- x mx)) log-vals)))))))

;; problem 1
(def theta1 (list (/ 1 2) (/ 1 4) (/ 1 4) ))
(def theta2 (list (/ 1 4) (/ 1 2) (/ 1 4) ))

(def thetas (list theta1 theta2))

(def theta-prior (list (/ 1 2) (/ 1 2)))

(def my-corpus '((call me) (call ishmael)))

(defn theta-corpus-joint [theta corpus theta-probs]
	(+ (score-corpus corpus theta) 
		(log2 (score-categorical theta thetas theta-probs))))

; (println theta1)
; (println (score-corpus my-corpus theta1))
; (println (theta-corpus-joint theta1 my-corpus theta-prior))

;; problem 2
(defn compute-marginal [corpus theta-probs]
	(logsumexp
			(map (fn [th] (theta-corpus-joint th corpus theta-probs))
		thetas)))

; (println (compute-marginal my-corpus theta-prior))
; (println (Math/pow 2 (compute-marginal my-corpus theta-prior)))

;; problem 3
(defn compute-conditional-prob [theta corpus theta-probs]
	(- (theta-corpus-joint theta corpus theta-probs) 
				(compute-marginal corpus theta-probs)))

; (println (compute-conditional-prob theta1 my-corpus theta-prior))

;; problem 4
(defn compute-conditional-dist [corpus theta-probs]
	(map (fn [th] (compute-conditional-prob th corpus theta-probs))
		thetas))

;; problem 5
; (println (compute-conditional-dist my-corpus theta-prior))
; (println (map (fn [x] (Math/pow 2 x))
; 		(compute-conditional-dist my-corpus theta-prior)))

;; problem 6
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
	(let [conditional-dist (map (fn [x] (Math/pow 2 x)) (compute-conditional-dist observed-corpus theta-probs))]
		(compute-marginal new-corpus conditional-dist)))

; (println (compute-posterior-predictive my-corpus my-corpus theta-prior))
; (println (Math/pow 2 (compute-posterior-predictive my-corpus my-corpus theta-prior)))


;; problem 7
(defn flip [p]
	(if (< (rand 1) p)
		true
		false))

; (defn repeat [f n]
; 	(if (= n 0) 
; 		'()
; 			(cons (f) (repeat f (- n 1)))))

(defn normalize [params]
	(let [sum (apply + params)]
		(map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
	(if (flip (first params))
		(first outcomes)
		(sample-categorical (rest outcomes)
											(normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
	(if (= len 0)
		'()
		(cons (sample-categorical vocabulary probabilities)
			(sample-BOW-sentence (- len 1) probabilities))))

(defn sample-BOW-corpus [theta sent-len corpus-len]
	(repeatedly corpus-len (fn [] (sample-BOW-sentence sent-len theta))))

; (println (sample-BOW-corpus theta1 2 2))

;; problem 8 
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
	(let [theta (sample-categorical thetas theta-probs)]
		(list theta (sample-BOW-corpus theta sent-len corpus-len))))

;; problem 9
(defn get-theta [theta-corpus]
	(first theta-corpus))

(defn get-corpus [theta-corpus]
	(first (rest theta-corpus)))

(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
 (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
	(let [sample-corpora (map get-corpus (sample-thetas-corpora sample-size sent-len corpus-len theta-probs))]
		(/ (apply + (map (fn [c] 
					(if (= c corpus)
							1 0))
				sample-corpora)) sample-size)))

; (println (sample-thetas-corpora 3000 2 2 theta-prior))

; (println (estimate-corpus-marginal my-corpus 50 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 50 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 50 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 50 2 2 theta-prior))


; (println (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior))
; (println (estimate-corpus-marginal my-corpus 10000 2 2 theta-prior))


;; problem 11
(defn get-count [obs observation-list count]
	(if (empty? observation-list)
		count
		(if (= obs (first observation-list))
						(get-count obs (rest observation-list) (+ 1 count))
						(get-count obs (rest observation-list) count))))

(defn get-counts [outcomes observation-list] 
	(let [count-obs (fn [obs] (get-count obs observation-list 0))]
		(map count-obs outcomes)))

(defn rejection-sampler [theta observed-corpus sample-size sent-len corpus-len theta-probs]
	(let [pairs (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)]
		(/ (get-count theta (map get-theta (filterv (fn [p] (= observed-corpus (get-corpus p))) pairs)) 0) (count pairs))))

; (println (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
; (println (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
; (println (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))
; (println (rejection-sampler theta1 my-corpus 10000 2 2 theta-prior))


