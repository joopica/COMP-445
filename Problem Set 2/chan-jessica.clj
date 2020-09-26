;; problem 1
(def moby-word-tokens '(CALL me Ishmael . 
	Some years ago never mind how long precisely having little or no money in my purse ,
	and nothing particular to interest me on shore , 
	I thought I would sail about a little and see the watery part of the world . 
	It is a way I have of driving off the spleen , 
	and regulating the circulation . 
	Whenever I find myself growing grim about the mouth whenever it is a damp , 
	drizzly November in my soul whenever I find myself involuntarily pausing before coffin warehouses , 
	and bringing up the rear of every funeral I meet and especially whenever my hypos get such an upper hand of me , 
	that it requires a strong moral principle to prevent me from deliberately stepping into the street , 
	and methodically knocking people's hats off then , 
	I account it high time to get to sea as soon as I can .
	This is my substitute for pistol and ball . 
	With a philosophical flourish Cato throws himself upon his sword I quietly take to the ship . 
	There is nothing surprising in this . 
	If they but knew it , 
	almost all men in their degree , 
	some time or other , 
	cherish very nearly the same feelings toward the ocean with me .))

(defn member-of-list? [w l] 
	(if (empty? l)
		false
	(if (= w (first l))
		true
	(member-of-list? w (rest l)))))

(defn get-vocabulary [word-tokens vocab]
	(if (empty? word-tokens)
			vocab
			(if (member-of-list? (first word-tokens) vocab) 
								(get-vocabulary (rest word-tokens) vocab) ;; if it is in vocab, call on rest
								(get-vocabulary (rest word-tokens) (cons (first word-tokens) vocab)) ;; if it is not in vocab, cons
								)))

(def moby-vocab 
	(get-vocabulary moby-word-tokens '()))

	;; problem 2
(defn get-count-of-word [w word-tokens count]
	(if (empty? word-tokens)
			count
			(if (= w (first word-tokens)) 
				(get-count-of-word w (rest word-tokens) (+ count 1)) 
				;; if word matches, add one to count and recurse
				(get-count-of-word w (rest word-tokens) count))))
				;; if not, recurse without adding

;; returns a list of numbers, no. of times word in vocab appears in tokens
(defn get-word-counts [vocab word-tokens]
	(let [count-word (fn [w]
			(get-count-of-word w word-tokens 0))]
		(map count-word vocab)))

	(def word-tokens1 (list 'the 'man 'is 'is))
	(def vocab1 (list 'man 'the 'is))


;; problem 3
(def moby-word-frequencies
	(get-word-counts moby-vocab moby-word-tokens))

(defn flip [p]
	(if (< (rand 1) p)
		true
		false))

(defn normalize [params]
	(let [sum (apply + params)]
		(map (fn [x] (/ x sum)) params)))

(defn sample-categorical [outcomes params]
	(if (flip (first params))
		(first outcomes)
		(sample-categorical (rest outcomes)
			(normalize (rest params)))))

(defn create-uniform-distribution [outcomes]
	(let [num-outcomes (count outcomes)]
		(map (fn [x] (/ 1 num-outcomes))
			outcomes)))


;;problem 4
(defn list-unfold [generator len]
  (if (= len 0)
    '()
    (cons (generator)
          (list-unfold generator (- len 1)))))

(defn sample-uniform-BOW-sentece [n vocab]
	(list-unfold (fn []
		(sample-categorical vocab (create-uniform-distribution vocab)))
		n))


;; problem 5
(defn score-categorical [w outcomes params]
  (if (empty? params)
    (print "error")
    (if (= w (first outcomes))
      (first params)
      (score-categorical w (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
       (list-foldr f base (rest lst)))))

(defn product [l]
	(apply * l))

(defn compute-uniform-BOW-prob [vocab sentence]
	(list-foldr
		(fn [word rest-score]
			(* (score-categorical word vocab (create-uniform-distribution vocab)) rest-score))
			1 sentence))

; (def s1 (sample-uniform-BOW-sentece 3 moby-vocab))
; (def p1 (compute-uniform-BOW-prob moby-vocab s1))

; (def s2 (sample-uniform-BOW-sentece 3 moby-vocab))
; (def p2 (compute-uniform-BOW-prob moby-vocab s2))

; (def s3 (sample-uniform-BOW-sentece 3 moby-vocab))
; (def p3 (compute-uniform-BOW-prob moby-vocab s3))

; (def s4 (sample-uniform-BOW-sentece 3 moby-vocab))
; (def p4 (compute-uniform-BOW-prob moby-vocab s4))

; (def s5 (sample-uniform-BOW-sentece 3 moby-vocab))
; (def p5 (compute-uniform-BOW-prob moby-vocab s5))


;; problem 7
(defn sample-BOW-sentence [len vocab params]
	(if (= len 0)
		'()
		(cons (sample-categorical vocab params)
			(sample-BOW-sentence (- len 1) vocab params))))

(def moby-word-probabilities 
	(normalize moby-word-frequencies))


;; problem 8
; (def sf1 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
; (def sf2 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
; (def sf3 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
; (def sf4 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))
; (def sf5 (sample-BOW-sentence 3 moby-vocab moby-word-probabilities))

;; problem 9
(defn lookup-probability [w outcomes probs]
	(if (= w (first outcomes))
		(first probs)
		(lookup-probability w (rest outcomes) (rest probs))))

(lookup-probability 'a (list 'the 'a 'every) (list 0.2 0.5 0.3))


;; problem 10
(defn compute-BOW-prob [sentence vocab probabilities]
	(list-foldr
		(fn [word rest-score]
			(* (lookup-probability word vocab probabilities) rest-score))
			1 sentence))


;; problem 11
; (def pf1 (compute-BOW-prob (list 'soul 'ocean 'about) moby-vocab moby-word-probabilities))
; (def pf2 (compute-BOW-prob sf2 moby-vocab moby-word-probabilities))
; (def pf3 (compute-BOW-prob sf3 moby-vocab moby-word-probabilities))
; (def pf4 (compute-BOW-prob sf4 moby-vocab moby-word-probabilities))
; (def pf5 (compute-BOW-prob sf5 moby-vocab moby-word-probabilities))



