; preamble
(def hidden-states-1 '(Start N V))

(def vocabulary '(Call me Ishmael))

(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1
	(list theta-transition-Start theta-transition-N theta-transition-V))

(def theta-observation-Start '(0.0 0.0 0.0))
(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1 (list theta-observation-Start
theta-observation-N theta-observation-V))

(defn dist-lookup [state states dists] 
	(if (= state (first states))
		(first dists)
		(dist-lookup state (rest states) (rest dists))))

(defn log2 [n]
    (/ (Math/log n) (Math/log 2)))

(defn logsumexp [log-vals]
	(let [mx (apply max log-vals)]
		(+ mx (log2
		(apply +
			(map (fn [z] (Math/pow 2 z))
			(map (fn [x] (- x mx)) log-vals)))))))

(defn logscore-categorical [outcome outcomes params] 
	(if (= outcome (first outcomes))
		(log2 (first params))
		(logscore-categorical outcome (rest outcomes) (rest params))))


; problem 1
(defn score-next-state-word 
		[current-hidden 
			next-hidden 
			next-observed 
			theta-transition-dists 
			theta-observation-dists]
		(logsumexp [(logscore-categorical next-hidden hidden-states-1 
							(dist-lookup current-hidden hidden-states-1 theta-transition-dists))
					(logscore-categorical next-observed vocabulary 
							(dist-lookup next-hidden hidden-states-1 theta-observation-dists))]))

; (println (score-next-state-word 'Start 'N 'Call theta-transition-dists-1 theta-observation-dists-1))


; problem 2
(defn compute-next-observation-marginal 
		[current-state
			next-observation
			theta-transition-dists
			theta-observation-dists]
		(logsumexp 
							(map (fn [nh] 
									(score-next-state-word current-state nh next-observation theta-transition-dists theta-observation-dists))
									(rest hidden-states-1))))

; (println (compute-next-observation-marginal 'Start 'me theta-observation-dists-1 theta-observation-dists-1))


; problem 3
(defn score-next-states-words 
		[current-hidden
			next-hiddens
			next-words
			theta-transition-dists
			theta-observation-dists]
		(if (empty? next-hiddens)
		0.0
		(logsumexp [(score-next-state-word current-hidden (first next-hiddens) (first next-words) theta-transition-dists theta-observation-dists)
					(score-next-states-words (first next-hiddens) (rest next-hiddens) (rest next-words) theta-transition-dists theta-observation-dists)])))

; (println (score-next-states-words 'Start '(N V N) '(me Call Ishmael) theta-transition-dists-1 theta-observation-dists-1))


; problem 4
(defn compute-next-words-marginal 
	[current-hidden
		next-words
		theta-transition-dists
		theta-observation-dists]
		(if (empty? next-words)
		0.0
		(logsumexp (concat [(compute-next-observation-marginal current-hidden (first next-words) theta-transition-dists theta-observation-dists)]
					(map (fn [nh]
							(compute-next-words-marginal nh (rest next-words) theta-transition-dists theta-observation-dists)) (rest hidden-states-1))))))

; (println (compute-next-words-marginal 'Start '(me Call) theta-transition-dists-1 theta-observation-dists-1))


; problem 5

(def seq1 '(Call me))
(def seq2 '(me Call))

; 2.8479969065549495
; 2.9068905956085187

; (println (compute-next-words-marginal 'Start seq1 theta-observation-dists-1 theta-observation-dists-1))
; (println (compute-next-words-marginal 'Start seq2 theta-observation-dists-1 theta-observation-dists-1))


; problem 6
(defn compute-hidden-prior 
	[hidden-states 
		theta-transition-dists]
		(if (< (count hidden-states) 2)
			0.0
			(logsumexp
					(concat [(logscore-categorical (first (rest hidden-states)) hidden-states-1 
								(dist-lookup (first hidden-states) hidden-states-1 theta-transition-dists))]
					[(compute-hidden-prior (rest hidden-states) theta-transition-dists)]))))

; (println (compute-hidden-prior '(V N N N V) theta-transition-dists-1))


; problem 7
(defn compute-likelihood-of-words 
	[hidden-states 
		words 
		theta-observation-dists]
			(if (empty? words)
			0.0
			(logsumexp 
					(concat [(logscore-categorical (first words) vocabulary
							(dist-lookup (first hidden-states) hidden-states-1 theta-observation-dists))]
							[(compute-likelihood-of-words (rest hidden-states) (rest words) theta-observation-dists)]))))

; (println (compute-likelihood-of-words '(Start V N V) '(me Call Ishmael Call) theta-observation-dists-1))


; problem 8
(defn compute-hidden-posterior 
	[hidden-states
		words
		theta-transition-dists
		theta-observation-dists]
		(logsumexp [(compute-likelihood-of-words hidden-states words theta-observation-dists)
								(compute-hidden-prior hidden-states theta-transition-dists)
						(- 0.0 (compute-next-words-marginal (first hidden-states) words theta-transition-dists theta-observation-dists))]))

; (println "********")
; (println (compute-hidden-posterior '(Start N V N) '(Call me Ishmael Call) theta-transition-dists-1 theta-observation-dists-1))

; problem 9
; (println (compute-next-words-marginal 'Start '(me Call) theta-transition-dists-1 theta-observation-dists-1))


; problem 10 
(def compute-next-words-marginal-mem 
	(memoize (fn
			[current-hidden
				next-words
				theta-transition-dists
				theta-observation-dists]
		(if (empty? next-words)
		0.0
		(logsumexp (concat [(compute-next-observation-marginal current-hidden (first next-words) theta-transition-dists theta-observation-dists)]
					(map (fn [nh]
							(compute-next-words-marginal-mem nh (rest next-words) theta-transition-dists theta-observation-dists)) (rest hidden-states-1))))))))


; (println (compute-next-words-marginal 'Start '(Ishmael Call me Call me me Call Call me) theta-transition-dists-1 theta-observation-dists-1))


; (println (compute-next-words-marginal-mem 'Start '(me Call) theta-transition-dists-1 theta-observation-dists-1))
; (println (compute-next-words-marginal-mem 'Start '(Call me) theta-transition-dists-1 theta-observation-dists-1))
; (println (compute-next-words-marginal-mem 'Start '(Ishmael Call me Call me me Call Call me) theta-transition-dists-1 theta-observation-dists-1))









