(ns music.utils)

(defn round [n]
  (Math/round (double n)))

(defn abs [n]
  (Math/abs (double n)))

(defn- midi->hz [note] ; copied from overtone so that we're not overtone dependent
  (* 440.0 (java.lang.Math/pow 2.0 (/ (- note 69.0) 12.0))))

(defn hz
  "coerces a midi note or pc to float frequency(s)"
  [arg]
  (let [coerce (comp float midi->hz)]
    (if (seq? arg)
      (map coerce arg)
      (coerce arg))))

(defn- gen-event-seq
  [beat ts u cs ss]
  (let [[fts sts] ts]
    (lazy-seq
      (cons
        [(+ beat (* u fts)) (interleave cs (map first ss))]
        (if (and ; we only continue if all of the seqs have more values
              (every? #(not (empty? (rest %))) ss) 
              (not (empty? (rest ts))))
          (gen-event-seq (+ beat (* u (- sts fts))) ; adjust the value of beat
                         (map #(- % (- sts fts)) (rest ts)) ; we adjust the times so it starts at zero
                         u cs (map rest ss))
          ()))))); if any of the sequences are empty, we cons onto an empty list

(defn event-seq
  "returns a lazy seq of events as defined by the given args. Events are represented as
  vectors [beat-num arg-list]. For example
  (def walk
    (event-seq 
      :start 0 ; we start at beat 0.
      :times (iterate inc 0) ; we generate an event every
      :unit 0.5              ; half-beat
      :freq (map #(hz (pc/relative 60 % (pc/scale :C :M))) 
                 (iterate inc 0)) ; each event contains a :freq arg
      :amp (repeat 0.2)))         ; and an :amp arg
  (take 4 walk)
  ;=> ([0.0 (:amp 0.2 :freq 261.62558)] [0.5 (:amp 0.2 :freq 293.66476)] [1.0 (:amp 0.2 :freq 329.62756)] [1.5 (:amp 0.2 :freq 349.22824)])
  "
  [& {:keys [start times unit] :or {start 0 times (iterate inc 0) unit 1} :as args}]
  (let [arg-seqs (dissoc args :start :times :unit)
        ctls (keys arg-seqs)
        seqs (vals arg-seqs)]
    (gen-event-seq start times unit ctls seqs)))

(defn- adj-elems [v]
  (let [a (rand-int (count v))]
    [a (rem (inc a) (count v))]))

(defn mutate-pattern 
  "perform the given mutations on a random pair of adjacent elements
  in vector 'pattern'. Possible mutations are :swap, :move+, :move-,
  :copy+ and :copy-"
  [pattern & mutations]
  (let [[a b] (adj-elems pattern)]
    (reduce 
      (fn [v mutation]
        (case mutation
          :swap  (-> v (assoc a (v b)) (assoc b (v a)))
          :move+ (-> v (assoc a nil) (assoc b (v a)))
          :move- (-> v (assoc a (v b)) (assoc b nil))
          :copy+ (assoc v b (v a))
          :copy- (assoc v a (v b))
          v))
      pattern mutations)))

(defn- make-sm
  [init transitions keyfn]
  {:state init
   :transitions transitions
   :keyfn keyfn
   :update #(make-sm %1 %2 %3)})

(defn- change-sm-state
  [s transfn & args]
  (let [updater (s :update)
        trans (s :transitions)]
    (updater (apply transfn args) trans (s :keyfn))))

(defn- execute-transition
  [s & args]
  (apply (partial change-sm-state s ((s :transitions) (s :state))) args))

(defn- change-sm-keyfn [s keyfn]
  (let [updater (s :update)]
    (updater (:state s) (:transitions s) keyfn)))

(defn- change-sm-transitions [s trans]
  (let [updater (s :update)]
    (updater (:state s) trans (:keyfn s))))

(defn state-machine 
  "returns an agent that acts as a state machine. init is the initial state of
  the agent. transitions is a function that takes the current state as its 
  single argument and returns a clojure (of no arguments) that will be called to 
  determine the next state. see the bottom of the source file for example usage"
  ([init transitions] (state-machine init transitions nil))
  ([init transitions keyfn]
    (agent (make-sm init transitions keyfn))))

(defn raw 
  "returns the current state of the state-machine s, without applying the keyfn"
  [s] (:state @s))

(defn current 
  "retrieves the current state of the state-machine s and returns the result of
  passing it to the keyfn (if there is a keyfn)."
  [s] (if-let [keyfn (:keyfn @s)]
        (keyfn (raw s))
        (raw s)))

(defn change 
  "causes the state machine s to change state"
  [s & args] (apply (partial send s execute-transition) args))

(defn set-keyfn 
  "set the keyfn of state machine s to kfn"
  [s kfn] (send s change-sm-keyfn kfn))

(defn set-state
  "explicitly set the state of state-machine s to state"
  [s state]
  (send s change-sm-state identity state))

(defn set-transitions 
  "set the transitions of state-machine s to trans"
  [s trans]
  (let [state (raw s)]
    (if (and (map? trans)
             (not (contains? trans state)))
      (println "ERROR: this change would strand the state machine in an invalid state.")
      (send s change-sm-transitions trans))))

(defn r-trans
  "convenience function for creating a simple random transitions matrix for use in 
  music.utils/state-machine each state should be a vector consisting of the state and 
  a vector of states that can be transitioned to. The following are therefore 
  equivalent.
  (r-trans [:a [:b :c]] 
           [:b [:a :c]] 
           [:c [:a :b]])
  {:a #(rand-nth [:b :c]) 
   :b #(rand-nth [:a :c]) 
   :c #(rand-nth [:a :b])}
  "
  [& states]
  (apply merge 
         (map (fn [[state options]]
                {state #(rand-nth options)}) states)))

(defn w-choose 
  "selects a random item from 'choices' using the weightings in 'weights'"
  [choices weights]
  (let [r (rand-int (reduce + weights))]
    (loop [acc (first weights)
           remw (rest weights)
           remn choices]
      (if (or (empty? remn)
              (< r acc)) 
        (first remn)
        (recur (+ acc (first remw))
               (rest remw)
               (rest remn))))))

(defn w-trans
  "convenience function for creating a weighted transitions matrix for use in 
  music.utils/state-machine each state should be a vector consisting of the state, a 
  vector of states that can be transitioned to and a vector of weightings. The 
  following are therefore equivalent.
  (w-trans [:a [:b :c] [1 2]]
           [:b [:a :c] [1 1]]
           [:c [:a :b] [3 1]])
  {:a #(w-choose [:b :c] [1 2]) 
   :b #(w-choose [:a :c] [1 1]) 
   :c #(w-choose [:a :b] [3 1]}
  "
  [& states]
  (apply merge 
         (map (fn [[state options weights]]
                {state #(w-choose options weights)}) states)))

(defn d-trans
  "convenience function for creating a deterministic transitions matrix for use in 
  music.utils/state-machine each state should be a vector consisting of the state and the 
  state that follows. The following are therefore equivalent.
  (d-trans [:a :b]
           [:b :a])
  {:a #(identity :b)
   :b #(identity :a)}
  "
  [& states]
  (apply merge 
         (map (fn [[state next-state]]
                {state #(identity next-state)}) states)))

(comment
; example 1 - Basic use
(def foo
  (state-machine :a 
    {:a #(identity :c) ; this function will be called when we transition out of state :a. It's return value will be the new state.
     :b #(identity :a) ; in this example the functions are all deterministic, but they don't have to be.
     :c #(identity :b)}
    str))
(raw foo)
;=> :a
(current foo)
;=> ":a"
(change foo)
;=> #<Agent@33436...
(raw foo)
;=> :c
(current foo)
;=> ":c"
(set-keyfn foo identity)
;=> #<Agent@33436...
(current foo)
;=> :c
(set-transitions foo {:a #(identity :b) 
                      :b #(identity :c)
                      :c #(identity :a)})
;=> #<Agent@33436...
(current foo)
;=> :c
(change foo)
;=> #<Agent@33436...
(current foo)
;=> :a
(set-transitions foo {:b #(identity :c) ; this fails as it would leave the state-machine stranded in state :a, with no transitions out of this state
                      :c #(identity :b)})
;=> ERROR: this change would strand the state machine in an invalid state.
(current foo)
;=> :a


; example 2 - Random walk arpeggiator

(use '[music.pitch :only (quantize relative diatonic)])

(def chord
  (state-machine :i
    (r-trans [:i  [:iv :v]]
             [:iv [:i :v]]
             [:v  [:iv]])  ; basic transitions between chords
    (fn [degree] 
      (diatonic :C :maj degree)))) ; the keyfn converts the degree into a ptich class (ie a list of midi notes)

(raw chord)
;=> :i
(current chord)
;=> (0 4 7)
(change chord)
;=> #<Agent...
(current chord)
;=> (5 9 0)

(def random-walk
  (state-machine 0                          ; the state is initialised to 0
    (fn [n] #((rand-nth [+ -]) % n))        ; this transition function returns a closure that randomly increases or decreases the state by the argument passed to change
    (fn [n] (let [pc (current chord)        ; the keyfn reads the current chord...
                  base (quantize 60 pc)]    ; chooses a pitch to use as a reference note and quantizes it to that chord...
              (relative base n pc)))))      ; and then converts the current state to a number of steps within the current chord relative to that reference.

(current random-walk)
;=> 60
(raw random-walk)
;=> 0
(change chord)
;=> #<Agent...
(current chord)
;=> (7 11 2)
(current random-walk) ; since the chord has changed, the current value of the random walk has also changed...
;=> 59
(raw random-walk) ; ...even though the raw state is unchanged.
;=> 0
(change random-walk 2) ; this will get the transition function and call it with 2 as the single argument
;=> #<Agent...
(raw random-walk)
;=> 2
(current random-walk)
;=> 67
)
;(def testy (state-machine :i (d-trans [:i :ii] [:ii :i])))
;(current testy)
;(change testy)
;(current testy)
;(set-state testy :ii)
;(current testy)
