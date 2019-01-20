(ns predicates)

(defn sum-f [f g x]
 (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

; filter doesn't have to return true or false
; a return value of nil is enough to exclude the item
; a return value of non nill is enough to include it
; hence, can do (filter :name [{:name "wong"} {:noname "wing"}])
; ... but this is problematic... what if value for keyword is nil or false...
; hence, we replace the 'function' :name with (fn [a-map] (contains? a-map a-key), to do contains? instead.
; he called it the function key->predicate

; sets act as functions, which is pretty cool, so can do this:
; (#{1 2 3} 3) => 3
; (#{1 2 3} 4) => nil
; (filter #{1 2 3} [1 2 3 4])  => (1 2 3)
; can do this as long as the set doesn't contain false or nil
; hence, better... use a predicate

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

; complement gives you the opposite result of a predicate.
; (complement neg?) that's function composition right there.
; (filter (complement neg?) a-seq)        ; <- very common form to know
; note complement takes a fn param & returns a fn.
; (filter (fn [x] (not (neg? x))) a-seq)  ; <- equiv. to

(defn pred-and [pred1 pred2]
  (fn [x] (and (pred1 x) (pred2 x))))
; above allows: (filter (pred-and pos? even?) [1 2 -4 0 6 7 -3]) ;=> [2 6]

(defn pred-or [pred1 pred2]
  (fn [x] (or (pred1 x) (pred2 x))))
; above allows (filter (pred-or pos? odd?) [1 2 -4 0 6 7 -3])  ;=> [1 2 6 7 -3]

; complement, pred-and & pred-or are _examples of functions that take a function and return a function_
; they're inverting, & combining predicate functions respectively, into new predicate functions.


(defn whitespace? [character]
  (Character/isWhitespace character))

; strings are seqs, so can use seq fns like every?
(defn blank? [string]
  (every? whitespace? string)) ; in pre-func. world, used to have to think of this as a loop. now it's just direct meaning.


(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  ; has-award? takes 2 params. we make a new function giving it one param, but not the other.
  (every? (fn [a] (has-award? book a)) awards)) ;  replacable with (partial has-award? book) !
                                                ;  simple but not easy. "partial only fills in arguments on the left side"
                                                ;  (searched 'clojure partial second argument')

; 'some is not actually a predicate, it returns first truthy value or nil'
(defn my-some [pred a-seq]
  ; all values that pass pred convert to true; 8 test cases passing, 1 still failing. not clear what is being asked in qu.
  (first (map (fn [x] true) (filter pred a-seq))))

(defn my-every? [pred a-seq]
  (empty? (filter (complement pred) a-seq)))

(defn prime? [n]
  (let [pred (fn [x] (integer?  (/ n x)))]
    (not (some pred (range 2 n)))))
;^^
