(ns curly.core
  (:require [clojure.java
             [io :as io]
             [shell :as sh]]
            [clojure
             [string :as str]
             [edn :as edn]]))

;;; numeric

(def one?
  (comp zero? dec))

(defn nan?
  [x]
  (and (number? x) (Double/isNaN x)))

;;; stats

(defn z-score
  [val & [{:keys [mean sd] :or {mean 0 sd 1}}]]
  ;; note, this gives divide-by-zero error instead of Infinity or NaN
  ;; possibly to do with this bug:  http://dev.clojure.org/jira/browse/CLJ-1142
  ;; when sd=0.0 (eg, without the (double sd) cast:-
  ;;  (/ (- val mean) sd)
  (/ (- val mean) (double sd)))

(defn fisher-p-value
  "Fisher exact test.
      | Outcome 1 | Outcome 2 | Total
G1    |    w      |    x      | w+x
G2    |    y      |    z      | y+z
Total |   w+y     |   x+z     | w+x+y+z
"
  [w x y z]
  ;; From Martin Smith:
  ;;   https://mail.google.com/mail/u/1/?ui=2&shva=1#apps/from%3Amartin+frith++test/fd1108a5f4894c9
  ;; 
  ;; here's some perl code that does "Fisher's exact test". You need to input 4
  ;; numbers: w, x, y, z. For example,
  ;; w = DNA-binding genes with an ultraconserved-element
  ;; x = non-DNA-binding genes with an ultraconserved-element
  ;; y = DNA-binding genes without an ultraconserved-element
  ;; z = non-DNA-binding genes without an ultraconserved-element
  
  ;; n = total number of genes = w+x+y+z
  
  ;; The code calculates: given this number of ultraconserved genes and this
  ;; number of DNA-binding genes (and this total number of genes), what is the
  ;; probability that the overlap between them is w or greater just by chance?

  (let [n (+ w x y z)
        lf (into [] (reductions #(+ %1 (Math/log %2)) 0 (range 1 (inc n))))
        const (+ (lf (+ w x)) (lf (+ w y)) (lf (+ x z)) (lf (+ y z))
                 (- (lf (+ w x y z))))]
    (apply + (map (fn [w x y z] (Math/exp (- const (lf w) (lf x) (lf y) (lf z))))
                  (iterate inc w) (range x (dec 0) -1) (range y 0 -1) (iterate inc z)))))

;; Strings

(defn string-splice
  "Create a new string with the old substring replaced by a new substring.
   Given three arguments, replace a portion of the old string at the given
   offset equal to the length of the replacement. The resulting string will
   be the same length as the original."
  ([target new offset] (string-splice target new offset (count new)))
  ([target new offset length]
     (str (subs target 0 offset) new (subs target (+ offset length)))))

(defn starts-with-ignore-case?
  "True if string s starts with prefix ignoring case."
  [^String s ^String prefix]
  (.startsWith (.toLowerCase s) (.toLowerCase prefix)))

;; Seqs

;;; sorting compare-by with keys and direction
;;; https://groups.google.com/forum/#!topic/clojure/VVVa3TS15pU

(defn compare-rev
  "Compare reverse order"
  [a b]
  (compare b a))

(def asc compare)                       ; alias for compare
(def desc #(compare %2 %1))             ; same as compare-rev

(defn compare-by [& key-cmp-pairs]
  "Compare by keyword key-fn in either ascending or descending order. Eg:
 (sort (compare-by :last-name asc, :date-of-birth desc, :weird-data-key custom-compare-fn) coll)"
  (fn [x y]
    (loop [[k cmp & more] key-cmp-pairs]
      {:pre [(keyword? k), (fn? cmp), (even? (count more))]}
      (let [result (cmp (k x) (k y))]
        (if (and (zero? result) more)
          (recur more)
          result)))))

(defn all-permutations [things]
  "All permutations of things."
  (if (= 1 (count things))
    (list things)
    (for [head things
          tail (all-permutations (disj (set things) head))]
      (do
        (cons head tail)))))

(defn reverse-pair
  "Reverse a pair vector"
  [[a b]]
  [b a])

(defn sum-vals
  "Sum the values of a sequence of pairs (eg, a map or vector of pairs)"
  [pair-seq]
  (reduce + (vals pair-seq)))

(defn sum-second
  "Sum the second values of a sequence of pairs (eg, a vector of pairs)"
  [pair-seq]
  (reduce + (map second pair-seq)))

(defn map-or
  "Returns a lazy sequence consisting of the result of applying f to the
set of first items of each coll, followed by applying f to the set
of second items in each coll. Different to map, it continues until all of the colls are
exhausted. Any missing items in other colls are treated as nil values. Function
f should accept number-of-colls arguments."
  {:added "1.0"
   :static true}
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (or s1 s2)
        (cons (f (first s1) (first s2))
              (map-or f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (or s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map-or f (rest s1) (rest s2) (rest s3))))))))

;;; Data structures
(defn map-rev-2-keys
  "Reverse two levels of keys of a map"
  [map]
  (for [[k1 vmap] map
        [k2 _] vmap]
    [k2 k1]))

(defn map-fn-key-set
  "Apply fun to a map then make a map of first key with set of second key"
  [map fun]
  (reduce (fn [v [k1 k2]]
            (update-in v [k1] (fnil (fn [v] (conj v k2)) #{})))
          {}
          (fun map)))

;;; Map

(defn flatten-map
  "Flatten a map of maps, joining keys with separator. From http://stackoverflow.com/questions/17901933/flattening-a-map-by-join-the-keys"
  ([form separator]
     (into {} (flatten-map form separator nil)))
  ([form separator pre]
     (mapcat (fn [[k v]]
               (let [prefix (if pre (str pre separator (name k)) (name k))]
                 (if (map? v)
                   (flatten-map v separator prefix)
                   [[(keyword prefix) v]])))
             form)))

;;; Tries
;;; see: http://stackoverflow.com/a/9808147/2104475
(defn build-trie [seed & kvs]
  (reduce
   (fn [trie [k v]]
     (assoc-in trie (concat k [:val]) v))
   seed
   (partition 2 kvs)))

(defn prefix-match [target trie]
  (when (seq target)
    (when-let [node (trie (first target))]
      (or (:val node)
          (recur (rest target) node)))))

;;; File IO

(defn gzip?
  "True if file is a gzip file."
  [file-name]
  (re-find #"gz$" file-name))

(defn gpg-decrypt [file]
  "Decrypt file and return contents as a string. Throws an exception if gpg decrypt fails."
  (let [{:keys [out exit err]} (sh/sh "gpg" "--quiet" "--batch" "--decrypt" (str file))]
    (if (zero? exit)
      out
      (throw (Exception. (str "Cannot decrypt file [" file "]: " err))))))

(defmacro redir
  "Redirect *out* to filename and evaluate body"
  [filename & body]
  `(with-open [w# (io/writer ~filename)]
     (binding [*out* w#] ~@body)))

(defn read-edn
  "Read EDN data from file"
  [file]
  (with-open [in (java.io.PushbackReader. (io/reader file))]
    (edn/read in)))

;;;

(defn version-compare
  "Compare two numeric version strings with elements separated by dots."
  [v1 v2]
  (let [to-int-seq (fn [s] (map #(Integer/parseInt %) (str/split s #"\.")))
        s1 (to-int-seq v1)
        s2 (to-int-seq v2)]
    ;; compare each pair of elements, return first non-zero result
    ;; (meaning the versions are different) or 0 otherwise
    (or (some #(if (not (zero? %)) %) (map-or compare s1 s2)) 0)))
