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
  [val {:keys [mean sd]}]
  ;; note, this gives divide-by-zero error instead of Infinity or NaN
  ;; possibly to do with this bug:  http://dev.clojure.org/jira/browse/CLJ-1142
  ;; when sd=0.0 (eg, without the (double sd) cast:-
  ;;  (/ (- val mean) sd)
  (/ (- val mean) (double sd)))

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

(defn compare-rev
  "Compare reverse order"
  [a b]
  (compare b a))

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
    (or (some #(if (not (zero? %)) %) (map-or compare s1 s2))) 0))
