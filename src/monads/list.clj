(ns monads.list
  (:require [monads.core :refer :all]
            [monads.util :as u]))


(defn flatten-1
  [seqs]
  (lazy-seq
   (when-let [s (seq seqs)]
     (concat (first s) (flatten-1 (rest s))))))

(defn mcat [f xs]
  (lazy-seq
   (if (not (seq xs))
     nil
     (concat (f (first xs)) (mcat f (rest xs))))))

(defmonad lazy-m
  :return list
  :bind (fn [m f]
          (mcat (fn [v] (run-monad lazy-m (f v))) m)))

(defn push-tails [s]
  (if (and (:tails s) (:head s) (-> s :head :tails))
    (-> s
        (update-in [:head :tails] concat (:tails s))
        (dissoc :tails))
    s))

(defrecord Node [head tails]
  Object
  (toString [this] (with-out-str (print [head tails]))))

(declare node-seq)
(declare strict-m) 

(def _x (atom 0))

(defn node-seq [n tails]
  (if (instance? Node n)
    (let [h (:head n)
          ts (concat tails (:tails n))]
      (if (instance? Node h)
        (recur h ts)
        (lazy-seq (let [x (run-monad strict-m (first ts))]
                    (cons h (node-seq x (rest ts)))))))
    (if (seq tails)
      (do #_(println "tails is ..." tails "\n" (seq tails))
          #_(println (run-monad strict-m (first tails)) (rest tails))
          (lazy-seq (cons n (node-seq (run-monad strict-m (first tails)) (rest tails)))))
      n)))

(defmonad strict-m
  :return list
  :bind (fn [m f]
          (tlet [m m]            
            (let [xs (map f m)]
              (reduce (fn [m-acc m]
                        (tlet [ms m-acc
                               mval m]
                          (concat ms mval)))
                      ()
                      xs))))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (tlet [l (first leftright)
                              r (second leftright)]
                         (concat l r)))})


(defmonad strict-m
  :return list
  :bind (fn [m f]
          (->Cont m
                  (fn [m]
                    (let [xs (map f (if (seq? m) m (mcat identity (node-seq m ()))))]
                      (reduce (fn [m-acc m]
                                (->Cont m
                                        (fn [mval]
                                          (Node. mval [(->Cont m-acc identity)]))))
                              ()
                              (reverse xs))))))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (concat (run-monad strict-m (first leftright))
                               (run-monad strict-m (second leftright))))})

(defn run-list [c]
  (mcat identity (node-seq (run-monad strict-m c) ())))

(deftype Stream [head tails]
  Object
  (toString [self] (with-out-str (print [head tails]))))

(defmacro stream [head & [tail]]
  `(Stream. (atom (list ~head)) (atom [(delay ~tail)])))

(defmacro streams [head & [tail]]
  `(Stream. (atom ~head) (atom [(delay ~tail)])))

(defn revappend [xs ys]
  (if (seq xs)
    (recur (rest xs) (conj ys (first xs)))
    ys))

(defn append [xs ys]
  (revappend (reverse xs) ys))

(defn step-stream [^Stream s]
  (let [head (.head s)
        tails (.tails s)
        tail (first @tails)
        tails (rest @tails)]
    (if-let [r @tail]
      (let [^Stream r r
            r-head (.head r)
            r-tails @(.tails r)]
        (if (or (seq tails) (seq r-tails))
          (let [new-head  (append @head @r-head)
                new-tails (append tails r-tails)]
            (reset! head new-head)
            (reset! (.tails s) new-tails)
            s)
          (append @head @r-head)))
      (if (seq tails)
        (do (swap! (.tails s) rest) s)
        @head))))

(defn stream-map [f s]
  (when-let [^Stream s s]
    (let [heads @(.head s)
          tails @(.tails s)]
      (Stream. (atom (map f heads)) (atom (map #(delay (stream-map f (deref %))) tails))))))

(defn step-if [^Stream s pred]
  (if (pred @(.head s) @(.tails s))
    (step-stream s)
    s))

(defn stream-first [s]
  (when-let [^Stream s s]
    (first @(.head s))))

(defn stream-rest [s]
  (when-let [^Stream s s]
    (let [r (step-if s (fn [heads tails] (< (count heads) 2)))]
      (if-not (instance? Stream r)
        (rest r)
        (let [^Stream r r]
          ;; we can't share the tails atom itself, but this lets us at
          ;; least share the delay it's wrapping.
          (Stream. (atom (rest @(.head r))) (atom @(.tails r))))))))

(defn stream-cat [s r]
  (cond
   (nil? s) r
   (nil? r) s
   :else
   (let [^Stream s s ^Stream r r]     
     (Stream. (.head s) (atom (conj @(.tails s) (delay r)))))))

(defn stream-realize [s]
  (if (instance? Stream s)
    (recur (step-stream s))
    s))
