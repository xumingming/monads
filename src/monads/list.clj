(ns monads.list
  (:require [monads.core :refer :all]
            [monads.util :as u])
  (:import [monads.core Cont]))

(defn revappend [xs ys]
  (if (seq xs)
    (recur (rest xs) (cons (first xs) ys))
    ys))

;; reverse + revappend takes twice the length of xs. (doall (concat xs
;; ys)) takes length xs + length ys. So this is not always a winner.
;; But it does win in the only test I've done, so.
(defn append [xs ys]
  (revappend (reverse xs) ys))

(defmonad list-m
  :return list
  :bind (fn [m f]
          (tlet [m m]
            (let [xs (map f m)]
              (tlet [x (first xs)]
                (append x (rest xs))))))
  :monadplus {:mzero ()
              :mplus (fn [leftright]
                       (tlet [l (first leftright)
                              r (second leftright)]
                         (append l r)))})

(defn mcat [f xs]
  (lazy-seq (if (not (seq xs))
              nil
              (concat (f (first xs)) (mcat f (rest xs))))))

(defn run-list* [xs]
  (when (seq xs)
    (lazy-seq (cons (first xs)
                    (let [r (rest xs)]
                      (run-list* (concat (run-monad list-m (first r)) (rest r))))))))

(defn run-list [c]
  (run-list* (run-monad list-m c)))

(deftype Stream [heads tails]
  Object
  (toString [self] (with-out-str (print [heads tails]))))

;; given the sequential reset!s in step-stream, the atoms here should
;; really probably be refs.
(defmacro stream [head & [tail]]
  `(Stream. (atom (list ~head)) (atom [(delay ~tail)])))
(defmacro streams [heads & [tail]]
  `(Stream. (atom ~heads) (atom [(delay ~tail)])))


(defn step-stream [^Stream s]
  (let [head (.heads s)
        tails (.tails s)
        tail (first @tails)
        tails (rest @tails)]
    (if-let [r @tail]
      (let [^Stream r r
            r-head (.heads r)
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
    (let [heads @(.heads s)
          tails @(.tails s)]
      (Stream. (atom (map f heads)) (atom (map #(delay (stream-map f (deref %))) tails))))))

(defn step-if [^Stream s pred]
  (if (pred @(.heads s) @(.tails s))
    (step-stream s)
    s))

(defn stream-first [s]
  (when-let [^Stream s s]
    (let [h @(.heads s)]
      (println h)
      (if (seq h)
        (first h)        
        (let [n (step-stream s)]
          (if (seq n)
            (recur n)
            n))))))

(defn stream-rest [s]
  (when-let [^Stream s s]
    (stream-first s)
    (let [r (step-if s (fn [heads tails] (< (count heads) 2)))]
      (if-not (instance? Stream r)
        (streams (rest r))
        (let [^Stream r r]
          ;; we can't share the tails atom itself.
          ;; we do share the delay it's wrapping.
          (Stream. (atom (rest @(.heads r))) (atom @(.tails r))))))))

(defn stream-cat [s r]
  (cond
   (nil? s) r
   (nil? r) s
   :else
   (let [^Stream s s ^Stream r r]     
     (Stream. (.heads s) (atom (conj @(.tails s) (delay r)))))))

(defn join-stream [s]
  (when-let [^Stream s s]
    (let [hs @(.heads s)
          ^Stream catted (reduce stream-cat nil hs)]
      (swap! (.tails catted) append @(.tails s))
      catted)))

(defn stream-realize [s]
  (if (instance? Stream s)
    (recur (step-stream s))
    s))

(defn stream->seq [s]
  (when (instance? Stream s)
    (lazy-seq (cons (stream-first s)
                    (stream->seq (stream-rest s))))))

