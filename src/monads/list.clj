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
