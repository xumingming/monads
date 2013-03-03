(ns monads.list
  (:require [monads.core :refer :all])
  (:import [monads.types Done])
  (:use [monads.types :only [tlet if-instance]]))


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
          (if (seq m)
            (let [xs (doall (map f m))]
              (tlet [x (run-monad* list-m (first xs))]
                (Done.
                 (append x (map (fn [x]
                                  ;; this is HORRIBLE.
                                  (if-instance monads.types.Return x
                                    (.v x)
                                    x)) (rest xs))))))
            (Done. nil)))
  :monadplus {:mzero (Done. ())
              :mplus (fn [leftright]
                       (tlet [l (run-monad* list-m (first leftright))
                              r (run-monad* list-m (second leftright))]
                         (let [res (append l r)]
                           ;; this is kind of stupid.
                           (Done.
                            (if (seq res)
                              (append [(first res)] (map return (rest res)))
                              nil)))))})

(declare run-list)

(defn get-next [xs]
  (let [r (rest xs)]
    (loop [f (first r)
           rr (rest r)]
      (let [f (run-monad list-m f)]
        (if (or (not (nil? f)) (not= () f))
          (do
            (run-list (tlet [f f]
                        (append f rr))))

          (if (seq rr)
            (recur (first rr) (rest rr))
            nil))))))

(defn run-list* [xs]
  (when (seq xs)
    (let [f (run-monad list-m (first xs))]
      (if (or (not (nil? f)) (not (= () f)))
        (lazy-seq (cons f (get-next xs)))
        (lazy-seq (get-next xs))))))

(defn run-list [c]
  (run-list* (run-monad list-m c)))

(def m list-m)
