(ns monads.list
  (:require [monads.core :refer :all])
  (:import [monads.types Done])
  (:use [monads.types :only [tlet]]))


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
            (let [xs (map f m)]
              (tlet [x (run-monad* list-m (first xs))]
                (Done.
                 (append x (rest xs)))))
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

(defn run-list* [xs]
  (when (seq xs)
    (if-let [f (seq (run-monad list-m (first xs)))]
      (lazy-seq (cons f (let [r (rest xs)]
                          (loop [f (first r)
                                 rr (rest r)]
                            (let [f (run-monad list-m f)]
                              (if (seq f)
                                (run-list (append f rr))
                                (if (seq rr)
                                  (recur (first rr) (rest rr))
                                  nil)))))))
      (lazy-seq  (let [r (rest xs)]
                          (loop [f (first r)
                                 rr (rest r)]
                            (let [f (run-monad list-m f)]
                              (if (seq f)
                                (run-list (append f rr))
                                (if (seq rr)
                                  (recur (first rr) (rest rr))
                                  nil)))))))))

(defn run-list [c]
  (run-list* (run-monad list-m c)))

(def m list-m)
