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
          (let [xs (map f m)]
            (tlet [x (run-monad* list-m (first xs))]
              (Done.
               (append x (rest xs))))))
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

(defn run-list* [xs]
  (when (seq xs)
    (lazy-seq (cons (first xs)
                    (let [r (rest xs)]
                      (run-list* (append (run-monad list-m (first r)) (rest r))))))))

(defn run-list [c]
  (run-list* (run-monad list-m c)))

(def m list-m)
