(ns monads.list
  (:require [monads.core :refer :all])
  (:import [monads.types Done Bind Return])
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

(defn keep-going? [o]
  (or (instance? Return o)
      (instance? Bind o)))

;; omg this is the worst.
(defn get-next [xs]
  (let [r (rest xs)]
    (loop [f (first r)
           rr (rest r)]
      (let [f (run-monad list-m f)]
        (cond
         (and (not (nil? f)) (not= () f))
         
         (run-list (append f rr))
         (seq rr)
         (recur (first rr) (rest rr))
         :else nil)))))

(defn run-list* [xs]
  (when (seq xs)
    (let [f (run-monad list-m (first xs))]
      (if (and (not (nil? f)) (not (= () f)))
        (lazy-seq (cons f (get-next xs)))
        (run-list (rest xs))))))

(defn run-list [c]
  (let [run (run-monad list-m c)
        run (if (keep-going? (first run)) (loop [f (first run) remainder (rest run)]
                                            (if (keep-going? f)
                                              (recur (run-monad list-m f) remainder)
                                              (cons (first f) (append (rest f) remainder))))
                run)]
    (when (seq run)
      (let [f (run-monad list-m (first run))]
        (if (and (not (nil? f)) (not= () f))
          (lazy-seq (cons f (get-next run)))
          (recur (rest run)))))))

(def m list-m)
