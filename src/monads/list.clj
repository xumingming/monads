(ns monads.list
  (:require [monads.core :refer :all])
  (:import [monads.types Done Bind Return Cont])
  (:use [monads.types :only [tlet instance-case]]))

(defn revappend [xs ys]
  (if (seq xs)
    (recur (rest xs) (cons (first xs) ys))
    ys))

;; reverse + revappend takes twice the length of xs. (doall (concat xs
;; ys)) takes length xs + length ys. So this is not always a winner.
;; But it does win in the only test I've done, so.
(defn append [xs ys]
  (revappend (reverse xs) ys))

(declare list-m)
(declare run-list)


(defn keep-going? [o]
  (or (instance? Return o)
      (instance? Bind o)))


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

(defn run-list [c]
  (let [run (run-monad list-m c)
        run (if (keep-going? (first run))
              (loop [f (first run) remainder (rest run)]
                (if (keep-going? f)
                  (recur (run-monad list-m f) remainder)
                  (let [r (append f remainder)]
                    (if (keep-going? (first r))
                      (recur (first r) (rest r))
                      r))))
              run)]
    (when (seq run)
      (let [f (first run)]
          (lazy-seq (cons f (get-next run)))
          ))))

(defn mcat [f xs]
  (lazy-seq
   (if (not (seq xs))
     nil
     (append (f (first xs)) (mcat f (rest xs))))))

(defmonad list-m
  :return list
  :bind (fn [m f]
          (if (seq m)
            (tlet [x (run-monad* list-m (f (first m)))]
              (let [rs (mcat #(instance-case %
                               Return [(.v %)]
                               Cont (run-list %)
                               [%]) (rest m))
                    rs (map f rs)]
                (Done. (append x rs))))
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

(def m list-m)

