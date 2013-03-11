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

;; this is actually comprehensible and elegant, but not lazy.
(defmonad strict-m
  :return list
  :bind (fn [m f]
          (if (seq m)
            (reduce (fn [acc xw]
                      (tlet [xs acc
                             x xw]
                        (Done. (append x xs))))
                    (Done. nil)
                    (map (comp (partial run-monad* strict-m) f) (reverse m)))
            (Done. nil)))
  :monadplus {:mzero (Done. nil)
              :mplus (fn [lr]
                       (tlet [l (run-monad* strict-m (first lr))
                              r (run-monad* strict-m (second lr))]
                         (Done. (append l r))))})

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
      (lazy-seq (cons (first run) (get-next run))))))

(defn mcat [f xs]
  (lazy-seq
   (if (not (seq xs))
     nil
     (concat (f (first xs)) (mcat f (rest xs))))))

(defmonad list-m
  :return list
  :bind (fn [m f]
          (if (seq m)
            (tlet [x (run-monad* list-m (f (first m)))]
              (let [rs
                    ;; if this form is uncommented, and the one below
                    ;; it is commented out, then you can use this
                    ;; monad with monad transformers to your heart's
                    ;; content. However, things will get sloooow
                    ;; because you'll be losing a lot of laziness.
                    ;; (Not all laziness, but a lot of it.)

                    ;; If, instead, you keep this form commented out,
                    ;; using "lift" in monad transformers where this
                    ;; is the base monad becomes risky. (And there
                    ;; isn't really a reason to use this as the base
                    ;; monad of a monad transformer stack if you
                    ;; *aren't* going to be lifting multiple choices.)

                    ;; However, with this commented out, with things
                    ;; as they are, you get actual laziness. it's
                    ;; still not as fast as a native for loop, but
                    ;; it's *pretty* fast.
                    #_(mcat #(instance-case %
                               Return [(.v %)]
                               Cont (run-list %)
                               [%]) (rest m))
                    (map #(instance-case % Return (.v %) %) (rest m))
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

