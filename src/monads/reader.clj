(ns monads.reader
  (:require [monads.core :refer :all])
  (:use [monads.util :only [curryfn tcurryfn lazy-pair if-inner-return]]
        [monads.types :only [tlet run-tramp]])
  (:import [monads.types Returned Done]))

(defn run-reader-t* [m comp e]
  (tlet [f (run-monad* m comp)]
    (f e)))

(def run-reader-t (comp run-tramp run-reader-t*))

(declare reader-t)

(defn- reader-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (curryfn [x _] (Done. (i-return x)))
     :bind (fn [m f]
             (Done.
              (fn [e]
                (tlet [comp (m e)]
                  (run-mdo* inner
                            a <- comp
                            (run-reader-t* (reader-t inner) (f a) e))))))
     :monadtrans {:lift (tcurryfn [m _]
                          (run-mdo* inner
                                    v <- m
                                    (return (constantly v))))}
     :monadplus (when (:monadplus inner)
                  (let [i-zero (-> inner :monadplus :mzero)
                        i-plus (-> inner :monadplus :mplus)
                        i-catch? (-> inner :monadplus :left-catch?)
                        i-zero? (-> inner :monadplus :mzero?)]
                    {:mzero (Done. (fn [_] i-zero))
                     :mplus (fn [lr]
                              (Done.
                               (fn [e]
                                 (tlet [lv (run-reader-t* (reader-t inner) (first lr) e)]
                                   (if (and i-catch? (not (i-zero? lv)))
                                     (Done. lv)
                                     (tlet [rv (run-reader-t* (reader-t inner) (second lr) e)]
                                       (Done. (i-plus [lv rv]))))))))})))))

(def reader-t (memoize reader-t*))

(declare run-reader*)

(defmonad reader-m
  :return (curryfn [v _] (Done. v))
  :bind (fn [m f]
          (Done.
           (fn [e]
             (tlet [r (m e)]
               (run-reader* (f r) e))))))

(defn run-reader* [comp e]
  (tlet [f (run-monad* reader-m comp)]
    (f e)))

(def run-reader (comp run-tramp run-reader*))

(def ask (Returned. (tcurryfn [m v]
                      (if-inner-return m
                        (i-return v) 
                        v))))
;; or: (defn asks [f] (lift-m f ask))
(defn asks [f] (Returned. (tcurryfn [m v]
                           (if-inner-return m
                             ((comp i-return f) v)
                             (f v)))))
(defn local [f comp] (Returned.
                      (tcurryfn [m e]
                        (if-inner-return m
                          (run-reader-t* m comp (f e))
                          (run-reader* comp (f e))))))

(def t reader-t)
(def m reader-m)
