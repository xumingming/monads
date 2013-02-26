(ns monads.maybe
  (:require [monads.core :refer :all])
  (:import [monads.types Done Cont])
  (:use [monads.types :only [from-just nothing? just nothing maybe tlet]]
        [monads.util :only [lift-m]]))

(defn maybe-t [inner]
  (let [i-return (:return inner)]
    (monad
     :return (fn [x] (i-return (just x)))
     :bind (fn [m f] (run-mdo inner
                             v <- m
                             (if (nothing? v)
                               (i-return nothing)
                               (run-monad (maybe-t inner) (f (from-just v))))))
     :monadfail {:mfail (fn [_] (i-return nothing))}
     :monadtrans {:lift (partial lift-m just)}
     :monadplus {:mzero (i-return nothing)
                 :mplus (fn [lr]
                          (let [lv (run-monad (maybe-t inner) (first lr))]
                            (or lv
                                (run-monad (maybe-t inner) (second lr)))))})))

(defmonad maybe-m
  :return just
  :bind (fn [m f]
          (if m
            (run-monad* maybe-m (f (from-just m)))
            (Done. nil)))
  :monadfail {:mfail (constantly (Done. nothing))}
  :monadplus {:mzero (Done. nothing)
              :mplus (fn [lr]
                       (tlet [lv (run-monad* maybe-m (first lr))]
                         (if lv
                           (run-monad* maybe-m (second lr))
                           (Done. nil))))})

(def m maybe-m)
(def t maybe-t)
