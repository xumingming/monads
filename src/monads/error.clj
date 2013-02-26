(ns monads.error
  (:require [monads.core :refer :all])
  (:use [monads.util :only [if-inner-return]]
        [monads.types :only [from-right from-left right left left? either ->Done tlet]])
  (:import [monads.types Returned Either Done Cont]))


(declare error-t)
(defn error-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (comp i-return right)
     :bind (fn [m f]
             (run-mdo inner 
                      x <- (run-monad (error-t inner) m)
                      (either (comp i-return left)
                              #(run-monad (error-t inner) (f %)) x)))
     :monadtrans {:lift (fn [m] (run-monad inner (>>= m (comp i-return right))))}
     :monadfail {:mfail (comp i-return left)}
     :monadplus {:mzero (i-return (left nil))
                 :mplus (fn [lr]
                          (run-mdo inner
                                   l <- (run-monad (error-t inner) (first lr))
                                   (if (left? l)
                                     (run-monad (error-t inner) (second lr))
                                     l)))})))


(defmonad error-m
  :return right
  :bind (fn [m f]
          (either (comp ->Done left)
                  (fn [x] (run-monad* error-m (f x)))
                  m))
  :monadfail {:mfail (comp ->Done left)}
  :monadplus {:mzero (Done. (left nil))
              :mplus (fn [lr]
                       (tlet [lv (run-monad* error-m (first lr))]
                         (if (left? lv)
                           (run-monad* error-m (second lr))
                           (Done. lv))))})

(defn throw-error [e] (Returned. (fn [m]
                                   (if-inner-return m
                                     (i-return (left e))
                                     (left e)))))
(defn catch-error [comp handler]
  (Returned. (fn [m]
               (if-inner-return m
                 (run-mdo (:inner m)
                          v <- (run-monad m comp)
                          (either #(run-monad m (handler %))
                                  (comp i-return right) v))
                 (let [v (run-monad m comp)]
                   (either #(run-monad m (handler %)) right v))))))

(def error-t (memoize error-t*))

(def m error-m)
(def t error-t)
