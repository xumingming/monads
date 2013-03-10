(ns monads.error
  (:require [monads.core :refer :all])
  (:use [monads.util :only [if-inner-return]]
        [monads.types :only [from-right from-left right left left? either ->Done tlet]])
  (:import [monads.types Returned Either Done Cont]))


(declare error-t)
(defn error-t* [inner]
  (let [i-return (:return inner)
        fail (comp ->Done i-return left)]
    (monad
     :inner inner
     :return (comp i-return right)
     :bind (fn [m f]
             (run-mdo* inner
                       x <- (run-monad (error-t inner) m)
                       (either fail
                               #(run-monad* (error-t inner) (f %)) x)))
     :monadtrans {:lift (fn [m] (run-monad* inner (>>= m (comp i-return right))))}
     :monadfail {:mfail (comp ->Done i-return left)}
     :monadplus {:mzero (Done. (i-return (left nil)))
                 :mplus (fn [lr]
                          (tlet [comp (run-monad* (error-t inner) (first lr))]
                            (run-mdo* inner
                                      l <- comp
                                      (if (left? l)
                                        (run-monad* (error-t inner) (second lr))
                                        (Done. (i-return l))))))
                 :left-catch? true
                 :mzero? left?})))


(let [fail #(Done. (left %))]
  (defmonad error-m
    :return right
    :bind (fn [m f]
            (either fail
                    (fn [x] (run-monad* error-m (f x)))
                    m))
    :monadfail {:mfail fail}
    :monadplus {:mzero (Done. (left nil))
                :mplus (fn [lr]
                         (tlet [lv (run-monad* error-m (first lr))]
                           (if (left? lv)
                             (run-monad* error-m (second lr))
                             (Done. lv))))
                :left-catch? true
                :mzero? left?}))

(defn throw-error [e] (Returned. (fn [m]
                                   (Done.
                                    (if-inner-return m
                                      (i-return (left e))
                                      (left e))))))

;;; it seems as if all the stuff in here ought to be trampolined as
;;; well. But perhaps that would only matter with deeply-nested
;;; catch-errors? (e.g. (catch-error (catch-error ...) ...))?
(defn catch-error [computation handler]
  (Returned. (fn [m]
               (Done.
                (if-inner-return m
                  (run-mdo (:inner m)
                           v <- (run-monad m computation)
                           (either #(run-monad m (handler %))
                                   (comp i-return right) v))
                  (let [v (run-monad m computation)]
                    (either #(run-monad m (handler %)) right v)))))))

(def error-t (memoize error-t*))

(def m error-m)
(def t error-t)
