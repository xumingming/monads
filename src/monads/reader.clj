(ns monads.reader
  (:require [monads.core :refer :all])
  (:use [monads.util :only [curryfn lazy-pair]])
  (:import [monads.types Returned]))

(defn run-reader-t [m comp e]
  ((run-monad m comp) e))

(declare reader-t)

(defn- reader-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (comp constantly i-return)
     :bind (fn [m f]
             (fn [e]
               (run-mdo inner
                        a <- (m e)
                        (run-reader-t (reader-t inner) (f a) e))))
     :monadfail (when (:monadfail inner)
                  {:mfail (curryfn [str _]
                            ((-> inner :monadfail :mfail) str))})
     :monadtrans {:lift (curryfn [m e] (run-monad inner m))}
     :monadreader {:ask (fn [e] (i-return e))
                   :local (fn [f comp]
                            (fn [e]
                              (run-reader-t (reader-t inner) comp (f e))))}
     :monadplus (when (:monadplus inner)
                  (let [i-zero (-> inner :monadplus :mzero)
                        i-plus (-> inner :monadplus :mplus)]
                    {:mzero (constantly i-zero)
                     :mplus (curryfn [leftright e]
                              (i-plus
                               (lazy-pair
                                (run-reader-t (reader-t inner) (first leftright) e)
                                (run-reader-t (reader-t inner) (second leftright) e))))})))))

(def reader-t (memoize reader-t*))

(declare run-reader)

(defmonad reader-m
  :return constantly
  :bind (fn [m f]
          (fn [e]
            (run-reader (f (run-reader m e)) e)))
  :monadreader {:ask (fn [e] e)
                :local (fn [f comp]
                         (fn [e]
                           (run-reader comp (f e))))})

(defn run-reader [comp e]
  ((run-monad reader-m comp) e))

(def t reader-t)
(def m reader-m)

(defn lift-catch [m h]
  (Returned. (curryfn [t e]
               (run-monad (:inner t) (catch-error (run-reader-t t m e)
                                                  (fn [err] (run-reader-t t (h err) e)))))))
