(ns monads.identity
  (:require [monads.core :refer :all])
  (:import [monads.types Done Cont]))

(defmonad identity-m
  :bind (fn [m f]
          (run-monad* identity-m (f m)))
  :return identity)

(def m identity-m)
