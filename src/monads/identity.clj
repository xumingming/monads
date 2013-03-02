(ns monads.identity
  (:use [monads.core :only [defmonad run-monad]]))

(defmonad identity-m
  :bind (fn [m f] (f m))
  :return identity)

(def m identity-m)
