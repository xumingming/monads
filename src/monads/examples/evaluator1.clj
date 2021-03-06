(ns monads.examples.evaluator1
  (:require [monads.core :refer :all]
            [monads.state :as s])
  (:use [monads.util :only [lift-m lift-m*]]))

(defn const [x]
  (if (symbol? x)
    (lift-m #(get % x) get-state)
    (return x)))

(declare run)

(defn calc [op x y]
  (lift-m* op (run x) (run y)))

(defn decl [x y]
  (mdo v <- (run y)
       (modify #(assoc % x v))
       (return v)))

(defn run [op]
  (if (list? op)
    (case (second op)
      + (calc + (first op) (last op))
      - (calc - (first op) (last op))
      * (calc * (first op) (last op))
      / (calc / (first op) (last op))
      = (decl (first op) (last op)))
    (const op)))

(def table {'pi 3.141592 'e 2.718281 'deg 57.295779})

(defn evaluate [op]
  (s/run-state (run op) table))
