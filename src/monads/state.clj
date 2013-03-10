(ns monads.state
  (:require [monads.core :refer :all])
  (:use [monads.types :only [fst snd run-tramp tlet]]
        [monads.util :only [curryfn tcurryfn lazy-pair if-inner-return lift-m]])
  (:import [monads.types Returned Pair Done]))

(declare state-t)

(defn run-state-t* [m computation initial-state]
  (tlet [f (run-monad* m computation)]
    (f initial-state)))

(def run-state-t (comp run-tramp run-state-t*))

(defn- state-t* [inner]
  (let [i-return (:return inner)]
    (monad
     :inner inner
     :return (curryfn [x s] (Done. (i-return (Pair. x s))))
     :bind (fn [m f]
             (Done.
              (fn [s]
                (tlet [comp (m s)]
                  (run-mdo* inner
                            ^Pair p <- comp
                            let v = (fst p), s = (snd p)
                            (run-state-t* (state-t inner) (f v) s))))))
     :monadfail (when (:monadfail inner)
                  {:mfail (fn [str] (Done. (constantly ((-> inner :monadfail :mfail) str))))})
     :monadplus (when (:monadplus inner)
                  (let [i-plus (-> inner :monadplus :mplus)
                        i-zero (-> inner :monadplus :mzero)
                        i-catch? (-> inner :monadplus :left-catch?)
                        i-zero? (-> inner :monadplus :mzero?)]
                    {:mzero (Done. (fn [_] i-zero))
                     :mplus
                     (fn [leftright]
                       (Done.
                        (fn [s]
                          (tlet [lv (run-state-t* (state-t inner) (first leftright) s)]
                            (if (and i-catch? (not (i-zero? lv)))
                              (Done. lv)
                              (tlet [rv (run-state-t* (state-t inner) (second leftright) s)]
                                (i-plus [lv rv])))))))}))
     :monadtrans {:lift (tcurryfn [m s]
                          (run-mdo* inner
                                    v <- m
                                    (return (Pair. v s))))})))
(def state-t (memoize state-t*))

(declare run-state*)

(defmonad state-m
  :return (curryfn [x s] (Done. (Pair. x s)))
  :bind (fn [m f]
          (Done.
           (fn [s]
             (tlet [^Pair p (m s)]
               (run-state* (f (fst p)) (snd p)))))))

(defn run-state* [computation initial-state]
  (tlet [f (run-monad* state-m computation)]
    (f initial-state)))

(def run-state (comp run-tramp run-state*))

(def get-state (Returned. (tcurryfn [m s] 
                            (if-inner-return m
                              (i-return (Pair. s s))
                              (Pair. s s)))))

(defn put-state [v] (Returned.
                     (tcurryfn [m s]
                       (if-let [i-return (-> m :inner :return)]
                         (i-return (Pair. nil v))
                         (Pair. nil v)))))

(defn modify [f] (>>= get-state (comp put-state f)))

(def eval-state (comp fst run-state))
(def exec-state (comp snd run-state))
(defn exec-state-t [m comp initial-state]
  (run-monad (:inner m) (lift-m snd (run-state-t m comp initial-state))))
(defn eval-state-t [m comp initial-state]
  (run-monad (:inner m) (lift-m fst (run-state-t m comp initial-state))))

(def t state-t)
(def m state-m)
