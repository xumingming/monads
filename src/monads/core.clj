(ns monads.core
  (:require [monads.types :as types]
            [clojure.set :as s]
            [the.parsatron :as parsatron]
            [macroparser.bindings :as bindings]
            [macroparser.monads :as parser])
  (:import [monads.types Return Returned Bind Cont])
  (:use [monads.types :only [if-instance]]))

(set! *warn-on-reflection* true)

(defn return [x]
  (Return. x))

;; (>>= (>>= m f) g) is, or should be, equivalent to
;; (>>= m (fn [x] (>>= (f x) g)))
;; and the list monad, sadly, breaks on the first form. so: transform
;; to second.
;; this is basically an ugly hack.
(defn >>= [m f]
  (if-instance Bind m
    (let [comp (.comp m)
          mf (.f m)]
      (Bind. comp (fn [v] (Bind. (mf v) f))))
    (Bind. m f)))

(defn >> [m c]
  (>>= m (fn [_] c)))

(defn run-monad* [m computation]
  (types/mrun computation m))

(defn run-monad [m computation]
  (types/run-tramp (types/mrun computation m)))

(defmacro monad [& {:as params}]
  `(let [params# (s/rename-keys ~params {:>>= :bind})]
     (assert (:bind params#) (str "monad " ~name " requires a bind operation!"))
     (assert (:return params#) (str "monad " ~name " requires a return operation!"))
     params#))

(defmacro defmonad [name & {:as params}]
  `(def ~name (monad ~@(apply concat params))))

;;; monadplus
(def mzero (Returned. (fn [m] (-> m :monadplus :mzero))))
(defn mplus [left right]
  (Returned. (fn [m] ((-> m :monadplus :mplus) [left right]))))

;; monadfail
(defn mfail [msg]
  (Returned. (fn [m] ((-> m :monadfail :mfail) msg))))

(defn lift [inner]
  (Returned. (fn [m] ((-> m :monadtrans :lift) inner))))
;;; utils

(defn- unparse-m-expr [inside outside]
  (case (:type outside)
    :let `(let [~@(mapcat (fn [{:keys [bound expr]}] [(bindings/unparse-bindings bound) expr])
                          (:bindings outside))]
            ~inside)
    (:normal :bind) `(>>= ~(:expr outside) (fn [~(bindings/unparse-bindings (:bound outside))]
                                             ~inside))))

(defmacro mdo [& exprs]
  (let [parsed (reverse (parsatron/run (parser/parse-mdo) exprs))]
    (assert (= :normal (:type (first parsed))) "Last expression in mdo must be a normal clojure expression.")
    (reduce unparse-m-expr (:expr (first parsed)) (rest parsed))))

(defmacro run-mdo [m & exprs]
  `(run-monad ~m (mdo ~@exprs)))

