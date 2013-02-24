(ns monads.types)

(deftype More [t]
  Object
  (toString [this] (with-out-str (print t))))
(deftype Cont [a f]
  Object
  (toString [this] (with-out-str (print [a f]))))
(deftype Done [v]
  Object
  (toString [this] (with-out-str (print v))))

(defprotocol MRun
  (mrun [this m]))

(extend-protocol MRun
  Object
  (mrun [this _] (Done. this))
  nil
  (mrun [this _] (Done. nil)))

(deftype Return [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (Done. ((:return m) v))))

(deftype Returned [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (Done. (v m))))

(deftype Bind [comp f]
  Object
  (toString [this]
    (with-out-str (print [comp f])))
  MRun
  (mrun [_ m] (Cont. (More. (fn [] (println comp) (mrun comp m)))
                     (fn [comp] (println "computed: " comp) ((:bind m) comp f)))))

(deftype Pair [fst snd]
  clojure.lang.Seqable
  (seq [_] (list fst snd))
  Object
  (toString [this]
    (with-out-str (print [fst snd]))))

(defn fst [^Pair o] (.fst o))
(defn snd [^Pair o] (.snd o))


(deftype Either [v type]
  Object
  (toString [this]
    (with-out-str (print [type v]))))

(defn right? [^Either o]
  (= :right (.type o)))
(defn left? [^Either o]
  (= :left (.type o)))

(defn right [x]
  (Either. x :right))
(defn left [x]
  (Either. x :left))

(defn either [onleft onright ^Either e]
  ((case (.type e)
     :right onright
     :left onleft) (.v e)))

(defn from-right [^Either e]
  (either (fn [_] (throw (Exception. "from-right on left value!"))) identity e))

(defn from-left [^Either e]
  (either identity (fn [_] (throw (Exception. "from-left on right value!"))) e))

(deftype Just [v]
  Object
  (toString [this]
    (with-out-str (print v))))

(def nothing nil)
(def nothing? nil?)

(defn just? [v]
  (instance? Just v))

(defn from-just [^Just v]
  (cond
   (just? v) (.v v)
   (nothing? v) (throw (Exception. "Can't get something from nothing!"))
   :else (throw (Exception. (str v " is neither something nor nothing!")))))

(def just #(Just. %))

(defn maybe [on-nothing on-just m]
  (if m
    (on-just (from-just m))
    on-nothing))


(defn run-tramp [cur]
  (loop [cur cur res nil stack ()]
    (if (not (nil? res))
      (from-just res)
      (case (:type cur)
        :done (if (empty? stack)
                (:val cur)
                (recur (((first stack) (:val cur))) nil (rest stack)))
        :more (recur ((:t cur)) nil stack)
        :cont (recur (:a cur) nil (cons (:f cur) stack))))))

(declare is-odd?)
(defn is-even? [n]
  (if (== n 0)
    {:type :done :val true}
    {:type :more :t (fn [] (is-odd? (- n 1)))}))
(defn is-odd? [n]
  (if (== n 1)
    {:type :done :val true}
    {:type :more :t (fn [] (is-even? (- n 1)))}))


(defn run-tramp* [cur]
  (loop [cur cur stack ()]
    (println cur)
    (condp instance? cur
      Done (let [^Done cur cur]
             (if (empty? stack)
               (.v cur)
               (recur ((first stack) (.v cur)) (rest stack))))
      More (let [^More cur cur]
             (recur ((.t cur)) stack))
      Cont (let [^Cont cur cur]
             (recur (.a cur) (cons (.f cur) stack))))))
