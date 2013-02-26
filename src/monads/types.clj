(ns monads.types)

(deftype Cont [a f]
  Object
  (toString [this] (with-out-str (print [a f]))))
(deftype Done [v]
  Object
  (toString [this] (with-out-str (print v))))

(defmacro cont [a f]
  `(Cont. (fn [] ~a)
          ~f))

;; i.e., "tail let". Would be nice to support multiple minding exprs,
;; but, eh.
(defmacro tlet [[binding expr] & forms]
  `(cont ~expr (fn [~binding] ~@forms)))

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
  (mrun [_ m]
    (Done. ((:return m) v))))

(deftype Returned [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (v m)))

(deftype Bind [comp f]
  Object
  (toString [this]
    (with-out-str (print [comp f])))
  MRun
  (mrun [_ m] (tlet [comp (mrun comp m)]
                ((:bind m) comp f))))

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
  (loop [cur cur stack ()]
    (condp instance? cur
      Done (let [^Done cur cur]
             (if (empty? stack)
               (.v cur)
               (recur ((first stack) (.v cur)) (rest stack))))
      Cont (let [^Cont cur cur]
             (recur ((.a cur)) (cons (.f cur) stack))))))
