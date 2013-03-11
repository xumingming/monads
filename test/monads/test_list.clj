(ns monads.test-list
  (:use monads.core
        monads.list
        monads.util
        expectations))

(expect nil (seq (run-list (mdo
                            x <- (range 30)
                            y <- (range 30)
                            mzero
                            (return (list x y))))))

(expect nil (seq (run-list nil)))
(expect nil (seq (run-list mzero)))
(expect [nil] (run-list (return nil)))

(expect [[1 1]
         [1 2]]
        (take 2 (run-list (mdo
                           x <- (range 1 30000)
                           y <- (range 1 30000)
                           (return (list x y))))))
(expect [[1 1 nil]
         [1 1 1]
         [1 2 nil]]
        (take 3 (run-list (mdo
                           x <- (range 1 30000)
                           y <- (range 1 30000)
                           z <- [nil 1]
                           (return (list x y z))))))

(expect [[21 1]
         [21 2]]
        (take 2 (run-list (mdo
                           x <- (range 1 30000)
                           y <- (range 1 30000)
                           (guard (> x 20))
                           (return (list x y))))))


(expect [[3 4 5]
         [5 12 13]
         [6 8 10]]
        (take 3 (run-list (mdo
                           x <- (range 1 30)
                           y <- (range (inc x) 30)
                           let x2+y2 = (+ (* x x) (* y y))
                           z <- (range 1 30)
                           (guard (== (* z z) x2+y2))
                           (return (list x y z))))))
