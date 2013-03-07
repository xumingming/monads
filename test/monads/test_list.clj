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
