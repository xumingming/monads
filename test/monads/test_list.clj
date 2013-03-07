(ns monads.test_list
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


