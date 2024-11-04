;; (or x (and a b c))
;;     => (and (or x a) (or x b) (or x c))

(%dist-or-and-1 '(x)
                '(and (or a) (or b) (or c)))
