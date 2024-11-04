;; (or (or x y) (and b c))
;;      => (and (or x y b) (or x y c))

(%dist-or-and-1 '(x y)
                '(and (or b) (or c)))
