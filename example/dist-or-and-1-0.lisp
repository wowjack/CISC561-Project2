;; (or a (and b c))
;;     => (and (or a b) (or a c))

(%dist-or-and-1 '(a)
                '(and (or b) (or c)))
