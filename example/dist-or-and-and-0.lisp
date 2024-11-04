;; (or (and x y) (and b c))
;;     => (and (or (and x y) b) (or (and x y) c))
;;     => (and (and (or b x) (or b y))
;;             (and (or c x) (or c y)))
;;     => (and (or b x) (or b y) (or c x) (or c y))

(%dist-or-and-and '(and (or x) (or y))
                  '(and (or b) (or c)))
