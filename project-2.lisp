;;;;;;;;;;;;;;;;
;;; UTILITIES ;;;
;;;;;;;;;;;;;;;;;

(defun fold (function initial-value list)
  "Convenience function for left-fold."
  (reduce function list :initial-value initial-value))

(defun boolean-implies (a b)
  (or (not a) b))

(defun boolean-iff (a b)
  (and (boolean-implies a b)
       (boolean-implies b a)))

(defun boolean-xor (a b)
  (and (or a b)
       (or (not a) (not b))))

(defun cons-new (element list)
  "Cons a new ELEMENT onto LIST.
If LIST already contains ELEMENT, LIST is returned unchanged"
  (if (some (lambda (x) (equal element x)) list)
      list
      (cons element list)))

(defun TODO (thing)
  "Placeholder for code to implement."
  (error "Unimplemented: ~A" thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPRESSION HELPERS ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun const-p (e)
  "Is expression a constant?"
  (or (eq e t)
      (null e)))

(defun var-p (e)
  "Is expression a variable?"
  (and (atom e)
       (not (const-p e))))

(defun and-p (e)
  "Is expression an AND?"
  (and (consp e)
       (eq (car e) 'and)))

(defun or-p (e)
  "Is expression an OR?"
  (and (consp e)
       (eq (car e) 'or)))

(defun not-p (e)
  "Is expression an NOT?"
  (and (consp e)
       (eq (car e) 'not)
       (null (cddr e))))

(defun lit-p (e)
  "Is expression a literal?"
  (or (var-p e)
      (and (not-p e)
           (var-p (second e)))))

(defun not-exp (exp)
  "Simplifying NOT of an expression."
  (if (not-p exp)
      (second exp) ; avoid the double negation
      `(not ,exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0: Conversion to CNF ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun nnf-p (e)
  "Is expression in negation normal form?"
  (labels ((visit (e)
             (or (const-p e)
                 (lit-p e)
                 (destructuring-bind (op &rest args) e
                   (case op
                     ((and or)
                      (every #'visit args))
                     (otherwise nil))))))
    (visit e)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0.a: Negation-Normal Form ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert boolean formula to negation-normal form:
;; * Only AND, OR, and NOT operators
;; * NOT is only applied to individual variables
;;
;;
;; Examples:
;; ---------
;;
;; (exp->nnf '(not (not a))) => a
;;
;; (exp->nnf '(not (and a b))) => (OR (NOT A) (NOT B))
;;
;; (exp->nnf '(not (or a (not b)))) => (AND (NOT A) B)
;;
;; (exp->nnf '(not (:iff a b))) =>
;;               (OR (AND A (NOT B)) (AND B (NOT A)))
;;
(defun exp->nnf (e)
  "Convert an expression to negation normal form."
  (labels ((base (e truth)
             (if truth
                 e
                 `(not ,e)))
           (visit (e truth)
             (if (var-p e)
                 (base e truth)
                 (destructuring-bind (op &rest args) e
                   (case op
                     (:implies
                      (destructuring-bind (a b) args
                        (TODO 'exp->nnf-implies)))
                     (:xor
                      (destructuring-bind (a b) args
                        (TODO 'exp->nnf-xor)))
                     (:iff
                      (destructuring-bind (a b) args
                        (TODO 'exp->nnf-iff)))
                     (not
                      (assert (and args (null (cdr args))))
                      (visit (car args) (not truth)))
                     (and
                      (TODO 'exp->nnf-and))
                     (or
                      (TODO 'exp->nnf-or))
                     (otherwise
                      (base e truth)))))))

    (visit e t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 0.b: Conjunctive-Normal Form ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cnf-p (e)
  "Is expression in conjunctive normal form?"
  (and (and-p e)
       (every (lambda (e) (and (or-p e)
                               (every #'lit-p (cdr e))))
              (cdr e))))

;; Sorting is useful to remove redundant (equivalent) clauses from
;; expressions
(defun sort-vars (vars)
  "Sort list of variables in lexicographic order."
  (assert (every #'var-p vars))
  (remove-duplicates
   (sort (copy-list vars)
         (lambda (x y) (string< (string x) (string y))))))

(defun maxterm (args)
  "Construct a Maxterm (an OR of literals).
Every argument must be a literal or (recursively) an OR of literals."
  (let (pos neg)
    (labels ((visit (e)
               (cond
                 ((var-p e)
                  (push e pos))
                 ((not-p e)
                  (assert (lit-p e))
                  (push (second e) neg))
                 ((or-p e)
                  (map nil #'visit (cdr e)))
                 (t
                  (error "Invalid exp: ~A" e)))))
      (map nil #'visit args))
    (if (intersection pos neg)
        t
        `(or ,@(sort-vars pos)
             ,@(map 'list #'not-exp (sort-vars neg))))))

(defun maxterm-p (x)
  "Is X a maxterm?"
  (and (consp x)
       (eq 'or (car x) )
       (every #'lit-p (cdr x))))

(defun maxterm-unit-p (x)
  "Is X a unit maxterm?
That is: (OR literal)"
  (assert (maxterm-p x))
  (= 1 (length (cdr x))))

(defun maxterm-false-p (x)
  "Is X false maxterm?
That is: (OR) or NIL"
  (assert (or (null x)
              (eq t x)
              (maxterm-p x)))
  (or (null x)
      (equal x '(or))))

(defun maxterm-true-p (x)
  "Is X true maxterm?
That is: T"
  (assert (or (eq t x)
              (maxterm-p x)))
  (eq t x))

;; Distribution examples:
;; ---------------------
;;
;; (or a (and b c))
;;     => (and (or a b) (or a c))
;;
;; (or x (and a b c))
;;     => (and (or x a) (or x b) (or x c))
;;
;; (or (or x y) (and b c))
;;      => (and (or x y b) (or x y c))
;;
;; (or (and x y) (and b c))
;;     => (and (or (and x y) b) (or (and x y) c))
;;     => (and (and (or b x) (or b y))
;;             (and (or c x) (or c y)))
;;     => (and (or b x) (or b y) (or c x) (or c y))
;;
;;
;; (or (and x y z) (and a b c))
;;     => (and (or x (and a b c))
;;             (or y (and a b c))
;;             (or z (and a b c)))
;;     => (and (and (or x a) (or x b) (or x c))
;;             (and (or y a) (or y b) (or y c))
;;             (and (or z a) (or z b) (or z c)))
;;     => (and (or x a) (or x b) (or x c)
;;             (or y a) (or y b) (or y c)
;;             (or z a) (or z b) (or z c))


;; Distribute literals over a single AND expression:
;;
;; (or lit-0 ... lit-1
;;     (and (or ...) (or ...) ...))
;;
;; The result is in conjunctive normal form
(defun %dist-or-and-1 (literals and-exp)
  (assert (every #'lit-p literals))
  (assert (cnf-p and-exp))
  `(or ,@literals ,and-exp)
  (TODO '%dist-or-and-1))

;; Distribute OR over two AND expressions:
;;
;; (or (and (or ...) (or ...) ...)
;;     (and (or ...) (or ...) ...))
;;
;; The result is in conjunctive normal form
(defun %dist-or-and-and (and-exp-1 and-exp-2)
  (assert (cnf-p and-exp-1))
  (assert (cnf-p and-exp-2))
  `(or ,and-exp-1 ,and-exp-2)
  (TODO '%dist-or-and-and))

;; Distribute n-ary OR over the AND arguments:
;;
;; (or lit-0 ... lit-n
;;     (and (or ...) (or ...) ...)
;;     ...
;;     (and (or ...) (or ...) ...))
;;
;; The result is in CNF
(defun %dist-or-and-n (literals and-exps)
  (assert (every #'lit-p literals))
  (assert (every #'cnf-p and-exps))
  (if literals
      (if and-exps
          (fold #'%dist-or-and-and
                  (%dist-or-and-1 literals (car and-exps))
                  (cdr and-exps))
          (let ((maxterm  (maxterm literals)))
            (if (maxterm-true-p maxterm)
                '(and)
                `(and ,maxterm))))
      (if and-exps
          ;; (or (and...))
          (reduce #'%dist-or-and-and and-exps)
          ;; (or)
          '(and (or)))))

(defun cnf-or (e)
  "Convert an NNF disjunction to CNF."
  (assert (nnf-p e))
  (assert (or-p e))
  (let ((literals)
        (and-args))
    (labels ((visit (e)
               (cond
                 ((lit-p e)
                  (push e literals))
                 ((or-p e)
                  (map nil #'visit (cdr e)))
                 ((and-p e)
                  (let ((e (cnf-and e)))
                    (assert (cnf-p e))
                    (push e and-args))))))
      (map nil #'visit (cdr e)))
    (%dist-or-and-n literals and-args)))

(defun cnf-and (e)
  "Convert an NNF conjunction to CNF."
  (assert (nnf-p e))
  (assert (and-p e))
  (labels ((visit (args e)
             (cond
               ((lit-p e)
                (cons `(or ,e) args))
               ((or-p e)
                (let ((e (cnf-or e)))
                  (assert (cnf-p e))
                  (append (cdr e)
                          args)))
               ((and-p e)
                (fold #'visit args (cdr e))))))
    (cons 'and (visit nil e))))

(defun nnf->cnf (e)
  "Convert an NNF expression to CNF."
  (assert (nnf-p e))
  (cond ((lit-p e) `(and (or ,e)))
        ((and-p e) (cnf-and e))
        ((or-p e) (cnf-or e))
        ((eq e t)
         '(and))
        ((null e)
         '(and (or)))
        (t (error "Invalid expression: ~A" e))))


(defun exp->cnf (e)
  "Convert an expression to conjunctive normal form."
  (nnf->cnf (exp->nnf e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1: DAVIS-PUTNAM-LOGEMANN-LOVELAND (DPLL) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-bindings (maxterms bindings)
  "Evaluate whether bindings satisify the maxterms.
Returns: (OR t nil)"
  (assert (every #'maxterm-p maxterms))
  (assert (every #'lit-p bindings))
  (every (lambda (maxterm)
           (some (lambda (arg)
                   (find arg bindings :test #'equal))
                 (cdr maxterm)))
         maxterms))

(defun maxterm-bind (maxterm literal)
"Bind a literal in maxterm.
Returns: new-maxterm"
  (assert (maxterm-p maxterm))
  (assert (lit-p literal))
  (let ((args (cdr maxterm)))
    (or (some (lambda (x) (equal x literal)) args)
        `(or ,@(remove (not-exp literal) args :test #'equal)))))

(defun dpll-bind (maxterms literal bindings)
"Bind a literal in the maxterms list.
Returns: (VALUES maxterms (CONS literal bindings))"
  (assert (every #'maxterm-p maxterms))
  (assert (lit-p literal))
  (assert (every #'lit-p bindings))
  (labels ((rec (new-terms rest)
             (if rest
                 (let ((new-term (maxterm-bind (car rest) literal)))
                   (cond
                     ((maxterm-false-p new-term)    ; short-circuit
                      (values (list new-term) nil))
                     ((eq t new-term)               ; cancel-out true term
                      (rec new-terms (cdr rest)))
                     (t                             ; add new term
                      (rec (cons new-term new-terms)
                           (cdr rest)))))
                 ;; end of terms
                 (values new-terms (cons literal bindings)))))
    (rec nil maxterms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1.a: Unit Propagation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dpll-unit-propagate (maxterms bindings)
"DPLL unit propogation.
Returns: (VALUES maxterms (LIST bindings-literals...))"
  (assert (every #'maxterm-p maxterms))
  (assert (every #'lit-p bindings))
  ;; HINT: use DPLL-BIND
  (TODO 'dpll-unit-propagate))


(defun dpll-choose-literal (maxterms)
  "Very simple implementation to choose a branching literal.
RETURNS: a literal"
  (assert (every #'maxterm-p maxterms))
  (let ((term (cadar maxterms)))
    (cond ((var-p term)
           term)
          ((not-p term)
           (assert (var-p (second term)))
           (second term))
          (t
           (error "Unrecognized thing: ~A" term)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Part 1.b: DPLL ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun dpll (maxterms bindings)
"Recursive DPLL routine.
Returns: (VALUES (OR T NIL) (LIST bindings-literals...))"
  (assert (every #'maxterm-p maxterms))
  (assert (every #'lit-p bindings))
  (multiple-value-bind (maxterms bindings)
      (dpll-unit-propagate maxterms bindings)
    (cond
      ((every #'maxterm-true-p maxterms) ; Base case: all maxterms canceled true, i.e., (AND)
       (values t bindings))
      ((some #'maxterm-false-p maxterms) ; Base case: some maxterm is false
       (values nil bindings))
      (t ; Recursive case
       (TODO 'dpll)))))

(defun sat-p (e)
  "Check satisfiability of e."
  (let ((maxterms (cdr (exp->cnf e))))
    (multiple-value-bind (is-sat bindings)
        (dpll maxterms nil)
      ;; sanity checking
      (when is-sat
        (assert (check-bindings maxterms bindings)))
      (values is-sat bindings))))
