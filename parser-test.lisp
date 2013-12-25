
(use :lisp-unit)

(load "./const.lisp")
(load "./parser.lisp")



(define-test expr->in%
	(let ((data 
			`(("~P(x)"
			   (,+NEG+ (P |x|)))
			  ("P(x) & ~P(x)" 
			   (,+AND+ (P |x|) (,+NEG+ (P |x|))))
			  ("P(x) > Q(x) & R(x)" 
			   (,+IMPL+ (P |x|) (,+AND+ (Q |x|) (R |x|))))
			  ("(P(x) > Q(x)) - R(x) & S(x)"
			   (,+EQL+ (,+IMPL+ (P |x|) (Q |x|)) (,+AND+ (R |x|) (S |x|))))
			  ("((AxAy.(P(x) > Q(x,y) V R(y))) > Ex.P(x) & Ex.Q(x))"
			   (,+IMPL+ (((,+FORALL+ |x|) (,+FORALL+ |y|)) (,+IMPL+ (P |x|) (,+OR+ (Q |x| |y|) (R |y|))))
				 (,+AND+ (((,+EXIST+ |x|)) (P |x|)) (((,+EXIST+ |x|)) (Q |x|)))))
			  ("P(x) & (P(x) > Q(x)) > Q(x)"
			   (,+IMPL+ (,+AND+ (P |x|) (,+IMPL+ (P |x|) (Q |x|))) (Q |x|)))
			  ("P(x) V Q(x) & ~P(x) > Q(x)"
			   (,+IMPL+ (,+AND+ (,+OR+ (P |x|) (Q |x|)) (,+NEG+ (P |x|)))  (Q |x|)))
			  )))

	  (dolist (each data)
		(destructuring-bind (test ans) each
		  (assert-equal ans (expr->in% test))))))


(print-errors (run-tests '(expr->in%)))


