
(load "defpack.lisp")
(use-package :const)

(defvar *test4*
	`(
		( 
			(((,+FORALL+ x)) (,+AND+ (P x) (,+NEG+ (P x)))))

		( 
			(((,+FORALL+ x)) (P x)) 
			(((,+EXIST+ y)) (,+NEG+ (P y))))

		(
			(((,+FORALL+ x)) (,+IMPL+ (P x) (Q x)))
			(((,+EXIST+ x)) (P x))
			(((,+NEG+ (,+EXIST+ x))) (Q x)))

		(
			(((,+FORALL+ x)) (,+OR+ (P x) (Q x) (R x)))	
			(((,+FORALL+ x)) (,+NEG+ (,+OR+ (P x) (Q x))))
			(,+NEG+ (((,+EXIST+ x)) (R x))))

		(
			(((,+FORALL+ x) (,+EXIST+ y)) (P x y))
			(((,+EXIST+ w) (,+FORALL+ z)) (,+NEG+ (P w z))))

		(
			(((,+FORALL+ x)) (,+IMPL+ (HAVE Jhon x) (WANTS Mike x)))
			(HAVE Jhon house)
			(,+NEG+ (WANTS Mike house)))))


(mapc #'infer:check-contrap *test4*)
