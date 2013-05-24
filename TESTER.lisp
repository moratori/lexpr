
(load "defpack.lisp")
(use-package :const)



;; ソクラテスは死ぬか?
(format t "~A~%"
	(infer:semantic-conseq
		`(
			(((,+FORALL+ x)) (,+IMPL+ (human x) (mortal x)))
			(human sokrates))
		'(mortal sokrates) ))


;; { ∀ x∀ y.(∃ zH(y , z)) ⊃  H(x , y) , H(a , b)} |= ∀ z∀ w.H(z , w)  ? -> t
;; 論理学を作る の練習問題73から
;; {人は,人を憎む人を憎む , aさんはbさんを憎む} |= 全ての人は全ての人を憎む
;; の例
(format t "~A~%" 
	(infer:semantic-conseq 
		`(
			(((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (((,+EXIST+ z)) (H y z))  (H x y)))
			(H a b))
		`(((,+FORALL+ z) (,+FORALL+ w))(H z w))))



;; 論理学を作るの練習問題32の(5)
(format t "~A~%"
	(infer:semantic-conseq 
		`(
			(((,+FORALL+ x)) (,+IMPL+ (A x) (B x)))
			(((,+FORALL+ x)) (,+IMPL+ (C x) (D x)))
			(((,+FORALL+ x)) (,+IMPL+ (E x) (F x)))
			(((,+FORALL+ x)) (,+IMPL+ (,+NEG+ (G x)) (,+NEG+ (H x))))
			(((,+FORALL+ x)) (,+IMPL+ (B x) (I x)))
			(((,+FORALL+ x)) (,+IMPL+ (,+NEG+ (A x)) (,+NEG+ (J x))))
			(((,+FORALL+ x)) (,+IMPL+ (K x) (,+NEG+ (D x))))
			(((,+FORALL+ x)) (,+IMPL+ (,+NEG+ (H x)) (,+NEG+ (I x))))
			(((,+FORALL+ x)) (,+IMPL+ (,+NEG+ (J x)) (E x)))
			(((,+FORALL+ x)) (,+IMPL+ (G x) (C x))))
		 `(((,+FORALL+ x)) (,+IMPL+ (K x) (F x)))))



