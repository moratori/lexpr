
(load "defpack.lisp")
(use-package :const)


#|
(format t "~A~%" 
        (infer:semantic-conseq 
          `(
            (((,+FORALL+ x) (,+FORALL+ y)) 
             (,+IMPL+ (,+AND+ (love x y) (love y x)) (,+AND+ (happy x) (happy y))))
            (love a b)
            (love b a))
            `(((,+EXIST+ x)) (happy x))))


;; ソクラテスは死ぬか?
(format t "~A~%"
	(infer:semantic-conseq
		`(
			(((,+FORALL+ x)) (,+IMPL+ (human x) (mortal x)))
			(human sokrates))
		'(mortal sokrates)))


;; Mike は Jhon が持っているものをなんでも欲しがる
;; Jhon は PC を持っている Mike は PC を欲しがるか?
(format t "~A~%"
	(infer:semantic-conseq 
		`(
			(((,+FORALL+ x)) (,+IMPL+ (HAVE Jhon x) (WANTS Mike x)))
			(HAVE Jhon PC))
		'(WANTS Mike PC)))


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
|#


;;; 矛盾してるはずだけどいつまでたってもおわらないよー
(format t "~A~%"
		(infer:semantic-conseq 
		  `(
			(((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (f x y) (m x)))
			(((,+FORALL+ x) (,+FORALL+ y) (,+FORALL+ z)) 
			 (,+IMPL+ (,+AND+ (f x y) (f z x)) (gf z y)))
			(((,+FORALL+ x) (,+FORALL+ y)) 
			 (,+IMPL+ (gf x y) (((,+EXIST+ z)) (,+AND+ (f x z) (f z y))))))
		  	`(((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (gf x y) (m x))) t nil))

; (p->q)->p->q

#|
(format t "~A~%" 
        (infer:semantic-conseq nil `( ((,+FORALL+ x)) (,+IMPL+ (,+IMPL+ (,+IMPL+ (P x) (Q x)) (P x)) (P x)))))



(format t "~A~%" 
		(infer:semantic-conseq `((,+AND+ (,+OR+ (,+NEG+ (P a)) (Q a)) (,+OR+ (,+NEG+ (Q a)) (P a)))) `(,+EQL+ (P a) (Q a))))


(format t "~A~%" 
		(infer:semantic-conseq `( (,+EQL+ (P a) (Q a))) `(,+AND+ (,+OR+ (,+NEG+ (P a)) (Q a)) (,+OR+ (,+NEG+ (Q a)) (P a))) ))

|#
