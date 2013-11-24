
(load "defpack.lisp")
(use-package :const)


(defvar test
  `(
	;;; {∀ x∀ y(love(x,y)∧ love(y,x) -> happy(x)∧ happy(y)) , love(a,b) , love(b,a)} |= ∃ x(happy(x))
	(((((,+FORALL+ x) (,+FORALL+ y)) 
       (,+IMPL+ (,+AND+ (love x y) (love y x)) (,+AND+ (happy x) (happy y))))
      (love a b)
      (love b a))

	(((,+EXIST+ x)) (happy x)))


	;;; {∀ x(human(x) -> mortal(x)) , human(sokrates)} |= mortal(sokrates)
	(((((,+FORALL+ x)) (,+IMPL+ (human x) (mortal x)))
	  (human sokrates))
	 (mortal sokrates))


	;;; {∀ x(have(Jhon , x) -> wants(Mike , x)) , have(Jhon , PC)} |= wants(Mike , PC)
	(((((,+FORALL+ x)) (,+IMPL+ (HAVE Jhon x) (WANTS Mike x)))
	  (HAVE Jhon PC))
	 (WANTS Mike PC))


	;;; {∀ x∀ y(∃ zH(y , z)) ->  H(x , y) , H(a , b)} |= ∀ z∀ w.H(z , w)
	(((((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (((,+EXIST+ z)) (H y z))  (H x y)))
	  (H a b))
	 (((,+FORALL+ z) (,+FORALL+ w))(H z w)))))



(loop for x in test do 
	  (format t "~A~%" (apply #'infer:semantic-conseq x)))


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




;;; 適当に定理と証明を２つ

(format t "~A~%" 
        (infer:semantic-conseq 
		  nil 
		  `( ((,+FORALL+ x)) (,+IMPL+ (,+IMPL+ (,+IMPL+ (P x) (Q x)) (P x)) (P x)))))


(format t "~A~%" 
		(infer:semantic-conseq 
		  `((,+AND+ (,+OR+ (,+NEG+ (P a)) (Q a)) (,+OR+ (,+NEG+ (Q a)) (P a)))) 
		  `(,+EQL+ (P a) (Q a))))


(format t "~A~%" 
		(infer:semantic-conseq 
		  `( (,+EQL+ (P a) (Q a))) 
		  `(,+AND+ (,+OR+ (,+NEG+ (P a)) (Q a)) (,+OR+ (,+NEG+ (Q a)) (P a))) ))





#|
;;; 矛盾してるはずだけどいつまでたってもおわらないよーな例
;;; この手の例もうまく行くように頑張っては見たけどどうもうまくいかなかった
;;; 方針としてはリテラルの否定を含む式を優先するやり方
(format t "DANGER START~%")
(format t "~A~%"
		(infer:semantic-conseq 
		  `(
			(((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (f x y) (m x)))
			;(((,+FORALL+ x) (,+FORALL+ y) (,+FORALL+ z)) 
			; (,+IMPL+ (,+AND+ (f x y) (f z x)) (gf z y)))
			(((,+FORALL+ x) (,+FORALL+ y)) 
			 (,+IMPL+ (gf x y) (((,+EXIST+ z)) (,+AND+ (f x z) (f z y))))))
		  	`(((,+FORALL+ x) (,+FORALL+ y)) (,+IMPL+ (gf x y) (m x))) t))

|#

