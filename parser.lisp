
#|

	一階述語論理の文字列表現の式を内部表現にする

	*内部表現の定義(あくまで内部表現のリストをそのまま表示した時のもの)
	
	<VAR>         ::= <SYMBOL>
	<PRED-SYM>    ::= <SYMBOL>	
	<OPERATOR>    ::= IMPL | AND | OR | NEG | EQL

	<QUANTIFIER>  ::= FORALL | EXIST
	<QUANTS>      ::= "(" <QUANTIFIER>  <VAR> ")"
	<QUANTS-PART> ::= "(" <QUANTS>+  ")"
	
	<ATOMIC>      ::= "(" <PRED-SYM>  <VAR>+ ")"
	<EXPR>        ::= 
		<ATOMIC> | "(" <OPERATOR>  <EXPR>+ ")" | "(" <QUANTS-PART> <EXPR> ")"

	@@ 関数記号ははじめに実装するときに忘れてしまったのでもうしゅうがない( @@


	*一階述語論理の文字列表現の定義

	<VAR>        ::= <SYMBOL>
	<PRED-SYM>   ::= <SYMBOL>	
	<OPERATOR>   ::= > | & | V | ~ | -
	
	<QUANTIFIER> ::= A | E
	<QUANTS>     ::= <QUANTIFIER> <VAR>
	<QUANTS-PART>::= <QUANTS>+ "."

	<ATOMIC>     ::= <PRED-SYM> "(" <VAR>+ ")"
	<EXPR>       ::= 
		<ATOMIC> | "(" <EXPR> <OPERATOR> <EXPR> ")" | <QUANTS-PART> <EXPR> 

|#


;;; 一般に中置記法は
;;; <EXPR> <OPERATOR> <EXPR>

;;; P(x)
;;; P(x,y,z)
;;;
;;; P(x) & Q(y)
;;;	P(x) -> (R(x,y) & Q(y))
;;;
;;;	AxAy.P(x,y)



;;; 文字列の演算子の定義
(defconstant +OPERATOR+ '(">" "&" "V" "~" "-"))

;;;最後尾のインデックス
(defun li (str) 
  (1- (length str)))


;;; i番目の文字がcか否か
(defun nchar= (str n c)
  (char= (char str n) c))

(defun scar (str) 
  (subseq str 0 1))


(defun scdr (str)
  (subseq str 1) )

(defun snull (str) 
  (string= str ""))



(defun innerparen? (str sc ec)
  ;; これが呼ばれたという事は少なくとも
  ;; 始めが ( で始まり 最後が ) で終わるような文字列であるということ
  (labels 
	((main (str acc)
		(cond
	   	((snull str)
			(if (zerop acc) 
		 	 	t 
		  		(error "parenthesis error!")))
	   	((zerop acc) nil)
	   	(t 
		 	(let ((head (char str 0)))
		   		(main (scdr str)
				 	(cond
				   	((char= head sc) (1+ acc))
				   	((char= head ec) (1- acc))
				   	(t acc)))))))) 
	(main (scdr str) 1)))

;;;最初と最後がカッコ(sc ecで文字コード)でなくなるまで、意味のないカッコを剥ぐ
(defun strip-paren (str sc ec)
  (cond 
	((or (not (nchar= str 0 sc)) 
		 (not (nchar= str (li str) ec))) str)
	((innerparen? str sc ec)
	 (strip-paren (subseq str 1 (li str)) sc ec))
	(t str)))


;;; ゴミのスペースを除去っていらないカッコを剥ぐ
(defun init (str &optional (sc #\() (ec #\))) 
  (if (snull str)
	str
	(strip-paren 
		(string-trim 
	  		'(#\space) str) sc ec)))


(defun opr (str)
  (member str +OPERATOR+ :test #'string=))



;; (P(x) & Q(y)) V R(x)
(defun token% (str)
  (labels 
	((main (str result acc)
		(if (snull str) result
			(let ((head (scar str)))
	  			(cond
				  ((opr head) 
				   (if (snull result) 
					 head 
					 (if (zerop acc) 
					   result 
					   (main (scdr str) (concatenate 'string result head) 
							 (cond 
							   ((string= head "(") (1+ acc))
							   ((string= head ")") (1- acc))
							   (t acc)
							   )))))
				  (t 
					(main (scdr str) 
						  (concatenate 'string result head) 
						  (cond 
							((string= head "(") (1+ acc))
							((string= head ")") (1- acc))
							(t acc))))
				  )
	  		)
		)
		)
	 )
	(main str "" 0))
  )


;;; token のリストにばらす
(defun token (str)
  ;; 演算子にぶち当たるor尽きるまで
  (labels 
	((main (str result)
		(if (snull str) (reverse result)
		  (let ((tk (token% str)))
			(print tk)
			(main 
			  (subseq str (length tk))
			  (cons (init tk) result)))))) (main (init str) nil)))




(print (token "(((((P(x) V R(y))) & Q(x))))"))






