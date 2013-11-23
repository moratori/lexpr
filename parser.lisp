
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


(load "./const.lisp")

;;; 文字列の演算子の定義
(defconstant +OPERATOR+ 
			 '(#\> #\& #\V #\~ #\-))

(defconstant +QUANTS+ 
			 '(#\A #\E))

(defconstant +SC+ #\()
(defconstant +EC+ #\))


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






(defun next-paren-acc (acc c sc ec)
  (cond 
	((char= c sc) (1+ acc))
	((char= c ec) (1- acc))
	(t acc)))


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
					  (next-paren-acc acc head sc ec))))))) 
	(main (scdr str) 1)))


;;;最初と最後がカッコ(sc ecで文字コード)でなくなるまで、意味のないカッコを剥ぐ
(defun strip-paren (str sc ec)
  (cond 
	((or (not (nchar= str 0 sc)) 
		 (not (nchar= str (li str) ec))) str)
	((innerparen? str sc ec)
	 (strip-paren (subseq str 1 (li str)) sc ec))
	(t str)))


;;; ゴミのスペースを除去っていらないカッコ(sc ec)を剥ぐ
(defun init (str &optional (sc #\() (ec #\))) 
  (if (snull str)
	str
	(strip-paren 
		(string-trim 
	  		'(#\space) str) sc ec)))


(defun opr (c)
  (member c +OPERATOR+ :test #'char=))

(defun qnt (c) 
  (member c +QUANTS+ :test #'char=))


;; (P(x) & Q(y)) V R(x)
(defun token% (str &optional (sc #\() (ec #\)))
  (labels 
	((main (str result acc)
		   (print str)
		(if (snull str) result
			(let* ((head (char str 0)) 
				   (heads (string head)))
	  			(cond
				  ((qnt head)
				   ;; ドットで区切られてるので
				   ;; sokomade syutoku
				   (if (not (zerop acc))
					 (main (scdr str)
						   (concatenate 'string result heads)
						   acc)

				   (subseq str 0 (position "." str :test #'string=))
					 )
				   
				   )
				  ((opr head) 
				   (if (snull result) 
					 heads
					 (if (zerop acc) 
					   result 
					   (main (scdr str) 
							 (concatenate 'string result heads) 
							 (next-paren-acc acc head sc ec)))))
				  (t 
					(main (scdr str) 
						  (concatenate 'string result heads) 
						  (next-paren-acc acc head sc ec))))))))
	(main str "" 0)))
  


;;; token のリストにばらす
(defun tokenize (str)
  ;; 演算子にぶち当たるor尽きるまで
  (labels 
	((main (str result)
		(if (snull str) (reverse result)
		  (let ((tk (token% str)))
			(print tk)
			(main 
			 (init (subseq str 
					  ;; dot wo dounika suru
					  (if (qnt (char tk 0))
						(+ 1 (length tk)) 
						(length tk)))) 
			  (cons (init tk) result)))))) (main (init str) nil)))




(defun split (str spliter)
  (labels 
	((main (str acc  result)
	 (if (snull str) (reverse (cons acc result))
	 	(if (nchar= str 0 spliter)
	   		(main (scdr str) "" (cons acc result))
	   		(main (scdr str) 
			 (concatenate 'string acc (string (char str 0))) result)))))
	(main str "" nil)))

(defun atomic? (str)
  (not (or (opr (char str 0)) (qnt (char str 0)))))


;; "P(x,y,z,...)" -> (P x y z ...)
(defun atomic->in (str)
  (let ((len (position "(" str :test #'string=)))
	`(,(intern (subseq str 0 len)) 
	   ,@(mapcar (lambda (x) (intern (init x)))
				 (split (subseq str (1+ len) (1- (length str)))  #\,)))))



