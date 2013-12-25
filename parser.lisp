
;(load "./const.lisp")

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

	<VAR>        ::= <LISP-SYMBOL>
	<PRED-SYM>   ::= <LISP-SYMBOL>	
	<OPERATOR>   ::= > | & | V | ~ | -
	
	<QUANTIFIER> ::= A | E
	<QUANTS>     ::= <QUANTIFIER> <VAR>
	<QUANTS-PART>::= <QUANTS>+ "."

	<ATOMIC>     ::= <PRED-SYM> "(" <VAR>+ ")"
	<EXPR>       ::= 
		<ATOMIC> | "(" <EXPR> <OPERATOR> <EXPR> ")" | <QUANTS-PART> <EXPR> 

|#


(defconstant +OPERATOR+
			 `((#\~ ,+NEG+  1)
			   (#\& ,+AND+  2)
			   (#\V ,+OR+   2)
			   (#\> ,+IMPL+ 3)
			   (#\- ,+EQL+  3)))

(defconstant +QUANTS+ 
			 '(#\A #\E))


(defstruct (oper
			 (:constructor oper (ch)))
  		(ch nil :type character))


(defmethod strength ((oper oper))
  (car (last (assoc (oper-ch oper) +OPERATOR+))))


(defmethod str->in ((oper oper))
  (second (assoc (oper-ch oper) +OPERATOR+)))



(defun operator? (ch)
  (assoc ch +OPERATOR+))

(defun two-term-operator? (ch)
  (and (char/= ch #\~) (operator? ch)))

(defun qnt? (ch)
  (member ch +QUANTS+ :test #'char=))

(defun atomic? (str)
  (not (or (operator? (char str 0)) (qnt? (char str 0)))))

(defun scdr (str)
  (subseq str 1) )

(defun snull (str) 
  (string= str ""))

(defun appstr (a b) 
  (concatenate 'string a b))

(defun paren? (ch)
  (or (char= ch #\() (char= ch #\))))

;;;最後尾のインデックス
(defun li (str) 
  (1- (length str)))

;;; i番目の文字がcか否か
(defun nchar= (str n c)
  (char= (char str n) c))

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



;; 文字列 str を 二項演算子で分ける
(defun token (str)
  (labels
	((main (str paren result)
		(if (snull str) result
		  (let* ((head  (char str 0))
			   	 (heads (string head))
				 (next  (scdr str)))
			(cond 
			  ((and (char/= #\~ head) (operator? head))
			   (if (zerop paren) 
				 (if (snull result) heads result)
				 (main next paren (appstr result heads))))
			  ((paren? head)
			   (main next (if (char= head #\() (1+ paren) (1- paren)) (appstr result heads)))
			  (t 
				(main next paren (appstr result heads))))))))
	(main str 0 "")))


(defun tokenize (str)
  (labels 
	((main (str result)
	   (if (snull str) (reverse result)
		   (let ((tk (token str)))
			 (main 
			   (subseq str (length tk))
			   (cons (init tk) result))))))
	(main (init str) nil)))


(defun weak-operator (tklst)
 (let ((l  (sort 
	(remove-if-not 
		(lambda (x) (two-term-operator? (char x 0))) tklst)
	(lambda (x y)
	  (> (strength (oper (char x 0))) (strength (oper (char y 0))))))))
   ;; 左結合のほうがいいので
   (cond 
	 ((null l) l)
	 ((every 
		(lambda (x) 
		  (= (strength (oper (char (car l) 0)))
			 (strength (oper (char x 0))))) l) (reverse l))
	 (t l))))


(defun split (str spliter)
  (labels 
	((main (str acc  result)
	 (if (snull str) (reverse (cons acc result))
	 	(if (nchar= str 0 spliter)
	   		(main (scdr str) "" (cons acc result))
	   		(main (scdr str) 
			 (concatenate 'string acc (string (char str 0))) result)))))
	(main str "" nil)))

(defun atomic->in (str)
  (let ((len (position "(" str :test #'string=)))
	`(,(intern (subseq str 0 len)) 
	   ,@(mapcar (lambda (x) (intern (init x)))
				 (split (subseq str (1+ len) (1- (length str)))  #\,)))))

(defun quants->in (str)
  (labels 
	((q->in (q)
		(if (char= q #\A) 
		  +FORALL+ 
		  +EXIST+))
	 (next-q (str)
			 (position-if 
						(lambda (x)
			  				(or (string= x "~") 
								(string= x "A") 
								(string= x "E"))) str))
	 (main (str result)
		(if (snull str) (reverse result)
		  (let ((head (char str 0)))
	   (cond 
		 ((qnt? head)
		  (let* ((pos (next-q (scdr str)))
				 (p (if (null pos) (length str) (1+ pos))))
			(main 
			  (subseq str p) 
			  (cons 
				(list 
				  (q->in head) 
				  (intern  (init (subseq str 1 p))))  result))))
		 ((char= head #\~)
		  (let* ((m (+ 2 (next-q (scdr str))))
				 (pos (next-q (subseq str m)))
				 (p (if (null pos) (length str) (+ m pos))))
			(main 
			  (subseq str p)
			  (cons 
				(list +NEG+
					 (car (quants->in (subseq str 1 p))))
					 result))))
		  (t  (error "undefined operator")))))))
	(main (init str) nil)))


(defun concat (lst &key (s "") (f (lambda (x)x)))
  (reduce (lambda (x y) (concatenate 'String x s (funcall f y))) lst))


(defun concat-wrap-token (lst)
  (concat lst :f
		  (lambda (x)
			(if (two-term-operator? (char x 0))
			  x
			  (concatenate 'String "(" x ")")))))

(defun expr->in% (str)
 (let* ((tar (tokenize str))
		(oporder (weak-operator tar)))
	(if (null oporder)
	  (let ((expr (car tar)))
		(cond 
		  ((atomic? expr) 
		   (atomic->in expr))
		  ((char= (char expr 0)  #\~)
		   (list +NEG+ (expr->in% (scdr expr))))
		  (t (let ((sub (split expr #\.)))
			   (list (quants->in (car sub))
					 (expr->in% (concat (cdr sub) :s ".")) )))))
	  (let ((pos  (position (car oporder) tar :test #'string=)))
		 (list 
			(str->in (oper (char (car oporder) 0)))
			(expr->in% (concat-wrap-token (subseq tar 0 pos)))
			(expr->in% (concat-wrap-token (subseq tar (1+ pos)))))))))

