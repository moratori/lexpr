
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
				   	 (subseq str 0 (position "." str :test #'string=))))
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
;;; 量化子 と　母式部の２つに分けてくれればいいのに
;;; 母式の仲間でtokenizeされるからかなりめんどいことになってしまった
;;; expr->in%が
;;; あとで書きなおし
(defun tokenize (str)
  ;; 演算子にぶち当たるor尽きるまで
  (labels 
	((main (str result)
		(if (snull str) (reverse result)
		  (let ((tk (token% str)))
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


(defun quantsp? (str)
  (cond 
	((snull str) nil)
	((not (null (position "." str :test #'string=))) nil)
	((nchar= str 0 #\~) (quantsp? (scdr str)))
	((or (nchar= str 0 #\A) (nchar= str 0 #\E)) t)
	(t nil)))


;; "AxAy~Ez" -> ((+FORALL+ x) (+FORALL+ y) (+NEG+ (+EXIST+ z)))
;; "~EyAz"
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
		 ((qnt head)
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

(defconstant +OPR-in+ 
			 `((">" . ,+IMPL+)
			   ("~" . ,+NEG+)
			   ("&" . ,+AND+)
			   ("V" . ,+OR+)
			   ("-" . ,+EQL+)))

(defun opr->in (str)
  (cdr (assoc str +OPR-IN+ :test #'string=)))


;; toriaezu ryoukasi no nai siki de kangaeru
(defun expr->in% (str)
  ;; <EXPR> <OPERATOR> <EXPR>
	(labels
	  ((main (tks)
		(if (= 1 (length tks))
	  		(if (atomic? (car tks))
				(atomic->in (car tks))
				(expr->in% (car tks)))
	  		(let ((left (first tks)))
				(if (string= left "~")
				  (list +NEG+ (main (cdr tks)))
				  (if (quantsp? left)
		  			(list (quants->in left) 
						  (main (cdr tks)))
		  			(list (opr->in (second tks)) 
						  (expr->in% left) 
						  (main (nthcdr 2 tks))))
				  )	
			  ))))
	  (main (tokenize str))))

