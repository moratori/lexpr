
;;;; 述語論理式の内部表現を文字列にして返す関数を提供する
;;;;
;;;; - 論理式の再帰的定義 -
;;;;
;;;; def. 述語は任意の長さのシンボルで良い
;;;; 
;;;; def. 項は　任意の長さのシンボルで良い
;;;;
;;;; def. 任意の変数,定数は項である
;;;;
;;;; def. 1個以上の項をとる関数は項である
;;;;
;;;;
;;;; def. 1個以上の項をとる述語を原子論理式とする
;;;;  
;;;; def. Aを任意の論理式とする時
;;;; 	 (not A) 
;;;; 	 は論理式である
;;;;
;;;; def. A,Bを任意の論理式とする時
;;;; 	 (and A B) (OR A B) (-> A B) (<-> A B)
;;;; 	 は論理式である
;;;;
;;;; def. Aを任意の論理式とする時
;;;;      (((forall 変項)) A) (((exist 変項)) A)
;;;;      は論理式である
;;;;
;;;; ※ 上記の定義では多重量化な式も生成されるが
;;;;   推論の決定性の関係上MPLに限られる可能性がある
;;;;   但しdumpでは多重量化も許すことにする
;;;;   つまりdump自体は上の定義の完全な述語論理式を扱えることにする
;;;;
;;;; ((量化子1 量化子2 ...) 式)


;;; (forall x) -> ∀ x  (exist y) -> ∃ y  (not (not (forall x))) -> ¬ ¬ ∀ x
;;; 最も簡単なパターンの量化子の変換
(defun quant->string (quant)
	(let* ((head (car quant)) 
		   (tail (second quant)) 
		   (head-str (util:opr-str head)))
		;; head は forall exist not の何れか
		;; tail は x,y,z 等の変数か head が not の時は量化子
		(if (eq head +NEG+)
			(util:str-app head-str (quant->string tail))
			(util:str-app head-str (symbol-name tail)))))


;;; ((forall x) (not (exist y)) (forall z)) | ... -> "∀ x¬ ∃ y∀ z"
;;; 量化子部分だけを文字列にして返す
(defun quants->string (quants)
	(reduce 
		(lambda (x y)
			(util:str-app x (quant->string y))) 
			quants :initial-value ""))


;;; (P x y z) | ... -> "P(x,y,z)"  (Q x y (f z)) | ... -> "Q(x,y,f(z))"
;;; 述語論理の原子論理式を文字列にする
(defun predexpr->string (predexpr)
	(util:str-app
		(util:str-cut 
			(reduce 
				(lambda (x y)
					(util:str-app x 
						(if (symbolp y) (symbol-name y) (predexpr->string y)) ","))
				(cdr predexpr)
					:initial-value (util:str-app (symbol-name (car predexpr)) "("))) ")"))


;;; lexpr->string のヘルパ. 適切にカッコをつけるか判断する
(defun lexpr->string-1 (head lexpr)
	(let ((opr (car lexpr)))
		(cond 
			((util:quantsexprp lexpr) 
				;; 量化子のついた論理式はカッコをつけよう
				(util:str-app "(" (lexpr->string lexpr) ")"))
			((util:literalp lexpr)
				;; リテラルだったらそのままにしよう
				(lexpr->string lexpr))
			((util:backetp head opr) 
				;; backetp が　t だったらカッコをつける 
				(util:str-app "(" (lexpr->string lexpr) ")"))
			(t (lexpr->string lexpr)))))


;;: (((forall x) (not (exist y))) (-> (P x) (Q y))) | ... -> "∀ x¬ ∃ y(P(x) -> Q(y))"
;;; 述語論理の式を受け取って文字列で返す
(defun lexpr->string (lexpr)
	(let ((head (first lexpr))) 		
		;; head は量化子部分もしくは not and or -> <-> P,Q.. の何れか
		(cond 
			((util:quantsp head)
				;; 量化子部分を文字列に直して、母式の文字列表現と連結 
				(util:str-app 
					(quants->string head) "." (lexpr->string (second lexpr))))
			((eq +NEG+ head)
				;; 演算子がNEGなら被演算子部分は必ず一つなので(second lexpr)
				;; はメインの論理式. bodyに束縛
				;; 演算子がなんであれリテラル以外は必ずカッコはつけるべき
				(let ((body (second lexpr)) (neg-str (util:opr-str +NEG+)))
					(cond
					 	((util:literalp body)
							;; リテラルならそのままnotをつけてよし
							(util:str-app neg-str (lexpr->string body)))
						((util:quantsp (car body))
							;; bodyに量化子がついてたときはそのままnotにしよう... うーんどうしよう悩み中
							(util:str-app neg-str  (lexpr->string body) ))
						(t 
							;; それ以外はかならずカッコにくるもう
							(util:str-app neg-str "(" (lexpr->string body) ")")))))
			((util:predexprp lexpr)
				;; 原子論理式を文字列に	
				(predexpr->string lexpr))	
			(t 
				;; 普通の論理結合子に関する変換
				(util:str-cut 
					(reduce 
						(lambda (x y)
							(util:str-app x 
								;; y は任意の論理式が入ってるので
								;; 述語論理の式の場合は無条件でカッコでくるむ
								;; backetp が t と言う場合もカッコでくるむ 	
								;; lexpr->string-1 がそれを判断
								(lexpr->string-1 head y)
								(util:opr-str head)))
						(cdr lexpr) :initial-value ""))))))


(defun dump-lexpr (lexpr &key (strm t) (template "~A~%"))
  (format strm template (lexpr->string lexpr)))



