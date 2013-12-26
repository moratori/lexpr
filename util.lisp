
;;;; いろいろと役に立つちょっとした関数を定義
;;;; 一般的なものと論理式関係のものを定義


;;; 一般的に使う関数


;;; (1 2 3 4 5) -> Boolean
;;; リストの要素が全てある型であるか
(defun allfactor (fun x)
	(and (listp x) 
		(reduce 
			(lambda (x y) (and x (funcall fun y))) x :initial-value t)))


;;; "abc" "def" -> "abcdef"
(defun str-app (&rest main) 
	(format nil "~{~A~}" main))


;;; "abcdefg," -> "abcdefg"
;;; 最後の文字列を落とす
(defun str-cut (x) (subseq x 0 (1- (length x))))


;;; 再帰的に要素を数えるだけ
(defun countr (factor lst &optional (test #'eq)) "FACTOR , (LST)"
	(cond 
		((null lst) 0)
		((listp (car lst)) (+ (countr factor (car lst)) (countr factor (cdr lst))))
		((funcall test (car lst) factor) (+ 1 (countr factor (cdr lst))))
		(t (countr factor (cdr lst)))))






;;; 論理式関連の関数



;;; forall -> "∀ "
;;; 論理結合子の文字列表現を得る
(defun opr-str (opr) (cdr (assoc opr +STRING-EXPR+)))


;;; (forall x) | (exist y) | (not (not (not (forall x)))) ... -> Boolean
;;; 量化子として正しかったら t を返す
(defun quantp (x)
	(and 
		(= (length x) 2)
		(cond 
			((eq (first x) +FORALL+) t)
			((eq (first x) +EXIST+) t)
			((eq (first x) +NEG+) (quantp (second x)))
			(t nil))))


;;; ((forall x) (not (exist y)) (not (not (forall z)))) | ... -> Boolean
;;; 量化子部分として正しかった t を返す
(defun quantsp (x)
	(and 
		(allfactor #'listp x)
		(reduce 
			(lambda (x y)
				(and x (quantp y))) x :initial-value t)))


;;; (P x y z) | (Q x (f y) z)... -> Boolean
;;; 述語論理の原子論理式なら t を返す. 
(defun predexprp (x)
	(and 
		(listp x) 
		(not (member (car x) +OPRT+))
		(reduce 
			(lambda (x y)
				(and x
					(if (symbolp y)
						t (predexprp y))))
			(cdr x) :initial-value t)))



;;; (P x) | (not (P x)) | (not (not (not (Q y)))) ... -> Boolean
;;; 原子論理式かその否定なら t を返す
(defun literalp (lexpr)
	(cond 
		((symbolp lexpr) nil)
		((quantsp (car lexpr)) nil)
		((predexprp lexpr) t)
		((eq (car lexpr) +NEG+) (literalp (second lexpr)))
		(t nil)))


;;; not -> 1
;;; 論理結合子の結合の強さをもとめる
(defun strength (opr)  (cdr (assoc opr +OPR-STRENGTH+)))


;;; (opr1 (opr2 ...) (opr2 ...) (opr2 ...))
;;; カッコをつけるべきか判定. 第一引数はopr1 第二引数はopr2
;;; 中身の結合子の方が弱かったらカッコをつける
(defun backetp (opr1 opr2)
	(>= (strength opr2) (strength opr1)))


;;; (((forall x) (exist y)) (-> (P x) (Q y))) -> Boolean
;;; 量化された論理式か否かを判定する
(defun quantsexprp (lexpr)
	;; (car lexpr) は量化部分か論理結合子になる
	(or (quantsp (car lexpr))))


;;; forall -> exist
;;; 量化子の否定した形の量化子を得る
(defun negquant (quant)
	(cond 
		((eq quant +FORALL+) +EXIST+)	
		((eq quant +EXIST+) +FORALL+)
		(t (error "undefined quantifire"))))

;;; 量化子つけるな
;;; 2n回の否定をとる かならず先頭は否定の式じゃないとだめ
(defun rid-even-neg (lexpr)
	(cond 
		((symbolp (second lexpr)) lexpr)	
		((not (eq +NEG+ (car (second lexpr)))) lexpr)
		(t (rid-even-neg (second (second lexpr))))))



;;; 含意を (or (not p) q) の形に変換する. 量化子つけるな
;;; (-> (P x) (Q x)) -> (or (not (P x)) (Q x))
(defun impl->or (lexpr)
	(destructuring-bind (opr p q) lexpr
	     (declare (ignore opr))
		`(,+OR+ (,+NEG+ ,p) ,q)))


;;; 同値を含意のandに変換する. 量化子つけるな
;;; (<-> p q) -> (and (-> p q) (-> q p))
(defun eq->impl (lexpr)
	(destructuring-bind (opr p q) lexpr
	     (declare (ignore opr))
		`(,+AND+ (,+IMPL+ ,p ,q) (,+IMPL+ ,q ,p))))


;;; 含意と同値の除去を再帰的に全て行う
;;; 量化子のついてるやつついてない奴どっちも
(defun rid-eq-impl (lexpr)
	;; (quants (->|and|or|<->|not ...))
	;; (->|and|or|<->|not ...)
	;(format t "ARG: ~A~%" lexpr)
	(destructuring-bind (head . tail) lexpr
		;; head は量化子部分か結合子
		;(format t "TAIL: ~A~%" tail)
		(cond 
			((quantsp head)
				;; head が量化子部分だったら
				;; 量化子部分そのまま
				(list head (rid-eq-impl (car tail))))
			;; こっちにきたら普通に結合子(か述語)
			((literalp lexpr) lexpr)
			((eq head +IMPL+) (cons +OR+ (mapcar #'rid-eq-impl (cdr (impl->or lexpr)))) )
			((eq head +EQL+)  (cons +AND+ (mapcar #'rid-eq-impl (cdr (eq->impl lexpr))))  )
			;; and と or と not
			(t (cons head (mapcar #'rid-eq-impl tail))))))


;;; 一つの量化子について否定を返す
;;; 但しnotがなければそのまま返す
;;; (forall x) -> (forall x)
;;; (not (exist x)) -> (forall x)
;;; (not (not (forall x))) -> (forall x) 
(defun contrquant (quant)
	(destructuring-bind (head snd) quant
	    (declare (ignore snd))
		(cond 
			((not (eq head +NEG+)) 
				(values quant nil))
			(t 
				(let ((clean (rid-even-neg quant)))
					(if (not (eq (car clean) +NEG+))
						(values clean nil)
						(values 
							(list 
								(negquant (car (second clean))) 
								(second (second clean))) t)))))))


;;; rid-quants-neg のヘルパ関数
;;; 先頭の否定形を追加する
(defun rid-quants-neg-1 (quants)
	(cons (list +NEG+ (car quants)) (cdr quants)))


;;; 量化子部分をとって否定でひっくり返した量化子部分
;;; ((forall x) (exist y) (not (forall z)) (exist z)) -> ((forall x) (exist y) (exist z) (forall w))
(defun rid-quants-neg (quants)
	(if (null quants)
		nil
		(multiple-value-bind (quant flag) (contrquant (car quants))
			(if flag
				(cons quant 
					(rid-quants-neg 
						;; 量化子部分と母式部分を別で考えてるからいけないんだとおもう
						;; これは最後の量化子だった時の処理
						(if (null (cdr quants)) 
							nil 
							(rid-quants-neg-1 (cdr quants)))))
				(cons quant (rid-quants-neg (cdr quants)))))))



;;; 量化子中の否定を除去る. 量化子のついた述語論理式
;;; (((forall x) (not (exist y))) (P x y)) -> (((forall x) (forall y)) (not (P x y)))
(defun rid-quants-neg-lexpr (lexpr)
	(destructuring-bind (quants body) lexpr
		(let ((clean-quants (rid-quants-neg quants))
		      (cnt (countr +NEG+ quants)))
			(list clean-quants
				(if (oddp cnt)
					`(,+NEG+ ,body) body)))))


;;; 論理式の中の全ての述語にかかる否定を取り除く
(defun rid-qneg (lexpr)
	;(format t "LEXPR: ~A~%" lexpr)
	(destructuring-bind (head . tail) lexpr
		;; (not P)
		;; head := not
		;; tail := (P)
		(cond 
			((quantsp head)
				;; lexpr は量化子のついた式だ!
				(let ((clean (rid-quants-neg-lexpr lexpr)))
					(list (car clean) (rid-qneg (second clean)))))
			((literalp lexpr) lexpr)
			((and (eq +NEG+ head) (quantsp (caar tail)))
				;; when head is negation
				;; like (not (((forall x) (exist y)) A))
				;; head := not
				;; tail := ((((forall x) (exist y)) A))
				(let ((body (car tail)))
					(destructuring-bind (quants main) body
						;; quants := ((forall x) (exist y))
						;; main := A
						(rid-qneg 
							(list 
								(cons (list +NEG+ (car quants)) (cdr quants)) main)))))
			(t 
				;; (->|or|and|not ...)
				(cons head (mapcar #'rid-qneg tail))
			))))



;;; ドモルガンで除去できる形式の論理式であるか判定
;;; リスト以外くるわけない
;;; (not (and|or ...)) -> t
(defun demorganp (lexpr)
	(cond 
		((not (eq (car lexpr) +NEG+)) nil)
		((or (eq +AND+ (car (second lexpr))) (eq +OR+ (car (second lexpr)))) t)
		(t nil)))

;;; ドモルガンの時に否定された結合子を返す
;;; and -> or , or -> and
(defun negopr (opr)
	(cond
		((eq opr +AND+) +OR+)
		((eq opr +OR+) +AND+)
		(t (error "and or or must be required"))))

;;; 量化子のつかない論理式のドモルガンで除去る
;;; (not (and|or ...)) -> (and|or (not ) (not ) ...)
(defun demorgan (lexpr)
	(cond 
		((demorganp lexpr) 
			(destructuring-bind (neg (opr . tail)) lexpr
			    (declare (ignore neg))
				;; neg := not
				;; opr := or | and
				;; tail := ((P x) (Q x) ...)
				(cons (negopr opr) (mapcar (lambda (x) (list +NEG+ x)) tail)))) (t lexpr)))


;;;　論理式から再帰的にドモルガンできるやつをしまくる
;;;  関数の形がrid-eql-implと統合するなり、抽象化したかったけど統合するのは無理だった
;;;  なぜなら, -> <-> を除去ると途中で not が出てきてしまってめんどいことになる
;;;  (((forall x)) (not (or (P x) (Q x)))) -> (((forall x)) (and (not (P x)) (not (Q x))))
(defun rid-demorgan (lexpr)
	;(format t "LEXPR: ~A~%" lexpr) 
	(destructuring-bind (head . tail) lexpr
		;(format t "HEAD: ~A~%TAIL: ~A~%" head tail)
		;; head := quants | opr
		;; tail ((not (P x) (Q x))) , ((-> (P x) (Q y)))
		(cond
			((quantsp head)
				(list head (rid-demorgan (car tail))))	
			;; こっちにきたら　普通に結合子とか 述語
			((literalp lexpr) lexpr)
			;; demorgan できるかたち
			((demorganp lexpr) 
				(let* ((clean (demorgan lexpr)) (opr (car clean)))
					(cons opr (mapcar #'rid-demorgan (cdr clean)))))
			(t 
				;;ドモルガンできない一般のかたち
				(cons (car lexpr) (mapcar #'rid-demorgan tail))))))


;;; 二重否定を再帰的に全て除去する
(defun rid-dn (lexpr)
	;(format t "LEXPR: ~A~%" lexpr) 
	(destructuring-bind (head . tail) lexpr
		;(format t "HEAD: ~A~%TAIL: ~A~%" head tail)
		(cond
			((quantsp head)
				(list head (rid-dn (car tail))))	
			;; こっちにきたら　普通に結合子とか 述語
			((eq +NEG+ head)
				(rid-even-neg lexpr))
			((literalp lexpr) lexpr)
			(t 
				;;先頭がnotでない一般のかたち
				(cons (car lexpr) (mapcar #'rid-dn tail))))))


;;; 論理式を綺麗にする
(defun regular-1 (lexpr)	
	(rid-qneg (rid-dn (rid-demorgan (rid-eq-impl (rid-qneg lexpr))))))


;;; reqular-1だけでは綺麗にならないので
;;; 繰り返すだけ. なんか効率悪いぞこれ
;;; 論理式の構造的にどの除去則をやった後にこれをやれば
;;; 必ず綺麗になるみたいなのないのかな
(defun regular (lexpr)
	(let ((clean (regular-1 lexpr)))
		(if (equal lexpr clean)
			lexpr
			(regular clean))))



;;; 論理式中の自由変数を返す
;;; regular されたものにして
;;; bound は量化子によって束縛されているもののリスト
;;; 関数記号とか考慮してない
;;; (((foall x) (exist y)) (-> (P x y) (Q a y))) -> (a)
(defun collect-free (lexpr &optional (bound nil))
	(destructuring-bind (head . tail) lexpr
		(cond 
			((quantsp head) 
				(collect-free 
					(car tail) 
					(append bound (mapcar (lambda (x) (second x)) head))))
			((literalp lexpr) 
				;; lexpr := (not (P x y))
				;; lexpr := (P x y)
				(if (eq +NEG+ head)
					(collect-free (car tail) bound)	
					(if (null bound) ;; 束縛変数のリストが0だったら
						tail 
						(reduce 
							(lambda (x y) 
								(if (null (member y bound))
									(cons y x) x)) tail :initial-value nil))))
			(t 
				;; それ以外の結合子
				;; (and|or|not|->|<-> ...)
				;; regular されてるから (and|or|not ...) のはずだけど
				(apply #'append (mapcar (lambda (x) (collect-free x bound)) tail))))))



;;; 論理式を正規化して自由変数のリストを二値で返す
(defun preproc (lexprs)
	(let ((clean (mapcar #'regular lexprs)))
		(values clean
			(reduce 
				#'union 
				(mapcar #'collect-free clean)
				:initial-value nil))))



