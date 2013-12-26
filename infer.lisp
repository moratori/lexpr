
;;;; 式集合 Γ から φ が意味論的帰結であるかを判定 Γ |= φ
;;;; 判定にはタブロー法を使う
;;;; Γ |= φ ならば Γ ∪  {¬ φ} |= 
;;;; 
;;;; - PROPOSITION  RULE -
;;;; 1. Depend clause (Γ) include φ and (not φ) then close branch
;;;; 2. Depend clause (Γ) include (or φ ψ)  then branch Γ ∪ {φ} and Γ ∪ {ψ} 
;;;; -3. Depend clause (Γ) include (-> φ ψ)  then branch Γ ∪ {¬ φ} and Γ ∪ {ψ}
;;;; 4. Depend clause (Γ) include (and φ ψ) then branch Γ ∪ {φ , ψ}
;;;;
;;;; - PREDICATE RULE -
;;;; 5. Depend clause (Γ) include (((forall x)) φ) then branch Γ ∪ {φ[α/x] , (((forall x)) φ)} 
;;;; 6. Depend clause (Γ) include (((exist x)) φ)  then branch Γ ∪ {φ[α/x]} * α mustn't use in depend clause
;;;; 
;;;; 
;;;; 述語論理の決定性の問題から、infer で使う述語論理式はMPLに限ろうと思う



;;;; ------------------------------------------------------------------
;;;; 方針
;;;; ①  util:rid-quants-neg-lexpr で量化子部分を綺麗にして
;;;; ②  util:rid-eq-implで同値と含意を除去る
;;;;
;;;; この時and or not だけを結合子として使うようになる
;;;;
;;;; ③  not が and , or にかからない様にドモルガンで変形する
;;;; ④  単一化とタブローのルールに従っていく
;;;; ------------------------------------------------------------------




;;; 量化子があるなら展開可能: unification
;;; 結合子が -> なら展開可能: (-> P Q)        => not P , Q
;;; 結合子が OR なら展開可能: (OR P Q ... Z)  => P , Q , ... Z
;;; 結合子が ANDなら展開可能: (AND P Q ... Z) => P Q ... Z
;;; 結合子が <->なら展開可能: (<-> P Q)       => (-> P Q) (-> Q P)
;;; リテラルであるなら展開不可能: (not (P x (f y z))) (P x) (Q x y (f z)) は展開不可能
;;; 
;;; 結合子が not で その次が -> OR AND <-> なら展開可能
;;; 但し展開しなければ行けない(not ...) に直接適用できる規則はない
;;;
;;; lexpr はタブロー展開規則を適用可能かどうか
;;; リテラルな論理式以外なら展開可能とする
(defun applyp (lexpr)
	(if (util:literalp lexpr) nil t))


;;; Qα.φ sym -> φ[sym / α]
;;; lexpr の外側の量化子をはがす. lexpr必ず量化子のついたもの
(defun unification (lexpr sym)
	;; lexpr は　(((forall x) (exist y) (forall z)) (-> (P x) (and (Q y) (R z))))
	;; みたいになってる. それ以外はしらない
	(let* ((quants-part (car lexpr)) 
			(head-quant (car quants-part))
			(body-part  (second lexpr))
			(rplc (subst sym (second head-quant) body-part)))
			;; quants-partに量化子部分をもってくる
			;; head-quantに 一番先頭の量化子一個を取ってくる
			;; body-partに述語論理の式のメインの部分を持ってくる
			;; rplcは置換した式
			(if (= 1 (length quants-part))
				rplc
				(list (cdr quants-part) rplc))))


;;; {P(x) , ¬ Q(x) } -> Boolean
;;; 全ての論理式が展開不可能かどうか 全ての式がリテラルであるか否か  
(defun all-literalp (lexprs) 
	(every (lambda (x) (not (applyp (car x)))) lexprs))


;;; 全称量化な式は単一化しても残るので
;;; どのシンボルで単一化したかを記憶してリストをもたせる
;;; 用に変形する
;;; regular されていると仮定
;;; プレースホルダ的な感じでforall以外にもつけてしまう
(defun forall-init (lexprs)
	(mapcar (lambda (x) (list x nil)) lexprs))


;;; forall-initでくっつけられたゴミみたいなnilをとりのぞくだけ
(defun rid-init (lexprs) 
	(mapcar (lambda (x) (car x)) lexprs))


;;; {¬ P(x) P(x)} -> t
;;; φの否定の形の式はlexprsに含まれているか
;;; O(n^2)でうける
;;; (((forall x)) (P x)) と (((exist y)) (not (P y))) が矛盾したものと判断されないぞー
(defun opposp-1 (lexpr lexprs)
	(let ((neg  (util:regular `(,+NEG+ ,lexpr))))
		;; lexprs のメタ情報を落とす
		(if (member neg (rid-init lexprs) :test #'equal) t nil)))

;;; 自分と否定の形の式は含まれているかどうか
;;; α同値な式に対応しなきゃ...
;;; とおもったけど、普通に量化子は除去られるのでいっか
(defun closep (lexprs)
	;; (car x) 取るのは 論理式の本体を得るため
	(some (lambda (x) (opposp-1 (car x) lexprs)) lexprs))


;;; lexprs := {(∃ x(P(x) -> Q(x)) nil) ...} みたな形式から先頭が存在量化な
;;; やつだけあつめる.(default)
(defun collect-exist (lexprs &optional (quants +EXIST+))
		(remove-if-not
			(lambda (x)
				(let* ((lexpr (car x)) (head (car lexpr)))
					(if (util:quantsp head)
						(eq (caar head) quants) nil))) lexprs))


;;; 存在量化の式のリストをもらって
;;; gensymで単一化した式とそのシンボルのリストを二値で返す
;;; {(∃ x(P(x) -> Q(x)) nil) , (∃ x(P(x)∧ Q(x)) nil)} -> {(P(a)->Q(a) nil) , (P(b)∧ Q(b) nil)}
(defun uni-exist (e-lexprs)
	(let ((exist-sym 
			(loop for x from 0 upto (1- (length e-lexprs)) 
				collect (gensym "RE-"))))
		(values 
			(map 'list 
				(lambda (expr sym) 
					(list (unification (car expr) sym)  nil)) 
				e-lexprs  exist-sym)  
			exist-sym)))


;;; 優先的にforallの除去をすべき式を降順に並べる
(defun sort-forall (forall-lexprs)
	(sort forall-lexprs (lambda (x y) (< (length (second x)) (length (second y))))))


;;; 量化子のついていない式を集める
(defun collect-nexpr (lexprs) 
	(remove-if
		(lambda (x)
			(util:quantsexprp (car x))) lexprs))


;;; (and|or p q r ...)みたいのを
;;; ((p nil) (q nil) (r nil) ...) にする
(defun split-sublexpr (lexpr)
	(mapcar (lambda (x) `(,x nil)) (cdr lexpr)))

;;; 量化子のついていない式で展開可能なものを集める
(defun select-normal (lexprs)	
	(find-if #'applyp lexprs))

;;; forall な式に使うべきシンボルを返す
(defun select-symbol (forall-usedsym usedsym)
	;; もしusedsymがnil だったら今までcontrap-mainを実行してて
	;; 一個も自由変数がないってことだから forall のためのシンボルはなるべく
	;; 使われてきた奴とかぶせる必要があるけど、致し方ない
	;; forall-usedsym は usedsym の部分集合
	(let ((tmp (set-difference usedsym forall-usedsym)))
		(cond 
			((null tmp)
				(values (gensym "RF-") t))
			(t (values (car tmp) nil)))))

(defun debug-print (lexprs usedsym)
	(format t "LEXPRS-SIZE: ~A~%ALL-USED-SYM: ~A~%------------------------------------------~%" 
		(length lexprs) usedsym)
	(loop for lexpr in lexprs for cont from 1 upto (length lexprs)  do 
		(format t "~A. ~%STRING: ~A~%USED-SYM: ~A~%" 
			cont (dump:lexpr->string (car lexpr)) (second lexpr)))
	(format t "------------------------------------------~%~%"))



(defconstant +LIMIT+ 100)
(defvar *return* nil)


;;; lexprs を構成する式の要素
;;; 1. 全称量化な式
;;; 2. 存在量化な式
;;; 3. 展開可能式 or and
;;; 4. リテラル
(defun contrap-main (lexprs usedsym &key (trc nil) (limit +LIMIT+) (strm nil) (nextcall nil))
	(when trc
	  (debug-print lexprs usedsym) (sleep 5))
	(cond
	    ;;探索のlimitに達したら
		((zerop limit)
		 (setf *return* t) 
		 (values nil t))
		;; 自分の否定の形の式が含まれていたら矛盾
		((closep lexprs) 
		 (values t t))
		;; すべてがリテラルで上の条件に合致しない、つまり
		;; 全ての式が自分の否定の形を含まないなら
		;; これ以上つくす手段はないので矛盾していない
		((all-literalp lexprs) 
		 (values nil t))
		(t
			;; ここのlet でlexprs を
			;; 構成する式を4種類(5)に分ける
			(let* ((falq-lexprs (sort-forall (collect-exist lexprs +FORALL+)))
					;; 全称量化な式	
				   (extq-lexprs (collect-exist lexprs +EXIST+))
				    ;; 存在量化な式
				   (nrml-lexprs (collect-nexpr lexprs))
				    ;; リテラルと展開可能式をあわせたやつ
				   (ltrl-lexprs (remove-if (lambda (x) (applyp (car x))) nrml-lexprs))
				    ;; リテラル. もうこいつはいじれない
				   (extb-lexprs (remove-if (lambda (x) (not (applyp (car x)))) nrml-lexprs)))
					(cond 
						((not (null extq-lexprs))
							;; 存在量化をぶっ潰すそしてまた回す
							(multiple-value-bind 
								(rid-extq-lexprs syms) 
								(uni-exist extq-lexprs)
								(contrap-main 
									(append 
										falq-lexprs 
										rid-extq-lexprs
										nrml-lexprs) 
									(append syms usedsym) 
									:trc trc 
									:limit (1- limit) 
									:strm strm
									:nextcall nil)))
						((not (null extb-lexprs))
							;; ここにきたなら 少なくとも extq-lexprs はnilのはず
							;; なぜなら上で徹底的に取り除かれるから
							(let* ((target (car extb-lexprs))
								   ;; 展開可能な式の一つを選ぶ
								   (other-extb-lexprs (cdr extb-lexprs))
								   ;; target 以外の展開可能式
								   (main   (car target))
								   ;; body となる部分を取り出す
								   (opr    (car main))
								   ;; body の結合子を取り出す
								   (next-base (append falq-lexprs ltrl-lexprs other-extb-lexprs)))
								   ;; forall literal targetでない展開可能式はそのままもちこす
								   (cond 
								   		((eq opr +AND+)
											(contrap-main 
												(append 
													(split-sublexpr main) next-base) 
												usedsym 
												:trc trc 
												:limit (1- limit) 
												:strm strm
												:nextcall nil))
										((eq opr +OR+)
											(every 
												(lambda (x)
												   	
													(if strm 
													  (progn
													
														;;; strm が真 つまり tex への出力を行う場合は
														;;; drawable であることが確かに確かめられているはずなので
														;;; *return* が真に成ることはないからスワップする必要はない
														
														(format strm "\\chunk{~%")

														(multiple-value-bind 
													  		(result next?)
													  		(contrap-main 
																`(,@next-base ,x) 
																 usedsym 
														 		 :trc trc 
														 		 :limit (1- limit) 
														 		 :strm nil
														 		 :nextcall t)

															(print next?)

															(if next? 
															  (format strm "~A}" 
																(instr->tex (dump:lexpr->string (first x))))
															  (format strm "\\begin{bundle}{~A}"
																(instr->tex (dump:lexpr->string (first x)))))

															(contrap-main 
																`(,@next-base ,x) 
																 usedsym 
														 		 :trc trc 
														 		 :limit (1- limit) 
														 		 :strm strm
														 		 :nextcall nextcall)
														
															(unless next? 
															  (format strm "\\end{bundle}}"))

															result
													  
														)


														)
													  (progn 
														(contrap-main 
																`(,@next-base ,x) 
																 usedsym 
														 		 :trc trc 
														 		 :limit (1- limit) 
														 		 :strm strm
														 		 :nextcall nextcall))))

												(split-sublexpr main)))
										(t (error "undefined operator: ~A" opr)))))
						((not (null falq-lexprs))
							(let*  ((target (car falq-lexprs)) 
									;; ターゲットとなる全称量化な式が入る
									;; (∀ xP(x) used)
								    (main (car target))
									;; 式自体を取り出す
									;; ∀ xP(x)
									(falq-usedsym (second target))
									;; 今までに使われたのを取り出す 
									;; used
									(other-falq-lexprs (cdr falq-lexprs)))
									;; その他の全称量化単一化可能式
								(multiple-value-bind (sym flag) (select-symbol falq-usedsym usedsym)
									(let*  ((bound  (cons sym falq-usedsym))
										   	(unifed (list (unification main sym) nil))
											(update (list main bound))
											(next `(,@other-falq-lexprs ,update ,unifed ,@nrml-lexprs)))
										(if flag
											(contrap-main 
											  next 
											  (cons sym usedsym) 
											  :trc trc 
											  :limit (1- limit) 
											  :strm strm
											  :nextcall nil)
											(contrap-main 
											  next 
											  usedsym 
											  :trc trc 
											  :limit (1- limit) 
											  :strm strm
											  :nextcall nil))))))
						(t (error "unexpected error: ~A~%" lexprs)))))))


;;; contrap-main へのインターフェース
;;; こいつの引数の strm は tex 出力のため
(defun contrap (lexprs &optional (trc nil) (strm nil))
    (setf *return* nil)
	(multiple-value-bind 
		(clean-lexprs init-free-value) (util:preproc lexprs)
		(when strm
		  (first-write lexprs strm))
		(values
		  (contrap-main 
		  	(forall-init  clean-lexprs) 
		  	init-free-value 
		  	:trc trc :strm strm)
		  *return*)))



(defun check-contrap (lexprs &optional (trc nil))
	(format t "{")
	(mapc (lambda (x) (dump:dump-lexpr x :template "~A , ")) lexprs)
	(multiple-value-bind (ctr? returned?) (contrap lexprs trc)
       (format t 
		(cond 
		  (returned? "} is undeterminable~%~%")
		  (ctr?  "} is contradiction~%~%" )
		  (t "} is satisfiable~%~%"))) (values ctr? returned?)))


;; lexpr は lexprs からの 意味論的帰結となるか
(defun semantic-conseq (lexprs lexpr &optional (trc nil) (ishow t))
	(let ((target `(,@lexprs (,+NEG+ ,lexpr))))
      (if ishow 
		(multiple-value-bind (ctr? returned?) (check-contrap target trc)
		  (if returned? -1 ctr?))
        (multiple-value-bind (ctr? returned?) (contrap target trc)
		  (if returned? -1 ctr?)))))
 








;;; 
;;; 
;;; パッケージ相互依存の関係で調整するのめんどいので(asdf 使うしかないかな)
;;; 用途は違うけどここに書かざるを得ないコードが以下(もう！Lispなんか嫌い！)
;;; 

(defun instr->tex (str &optional (result ""))
  (cond 
	((string= str "") 
	 (concatenate 'string "$" result "$"))
	((not (null (assoc (subseq str 0 1) +FOR-TEX+ :test #'string=)))
	 (instr->tex 
	   (subseq str 1 ) 
	   (concatenate 'string result (cdr (assoc (subseq str 0 1) +FOR-TEX+ :test #'string=))) ))
	(t 
	  (instr->tex (subseq str 1) (concatenate 'string result (subseq str 0 1))))))



(defun init-tex (strm which)
  (format 
	strm 
	"\\documentclass{jarticle}~%\\usepackage{ecltree,epic}~%\\begin{document}~%~A~%" 
	which))

(defun finalize-tex (strm)
  (format 
	strm
	"\\end{bundle}~%\\end{document}"))


(defun first-write (lexprs strm)
  (format 
	strm
	"\\begin{bundle}{~%\\begin{tabular}{c}~%~{~A~^\\\\~%~}\\end{tabular}}"
	(mapcar 
	  (lambda (x)
		(instr->tex (dump:lexpr->string x))) lexprs)))



(defun chunk-start (lexpr strm)
  (format 
	strm
	"\\chunk{~%\\begin{bundle}{~A}" 
	(instr->tex (dump:lexpr->string lexpr))))

(defun chunk-end (strm)
  (format strm "\\end{bundle}}"))


(defun start-node (strm)
  (format strm "\\chunk{~%"))


(defun start-inner-node (strm lexpr)
  (format strm "\\begin{bundle}{~A}" (instr->tex (dump:lexpr->string lexpr))))

(defun end-node (strm)
  
  )


(defun drawable? (lexprs)
  (multiple-value-bind (ctr? returned?) (contrap lexprs)
	(declare (ignore ctr?))
	(values (null returned?) ctr?)))

(defun lexprs->tex (lexprs filename)
  (multiple-value-bind (flag which) (drawable? lexprs)
	(if flag
		(with-open-file (out filename :direction :output :if-exists :supersede)
	  		(init-tex out (if which "\\Large{Contradiction}\\\\\\\\" "\\Large{Satisfiable}\\\\\\\\"))
	  		(contrap lexprs nil out)
	  		(finalize-tex out))	nil)))




