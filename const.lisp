
;;;; 定数の定義
;;;; どのパッケージからでも修飾子なしで使える(use-packageされる)


(defconstant +FORALL+ 'forall)
(defconstant +EXIST+  'exist)
(defconstant +QUANT+  `(,+EXIST+ ,+FORALL+))
(defconstant +IMPL+   '->)
(defconstant +NEG+    'n)
(defconstant +AND+    'a)
(defconstant +OR+     'o)
(defconstant +EQL+    '<->)
(defconstant +OPRT+   `(,+IMPL+ ,+NEG+ ,+AND+ ,+OR+ ,+EQL+))
(defconstant +OPERATORS+ (union +QUANT+ +OPRT+))

(defconstant +OPR-STRENGTH+
	`(
	 	(,+NEG+  . 1)
		(,+AND+  . 2)
		(,+OR+   . 2)
		(,+IMPL+ . 3)
		(,+EQL+  . 3)))


(defconstant +FORALL-S+ "∀")
(defconstant +EXIST-S+  "∃")
(defconstant +IMPL-S+   "⊃")
(defconstant +NEG-S+    "¬")
(defconstant +AND-S+    "∧")
(defconstant +OR-S+     "∨")
(defconstant +EQL-S+    "⇔")


(defconstant +FOR-TEX+
			 `(
			   (,+FORALL-S+ . " \\forall ")
			   (,+EXIST-S+  . " \\exists ")
			   (,+IMPL-S+   . " \\to ")
			   (,+NEG-S+    . " \\lnot ")
			   (,+AND-S+    . " \\land ")
			   (,+OR-S+     . " \\lor ")
			   (,+EQL-S+    . " \\Leftrightarrow ")))


(defconstant +STRING-EXPR+ 
	`(
		(,+EXIST+  . ,+EXIST-S+)
		(,+FORALL+ . ,+FORALL-S+)
		(,+IMPL+   . ,+IMPL-S+)
		(,+NEG+    . ,+NEG-S+)
		(,+AND+    . ,+AND-S+)
		(,+OR+     . ,+OR-S+)
		(,+EQL+    . ,+EQL-S+)))


(defconstant +NW+ "")
(defconstant +LB+ ")")
(defconstant +RB+ "(")
(defconstant +SP+ " ")
(defconstant +CM+ ",")


