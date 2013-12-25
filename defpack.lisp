
;;;; パッケージを依存関係を考えながら
;;;; 定義し読み込む. 




(in-package :common-lisp-user)
(defpackage :constant.lexpr
	(:use :common-lisp)
	(:nicknames :const)
	(:export 
		:+FORALL+   :+EXIST+	
		:+QUANT+    :+IMPL+
		:+NEG+      :+AND+
		:+OR+       :+EQL+
		:+OPRT+     :+OPERATORS+
		:+FORALL-S+ :+EXIST-S+
		:+IMPL-S+   :+NEG-S+
		:+AND-S+    :+OR-S+
		:+EQL-S+    :+STRING-EXPR+
		:+NW+       :+LB+
		:+RB+       :+SP+
		:+CM+       :+OPR-STRENGTH+))
(in-package :const)
(load "./const.lisp")


(in-package :common-lisp-user)
(defpackage :util.lexpr
	(:use :common-lisp :const)
	(:nicknames :util)
	(:export 
		:quantsp      :opr-str
		:predexprp    :str-app
		:str-cut      :literalp
		:backetp      :quantsexprp
		:preproc
		:regular
	))
(in-package :util)
(load "./util.lisp")

(in-package :common-lisp-user)
(defpackage :convert2string.lexpr
	(:use :common-lisp :const)
	(:nicknames :dump)
	(:export :lexpr->string :dump-lexpr))
(in-package :dump)
(load "./dump.lisp")

(in-package :common-lisp-user)
(defpackage :inference.lexpr 
	(:use :common-lisp :const)
	(:nicknames :infer)
	(:export :semantic-conseq :partof))
(in-package :infer)
(load "./infer.lisp")


(in-package :common-lisp-user)
(defpackage :parser.lexpr 
	(:use :common-lisp :const)
	(:nicknames :parser)
	(:export :expr->in%
			 ))
(in-package :parser)
(load "./parser.lisp")
