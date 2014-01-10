

(load "./defpack.lisp")

(in-package :common-lisp-user)
(defpackage main
  (:use :common-lisp
		:const))
(in-package :main)


(defun credit ()
  (format t "Theorem Prover Beta 0.9~%"))


(defun read-one (prm)
  (format t "~A " prm)
  (force-output t)
  (multiple-value-bind
			(text flag) 
			(read-line t nil :eof)
			(cond 
	  			(flag 
					(format t "Bye!~%") 
					#+sbcl  (sb-ext:exit)
					#+clisp (ext:exit)
					)
				((string= text "") (read-one prm))
				(t text))))



;; 仮定の論理式の集合を読み込んで返す
(defun input () 
  (labels 
	((main (l)
		(let ((text (read-one "?")))
		  (cond 	
				((string= text "}") l)
				(t 
				  
				  (let ((expr (parser:expr->in% text)))
				  	(format t "inputted: ")
					(dump:dump-lexpr expr)
					(main (cons expr l)))
				  
				  )))))
	(main nil)))
  


(defun main ()
  (format t "~%Input set of wff { ~%")
  (force-output t)
  (ignore-errors 
	
	(let ((in (input))
		  (conseq (parser:expr->in% (read-one "conseq ?"))))
	  	
		(format t "inputted: ")
		(dump:dump-lexpr conseq)
		(format t "processing...~%")
	  	
	  	(let ((r 
				(infer:semantic-conseq in conseq)))

		  (when (and (typep r 'boolean) r (y-or-n-p "output to TeX? "))
			(infer:lexprs->tex   
			  `(,@in (,+NEG+ ,conseq))
			  (read-one "input filename: ")))))
	
	) 
  (main))

(credit)
(main)

