

(load "./defpack.lisp")



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
					(exit))
				((string= text "") (read-one prm))
				(t text))))

(defun input () 
  (labels 
	((main (l)
		(let ((text (read-one "?")))
		  (cond 	
				((string= text "}") l)
				(t 
				  (format t "inputted: ")
				  (dump:dump-lexpr (parser:expr->in% text))
				  (main (cons text l)))))))
	(main nil)))
  


(defun main ()
  (format t "~%~%Input set of wff { ~%")
  (force-output t)
  (ignore-errors 
	(let ((in (input)))
		 (infer:semantic-conseq 
	  		(mapcar #'parser:expr->in% in)
	  		(let ((tmp (parser:expr->in% (read-one "conseq ?"))))
				(format t "inputted: ")
				(dump:dump-lexpr tmp)
				(format t "processing...~%")
				tmp)))) (main))

(credit)
(main)



