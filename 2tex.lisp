

(load "./const.lisp")




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


(format t (instr->tex "∀ x∃ y.(P(x) ⊃  Q(x))"))
