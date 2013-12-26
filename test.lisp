
(load "./defpack.lisp")


(infer:lexprs->tex
  (list 
	(parser:expr->in% "Ax.(P(x) > Q(x))")
	(parser:expr->in% "Ax.(Q(x) > R(x))")
	(parser:expr->in% "~Ax.(~R(x) > ~P(x))")
	)
  "foo4.tex"
  )

