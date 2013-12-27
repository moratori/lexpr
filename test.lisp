
(load "./defpack.lisp")


(infer:lexprs->tex
  (list 
	(parser:expr->in% "AxAy.(R(x,y) & R(y,x) > P(x) & P(y))")
	(parser:expr->in% "R(a,b) & R(b,a)")
	(parser:expr->in% "~Ex.(P(x))")
	)
  "foo-1.tex"
  )

