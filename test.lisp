
(load "./defpack.lisp")


(infer:lexprs->tex
  (list 
	(parser:expr->in% "P(x) & (P(x) > Q(x)) > Q(x)")
	)
  "foo.tex"
  )

