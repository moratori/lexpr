
(load "./defpack.lisp")



(print (parser:expr->in% "~(P(x) & R(x)) & Ax.(P(x))"))
