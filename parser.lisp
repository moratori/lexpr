
#|

	一階述語論理の文字列表現の式を内部表現にする

	*内部表現の定義(あくまで内部表現のリストをそのまま表示した時のもの)
	
	<VAR>         ::= <SYMBOL>
	<PRED-SYM>    ::= <SYMBOL>	
	<OPERATOR>    ::= IMPL | AND | OR | NEG | EQL

	<QUANTIFIER>  ::= FORALL | EXIST
	<QUANTS>      ::= "(" <QUANTIFIER>  <VAR> ")"
	<QUANTS-PART> ::= "(" <QUANTS>+  ")"
	
	<ATOM>        ::= "(" <PRED-SYM>  <VAR>+ ")"
	<EXPR>        ::= 
		<ATOM> | "(" <OPERATOR>  <EXPR>+ ")" | "(" <QUANTS-PART> <EXPR> ")"

	@@ 関数記号ははじめに実装するときに忘れてしまったのでもうしゅうがない( @@


	*一階述語論理の文字列表現の定義

	<VAR>        ::= <SYMBOL>
	<PRED-SYM>   ::= <SYMBOL>	
	<OPERATOR>   ::= -> | & | V | ~ | <->
	
	<QUANTIFIER> ::= A | E
	<QUANTS>     ::= <QUANTIFIER> <VAR>
	<QUANTS-PART>::= <QUANTS>+ "."

	<ATOM>       ::= <PRED-SYM> "(" <VAR>+ ")"
	<EXPR>       ::= 
		<ATOM> | "(" <EXPR> <OPERATOR> <EXPR> ")" | <QUANTS-PART> "(" <EXPR> ")"

|#




