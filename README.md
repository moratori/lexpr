lexpr
=====

タブロー法で述語論理の式集合に対して充足可能性を判定します  

ホーン節に限るとか、述語論理の表現力を落とせば、もっとうまく行くかも

※ style-warning がたくさん出るのは仕様です(

Usage
=====

inferパッケージのsemantic-conseqに前提となる式集合と  
  
結論となる式を与えると、意味論的帰結となるかどうかを調べます

TESTER.lispがサンプルになっているので

動かしてみるとどんな感じになるかわかります

main.lisp でインタラクティブに定理証明ができます

```使い方
$ sbcl --script main.lisp 
Theorem Prover Beta 0.9
Input set of wff { 
? Ax.(human(x) > mammal(x))
inputted: ∀x.human(x)⊃mammal(x)
? Ax.(mammal(x) > creature(x))
inputted: ∀x.mammal(x)⊃creature(x)
? Ax.(creature(x) > mortal(x))
inputted: ∀x.creature(x)⊃mortal(x)
? }
conseq ? ~Ex.(human(x) & ~mortal(x))
inputted: ¬∃x.human(x)∧¬mortal(x)
processing...

{∀x.creature(x)⊃mortal(x) , 
 ∀x.mammal(x)⊃creature(x) , 
 ∀x.human(x)⊃mammal(x) , 
 ¬(¬∃x.human(x)∧¬mortal(x)) , } is contradiction
```

矛盾(contradiction)になれば意味論的帰結となることが言えます

逆に充足可能(satisfiable)な状態で停止することは少ないです...

演算子は以下の通りです

全称量化: A
存在量化: E
否定: ~
含意: >
連言: &
選言: V
同値: -


```使用可能な一階述語論理の式の形式的定義
	<VAR>        ::= <LISP-SYMBOL>
	<PRED-SYM>   ::= <LISP-SYMBOL>	
	<OPERATOR>   ::= > | & | V | ~ | -
	
	<QUANTIFIER> ::= A | E
	<QUANTS>     ::= <QUANTIFIER> <VAR>
	<QUANTS-PART>::= <QUANTS>+ "."

	<ATOMIC>     ::= <PRED-SYM> "(" <VAR>+ ")"
	<EXPR>       ::= 
		<ATOMIC> | "(" <EXPR> <OPERATOR> <EXPR> ")" | <QUANTS-PART> <EXPR> 
```
