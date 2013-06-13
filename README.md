lexpr
=====

タブロー法で述語論理の式集合に対して充足可能性を判定します  
  
充足可能な式集合が渡されると停止しなくなる場合があります

効率が悪いので、停止する場合でも非常に時間がかかる場合あり

Usage
=====

inferパッケージのsemantic-conseqに前提となる式集合と  
  
結論となる式を与えると、意味論的帰結となるかどうかを調べます

{∀X.HUMAN(X)⊃MORTAL(X) , HUMAN(SOKRATES) , ¬MORTAL(SOKRATES) , } is contradiction
