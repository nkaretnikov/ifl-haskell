```
Programs          program  ->  sc_1 ; ... ; sc_n               n >= 1

Supercombinators       sc  ->  var var_1 ... var_n = expr      n >= 0

Expressions          expr  ->  expr aexpr                      Application
                            |  expr_1 binop expr_2             Infix binary application
                            |  let defns in expr               Local definitions
                            |  letrec defns in expr            Local recursive definitions
                            |  case expr of alts               Case expression
                            |  \ var_1 ... var_n . expr        Lambda abstraction (n >= 1)
                            |  aexpr                           Atomic expression

                    aexpr  ->  var                             Variable
                            |  num                             Number
                            |  Pack{num,num}                   Constructor
                            |  ( expr )                        Parenthesized expression

Definitions         defns  ->  defn_1 ; ... ; defn_n           n >= 1
                     defn  ->  var = expr

Alternatives         alts  ->  alt_1 ; ... ; alt_n             n >= 1
                      alt  ->  <num> var_1 ... var_n -> expr   n >= 0

Binary operators    binop  ->  arithop | relop | boolop
                  arithop  ->  + | - | * | /                   Arithmetic
                    relop  ->  < | <= | == | ~= | >= | >       Comparison
                   boolop  ->  & | |                           Boolean

Variables            var   ->  alpha varch_1 ... varch_n       n >= 0
                    alpha  ->  an alphabetic character
                    varch  ->  alpha | digit | _

Numbers               num  ->  digit_1 ... digit_n             n >= 1
```
