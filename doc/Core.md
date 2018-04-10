# BNF syntax for the Core language

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

# Operators

| Precedence | Associativity | Operator        |
| ---------- |:-------------:| --------------- |
| 6          | Left          | Application     |
| 5          | Right         | *               |
|            | None          | /               |
| 4          | Right         | +               |
|            | None          | -               |
| 3          | None          | == ~= > >= < <= |
| 2          | Right         | &               |
| 1          | Right         | |               |


# Grammar expressing operator precedence and associativity

```
expr -> let defns in expr
      | letrec defns in expr
      | case expr of alts
      | \ var_1 ... var_n . expr
      | expr1

expr1 -> expr2 | expr1
       | expr2

expr2 -> expr3 & expr2
       | expr3

expr3 -> expr4 relop expr4
       | expr4

expr4 -> expr5 + expr4
       | expr5 - expr5
       | expr5

expr5 -> expr6 * expr5
       | expr6 / expr6
       | expr6

expr6 -> aexpr_1 ... aexpr_n    (n >= 1)
```
