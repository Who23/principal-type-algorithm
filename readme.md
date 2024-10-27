
principal type algorithm for curry types in vanilla lambda calculus

todo:
- parse lambda calculus types nicely for input
- rename type variables at the end to start from 1 & use letters
- remove parens in show instances for lambda calc & types

```
$ cabal repl

ghci> pp sCombinator 
((4) -> ((5) -> (6))) -> (((4) -> (5)) -> ((4) -> (6)))

ghci> pp kCombinator 
(0) -> ((1) -> (0))

ghci> pp iCombinator 
(0) -> (0)
```
