
principal type algorithm for curry types in vanilla lambda calculus

todo:
- parse lambda calculus types nicely for input

```
$ cabal repl

ghci> sCombinator 
λxyz.xz(yz)
ghci> kCombinator 
λxy.x
ghci> iCombinator 
λx.x

ghci> pp sCombinator
(a -> b -> c) -> (a -> b) -> a -> c
ghci> pp kCombinator
a -> b -> a
ghci> pp iCombinator
a -> a
```
