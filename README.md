# parsers

Parser combinators and Pratt parsing (TBA).

`ParserTsoding.hs` parses JSON (with minor omissions from the standard like doubles and string escaping) and is practically based on [this video by Tsoding](https://www.youtube.com/watch?v=N9RUqGYuGfw). Some implementations differ. Unlike the original, this parser supports negative numbers. Note that this parser is not (yet) a `Monad`.

`Lambda.hs` parses lambda expressions. It's worth noting that the common "grammar" for the language, namely

```
M ::= V | \x.M | M M
```

or its slight variation


```
M ::= V | \x.M | (M M)
```

that can be found online is not suitable for robust parsing. The first grammar is even worse than the second because it's nontrivial to parse with a left recursive parser (which combinators, at least with unrefactored terms, mostly are). The second option is better in the sense that it seems nonambiguous, but most lambda terms out in the wild will assume left associativity of application and omit parentheses where possible.

De Brujin parsing for lambda expressions is TBA.
