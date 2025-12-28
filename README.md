# parsers

Parser combinators and Pratt parsing (TBA).

`ParserTsoding.hs` parses JSON (with minor omissions from the standard like doubles and string escaping) and is practically based on [this video by Tsoding](https://www.youtube.com/watch?v=N9RUqGYuGfw). Some implementations differ. Unlike the original, this parser supports negative numbers. Note that this parser is not (yet) a `Monad`.
