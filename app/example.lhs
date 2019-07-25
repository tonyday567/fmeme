---
pagetitle: fmeme
---

[fmeme](https://github.com/tonyday567/fmeme) example
===

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

> {-# OPTIONS_GHC -Wall #-}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [readme-lhs](https://www.hackage.org/package/readme-lhs)

> import Protolude
> import Readme.Lhs

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

> main :: IO ()
> main = do
>   let n = 10
>   let answer = product [1..n::Integer]
>   void $ runOutput ("app/example.lhs", LHS) ("readme.md", GitHubMarkdown) $ do
>     output "example" (show answer)

10! is equal to:

```{.output .example}

```

tests
---

- [doctest](https://www.stackage.org/package/doctest)

> -- | doctests
> -- >>> let n = 10
> -- >>> product [1..n]
> -- 3628800