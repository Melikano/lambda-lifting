module Lib.RawString (r) where

{- This only works if you have template Haskell....
 - A template Haskell wrapper to turn an expression into a raw string
 - literal.
 -
 - If your Haskell installation does not have template haskell, then delete this file, and in the ``ExamplePrograms.hs`` file, do not use the ``[r| ... |]`` syntax
 -}

import Language.Haskell.TH
import Language.Haskell.TH.Quote

r :: QuasiQuoter 
r = QuasiQuoter {
    quoteExp = return . LitE . StringL
    , quotePat = const (fail "only raw strings literals allowed in a raw string literal quote")
    , quoteType = const (fail "only raw strings literals allowed in a raw string literal quote")
    , quoteDec = const (fail "only raw strings literals allowed in a raw string literal quote")
}


