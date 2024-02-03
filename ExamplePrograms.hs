{-# LANGUAGE QuasiQuotes #-}
module ExamplePrograms where

import Data.List
-- import Lib.RawString

{- Notes about writing example programs.
 - These programs are white space sensitive, so we can use
 - the 'unlines' function from Data.List to convert a list
 - of strings to get the appropriate new lines on each line as follows...
 -}

assign0 :: String
assign0 = unlines 
    [ "fun main(x, y) ="
    , "     let fun add(p) = add_to_x(p)"
    , "         fun add_to_x(q) = add_to_y(q) + x"
    , "         fun add_to_y(q) = q + y"
    , "     in add(y + x) end"
    ]

{- But, what we would really like is to just write a program as follows
 - where the whole program is literally just interpreted as a raw 
 - string literal..  -}
{-
assign0RawStringLiteral :: String
assign0RawStringLiteral= [r|
fun main(x, y) =
    let fun add(p) = add_to_x(p)
        fun add_to_x(q) = add_to_y(q) + x
        fun add_to_y(q) = q + y
    in add(y + x) end
|]
-}

{- If you would like to just write raw string literals for test programs,
 - uncomment the ``-- import Lib.RawString`` line above, and 
 - try uncommenting the function ``assign0RawStringLiteral``

 - Although, using the raw string literals in Haskell uses an extension
 - known as ``Template Haskell`` and not all Haskell compilers support it.
 -
 - If it works, feel free to use it -- it will make your life a lot easier.
 - But if it doesn't, just go back to using the ``unlines`` function..
 -
 - Enjoy writing your own example programs!
 -
 - Some fun example programs are included below (written as a raw string literal)
 -
 - Notes on parsing:
 -      - The language is meant to ONLY have bool expressions on 
 -         the if part of the ``if then else`` construct, and will 
 -         otherwise fail to parse if a bool expression is elsewhere;
 -         OR it will parse error if an expression 
 -         (not a bool expression see AST.hs) is in the if part of 
 -         an ``if then else`` construct
 -
 -      - Let bindings are white space sensitive (similar to Haskell)
 -         but have the form of ``let ... in ... end``.
 -         Note that we require the ``end`` keyword in this language.
 -
 -      - Function application: unlike Haskell (which uses space for function application,
 -          this language explicitly writes out what functions are called with what -- similar
 -          to C,C++ or Java.
 -          In other words, write ``myFun(a, b + b)`` to say ``apply function myFun to arguments `a` and `b+b` ``.
 -          Similarly, to call a function with 0 arguments, write ``myFun()``.
 -          Note the difference from referencing a variable i.e., if you write ``myFun``, 
 -          it will think that ``myFun`` is a variable.
 -
 -      - Function declarations must ALL begin with the ``fun`` keyword e.g.
 -            To declare a function ``myFun``, do NOT write ``myFun x() = {- function body here -}`` ( parse error ).
 -            Instead, write ``fun myFun x() = {- function body here -}`` (note the ``fun`` keyword).
 -            Function declarations in let bindings also need the ``fun`` keyword.
 -      
 -      - Most importantly, the parser is provided for CONVENIENCE ONLY and there may or may not be bugs with it. See ``ASTParse.hs`` for more details.
 -}


{- 
assign0 :: String
assign0 = [r|
fun main(x, y) =
    let fun add(p) = add_to_x(p)
        fun add_to_x(q) = add_to_y(q) + x
        fun add_to_y(q) = q + y
    in add(y + x) end

|]

assign0Sol :: String
assign0Sol = [r|
fun main(x__0,y__1) = add__2(y__1+x__0,x__0,y__1)
fun add__2(p__5,x__0,y__1) = add_to_x__3(p__5,x__0,y__1)
fun add_to_x__3(q__6,x__0,y__1) = add_to_y__4(q__6,y__1)+x__0
fun add_to_y__4(q__7,y__1) = q__7+y__1
|]


assign1 :: String
assign1 = [r| 
fun main(x, y, z) = 
    let fun f(y) = x + g(y)
        fun g(z) = 
            let fun f(x) = x * z
            in f(x) end
    in g(z) + f(x) end
|]

assign1Sol :: String
assign1Sol = [r|
fun main(x__0,y__1,z__2) = g__4(z__2,x__0)+f__3(x__0,x__0)
fun f__3(y__5,x__0) = x__0+g__4(y__5,x__0)
fun g__4(z__6,x__0) = f__7(x__0,z__6)
fun f__7(x__8,z__6) = x__8*z__6
|]


assign2 :: String
assign2 = [r| 
fun main(x, y, z, n) = 
    let fun f1(v) = x + f2(v)
        fun f2(j) = 
            let fun g2(b) = b + f3(j)
            in g2(y) + f3(x)
            end
        fun f3(k) = 
            let fun g3(c) = c * f1(k)
            in g3(z)
            end
    in f1(n)
    end
|]

assign2Sol :: String
assign2Sol = [r|
fun main(x__0,y__1,z__2,n__3) = f1__4(n__3,x__0,y__1,z__2)
fun f1__4(v__7,x__0,y__1,z__2) = x__0+f2__5(v__7,x__0,y__1,z__2)
fun f2__5(j__8,x__0,y__1,z__2) = g2__9(y__1,j__8,z__2,x__0,y__1)+f3__6(x__0,z__2,x__0,y__1)
fun g2__9(b__10,j__8,z__2,x__0,y__1) = b__10+f3__6(j__8,z__2,x__0,y__1)
fun f3__6(k__11,z__2,x__0,y__1) = g3__12(z__2,k__11,x__0,y__1,z__2)
fun g3__12(c__13,k__11,x__0,y__1,z__2) = c__13*f1__4(k__11,x__0,y__1,z__2)
|]

----------------------
-- Some more rather random test cases...
----------------------
random1 :: String
random1 = [r|
fun main(x, y, z) = 
    let fun f(x) = 
            let fun f(x) = x
            in let fun f(x) = x + y in f(x)  end 
            end
        fun g(x) = x
    in if f(z) == g(x) then 0 else 1 end
|]

random1Sol :: String
random1Sol = [r|
fun main(x__0,y__1,z__2) = if f__3(z__2,y__1)==g__4(x__0) then 0 else 1
fun f__3(x__5,y__1) = f__8(x__5,y__1)
fun f__6(x__7) = x__7
fun f__8(x__9,y__1) = x__9+y__1
fun g__4(x__10) = x__10
|]

random2 :: String
random2 = [r|
fun main(x, y, z) = 
    let fun f(x) = g(y) + z 
        fun g(x) = 1 - f(y) + 1 / 2
    in if (let fun f(x) = z in f(y) end) == 3 || x == z then f(z) else g(z) end
|]

random2Sol :: String
random2Sol = [r|
fun main(x__0,y__1,z__2) = (if f__7(y__1,z__2)==3||x__0==z__2
   then f__3(z__2,z__2,y__1)
   else g__4(z__2,y__1,z__2))
fun f__3(x__5,z__2,y__1) = g__4(y__1,y__1,z__2)+z__2
fun g__4(x__6,y__1,z__2) = 1-f__3(y__1,z__2,y__1)+1/2
fun f__7(x__8,z__2) = z__2
|]
-}
