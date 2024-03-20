module PrincipalType (typeA, typeB, typeC, typeD, typeE) where

{-
                                       ** THE TASK **

Implement functions with the following types. You are not allowed to use undefined or error.

Haskell is able to infer the most general type for a function that has been defined in it.  For example
λ> g x y = x y
λ> :t g
g :: (t1 -> t2) -> t1 -> t2

For the following questions, define a function so that if we were to type it into haskell, and ask for its type using ":t" we would get types from A, B, C, D, and E, up to renaming variables.

This task is worth 10 POINTS.

--}

typeA (f, a) = f a 

typeB f1 a = (a, f1 (a, a) ++ f1 (a, a))

typeC  f (x, y) (z, w) = f x y z w

typeD f g x = f x (g x)

typeE = undefined



