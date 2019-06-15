h x = if x > 20 then 50 else x*x + h (x+1)

h' x = quadsum x 50 -- 50 wird immer addiert
quadsum x sum = if x > 20 then sum else quadsum (x+1) (sum+x*x)
{- Testfaelle
*Main> h 1
2920
*Main> h' 1
2920
*Main> h 19
811
*Main> h' 19
811
*Main> h 20
450
*Main> h' 20
450
*Main> h 21
50
*Main> h' 21
50
*Main> h 23
50
*Main> h' 23
50
-}