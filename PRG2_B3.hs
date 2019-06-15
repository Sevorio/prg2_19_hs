-- =============================================================================
-- Grundlagen der Programmierung 2
-- Aufgabenblatt 3
-- =============================================================================

import Data.Char
-- =============================================================================
-- =
-- = Aufgabe 1

transform x = map switchYZ (step3and4 (step1and2 x) )
-- implentierung der einzelnen Schritte: Schritt1und2
step1and2 x = map spaceToLine (filter myfilter1 x) -- Aplhanum&spaces, dann spacetoline

myfilter1 x = isAlphaNum (x) || x == ' '
spaceToLine x = if x == ' ' then '_' else x

-- implentierung der einzelnen Schritte: Schritt3und4
step3and4 x = map toUpper (map digitToSpace x)

digitToSpace x = if isDigit(x) then ' ' else x

--zuletzt tausche x und y
switchYZ x
 | x == 'Z' = 'Y'
 | x == 'Y' = 'Z'
 | otherwise = x

{-Tests:
*Main> transform "Wie Bitte? Aktenzeichen X Y Nummer 1234"
"WIE_BITTE_AKTENYEICHEN_X_Z_NUMMER_    "
*Main> transform "a? haskell1337zYyZ% !z"
"A_HASKELL    YZZY_Y"
*Main> transform "XYZ1337-ProgrammerZYX"
"XZY    PROGRAMMERYZX"
-}
-- =============================================================================

-- =============================================================================
-- =
-- = Aufgabe 2
-- - a)

g1 x = manipulate x 

manipulate ('G':'e':'b':'e':'n':' ':'S':'i':'e':' ':'m':'i':'r':xs) = ("Geb mi mol")++(manipulate xs)
manipulate ('Z':'a':'n':'g':'e':xs) = "Tang"++ (manipulate xs)
manipulate ('b':'i':'t':'t':'e':' ':xs) = manipulate xs
manipulate (x:xs) = x: manipulate(xs)
manipulate []  = []

{-Tests:
*Main> g1 "Geben Sie mir bitte die Zange her!"
"Geb mi mol die Tang her!"
*Main> g1 "Was bitte ist eine Zange"
"Was ist eine Tang"
*Main> g1 "Geben Sie mir bitte Testfaelle mit"
"Geb mi mol Testfaelle mit"
-}
-- - b)
-- -

g2 x = triplets x 
triplets (e1:e2:e3:rest) = if rest == [] then [[e2,e3,e1]] else (([e2,e3,e1]):(triplets(rest)))
triplets x = [x]

{-Tests:
*Main> g2 [1,2,3,4,5,6,7,8]
[[2,3,1],[5,6,4],[7,8]]
*Main> g2 [1,2,3]
[[2,3,1]]
*Main> g2 []
[[]]
*Main> g2 [1,2,3,4]
[[2,3,1],[4]]
-}
-- - c)
-- -

g3 x= pairs x -- zu implementieren!
-- check for 2 oder mehr
pairs (e1:e2:rest) = if rest == [] then [[e1,e2]] else (getandReverse e1 (reverseme (e2:rest)))
pairs x = [x] -- 1 oder leer

getandReverse e1 (last:rest) = [e1,last] : (pairs (reverseme rest))

--hilfsfunktion reverseme:
reverseme (e1:rest) = reverseme (rest) ++[e1]
reverseme [] = []

{- Tests:
*Main> g3 []
[[]]
*Main> g3 [1]
[[1]]
*Main> g3 [1,2]
[[1,2]]
*Main> g3 [1,2,3]
[[1,3],[2]]
*Main> g3 [1,2,3,4]
[[1,4],[2,3]]
*Main> g3 [1,2,3,4,5,6,7]
[[1,7],[2,6],[3,5],[4]]
*Main> g3 [1,2,3,4,5,6,7,8]
[[1,8],[2,7],[3,6],[4,5]]
-}
-- =
-- =============================================================================
-- = Aufgabe 3
-- =

-- Beispiel-Legs.
leg1 = [[[3,20],[3,20],[3,20]], [[3,20],[3,19],[2,25]], [[3,19],[3,19],[3,19]],
        [[3,20],[3,19],[2,25]], [[2,25],[2,25],[2,25]]]
leg2 = [[[3,20],[3,20],[3,20]], [[3,20],[3,19],[2,25]], [[3,19],[3,19],[3,19]],
        [[3,20],[3,19],[2,25]], [[2,25],[2,25],[1,25]], [[3,20],[3,19],[2,25]]]
leg3 = [[[3,20],[1,20],[1,20]], [[3,20],[1,20],[1,20]], [[3,20],[3,20],[3,20]],
        [[3,20],[3,20],[3,20]], [[3,20],[1,1] ,[2,20]], [[3,20],[1,1] ,[2,20]],
        [[3,20],[1,20],[2,20]]]
leg4 = [[[3,20],[1,20],[1,20]], [[3,20],[1,20],[1,20]], [[3,20],[3,20],[3,20]],
        [[3,20],[3,20],[3,20]], [[3,20],[1,1] ,[2,20]], [[3,20],[1,1] ,[2,20]],
        [[3,20],[1,20],[1,20]], [[3,20],[1,20],[2,20]]]
mytestleg1 = [[[3,20],[3,20],[3,20]],  [[0,0]], --180
            [[3,20],[3,20],[3,20]],  [[0,0]], -- 360
            [[3,20],[1,10],[2,10]],  [[0,0]], --450
            [[3,20]],  [[0,0]], -- ueberworfen
            [[1,1],[2,25]]]
mytestleg2 = [[[3,20],[3,20],[3,20]],  [[0,0]], --180
            [[3,20],[3,20],[3,20]],  [[0,0]], -- 360
            [[3,20],[1,10],[2,10]],  [[0,0]], --450
            [[3,20]],  [[0,0]], -- ueberworfen
            [[1,1],[2,20]]] -- zu wenig
-- -
-- =============================================================================   
--HILFSFUNKTIONEN:
listOfPossiblepoints = [0..20]++[25]
-- turnpoints:
tp (w1:rest) = if rest == [] then (dartpoints w1) else 
        (dartpoints w1) + (tp (rest) )--Punkte der Aufnahme

dartpoints (multi:points:[]) = if notElem multi [0,1,2,3] then -999 else --wrong multiplicator
    if notElem points listOfPossiblepoints then -999 else -- wrong points
    if multi + points ==28 then -999 else multi*points -- triple bull?

-- parse:wer war dran ; p1/2 = punkte bisher ; score = punkte am brett
afterturn p p1 p2 score isDoubleout (x:xs)
 | score < 0 = 0
 | (xs == []) = if (checkout x) then istSieger p p1 p2 (tp x) else 0
 | p == 1 && (p1 + score < 500) = afterturn 2 (p1+score) (p2) (tp x) (checkout x) xs -- gueltiger wurf
 | p == 2 && (p2 + score < 500) = afterturn 1 (p1) (p2+score) (tp x) (checkout x) xs --gueltiger wurf
 | p == 1 && (p1 + score > 499) = afterturn 2 (p1) (p2) (tp x) (checkout x) xs -- ueberworfen
 | p == 2 && (p2 + score > 499) = afterturn 1 (p1) (p2) (tp x) (checkout x) xs -- ueberworfen
 | otherwise = 0

istSieger p p1 p2 score -- Switched p, last turn skipped
 | p == 1 && (p2 + score == 501) = 2
 | p == 2 && (p1 + score == 501) = 1
 | otherwise = 0

checkout (([m,p]):rest) = if rest == [] then (m ==2) else checkout rest -- check for ouble out
 
-- =============================================================================   
-- - a)

legSieger (x:xs) = afterturn 1 0 0 (tp x) False xs -- zu implementieren!
legSieger [] = 0

{- Tests:
*Main> legSieger leg1
1
*Main> legSieger leg2
2
*Main> legSieger leg3
1
*Main> legSieger leg4
2
*Main> legSieger mytestleg1
1
*Main> legSieger mytestleg2
0
*Main> legSieger (take 3 leg1)
0
-}
-- - b)
satzSieger x = beginnerwins 0 0 True 0 x

beginnerwins wins loose is1 count (x:xs)
 | count >5 = error "zuviele Legs"
 | wins == 3 = error "zuviele Legs"
 | loose == 3 = error "zuviele Legs"
 | is1 && ((legSieger x) == 1) = beginnerwins (wins+1) loose False (count+1) xs
 | is1 && ((legSieger x) == 2) = beginnerwins (wins) (loose+1) False (count+1) xs
 | ((legSieger x) == 1) = beginnerwins (wins) (loose+1) True (count+1) xs
 | ((legSieger x) == 2) = beginnerwins (wins+1) (loose) True (count+1) xs
 | otherwise = error "nicht korrekt"

beginnerwins wins loose _ _ []
 | wins == 3 = True
 | loose == 3 = False
 | otherwise = error "kein gewinner"

{- Tests:
*Main> satzSieger [leg1,leg2,leg3]
True
*Main> satzSieger [leg1,leg2,leg3,leg3]
*** Exception: zuviele Legs
CallStack (from HasCallStack):
  error, called at C:\Users\sevor\Desktop\SS19\PRG2\B3\blatt3.hs:191:16 in main:Main
*Main> satzSieger [leg4,leg3,leg3,leg4,leg1]
True
*Main> satzSieger [leg1,leg1,leg1,leg1,leg1]
True
-}
-- =============================================================================                                                                                    
