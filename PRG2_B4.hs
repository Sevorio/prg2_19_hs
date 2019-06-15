-- ============================================================================
-- Grundlagen der Programmierung 2
-- Aufgabenblatt 4
-- ============================================================================

import Data.Char
import Data.List

-- =============================================================================
-- =
-- = Aufgabe 1
-- =

-- -
-- - a)
-- -

-- Unvollständige gegebene List-Comprehension: [(a,b,c,d,e,f,g) | a <- [1..7], ]
 -- Bedingung: start bei 2, ende bei 4, vorletztes 7, zweites 6, drittes 3,
-- verbleibend d&e als 2 &5 bedingung: von der 5 zur 1
-- Die Ungleichheit wird durch die vorherigen Bedingungen gegeben.
 
wege = [(a,b,c,d,e,f,g) | a <- [1..7],b <- [1..7],c <- [1..7],f <- [1..7],g <- [1..7], d<-[1,5] ,e <-[1,5],
 a ==2, g==4,b ==6, c == 3,  f== 7, e ==(d-4) ]

-- Test:
-- *Main> wege
-- [(2,6,3,5,1,7,4)]
-- - b)
-- -
data Ausruestung = Ausruestung [Waffe]
  deriving(Eq,Show)
data Waffe = Waffe Art Name Seltenheit Verkaufbar
  deriving(Eq,Show)
data Art = Schwert | Dolch | SchwereKeule | SchwereKlinge | Stab | Speer | Bogen
  deriving(Eq,Show)
data Seltenheit = Gewoehnlich | Selten | Episch | Legendaer
  deriving(Eq,Show)
type Verkaufbar = Bool
type Name = String

-- -

damokles1 = Waffe Schwert "Damoklesschwert" Legendaer False
kronos1 = Waffe Dolch "Dolch des Kronos" Legendaer False
bogi1 = Waffe Bogen "Bogen des Paris" Legendaer False
testsw1 = Waffe Schwert "Silberschwert" Selten True
testsw2 = Waffe Schwert "Silberschwert" Selten False
testbo1 = Waffe Bogen "Bogen des Paris" Legendaer True
testset = Ausruestung [testsw1, testsw1, kronos1]
testset2 = Ausruestung [testsw1, testsw2, testbo1]
-- - b1)
-- -

ausruestung :: Ausruestung
ausruestung = Ausruestung [damokles1, kronos1, bogi1]
{- Test:
*Main> ausruestung
Ausruestung [Waffe Schwert "Damoklesschwert" Legendaer False,
Waffe Dolch "Dolch des Kronos" Legendaer False,
Waffe Bogen "Bogen des Paris" Legendaer False]-}

-- -
-- - b2)
-- -

--namenSchwerterVerkaufbar :: Ausruestung -> [String]
namenSchwerterVerkaufbar (Ausruestung waffenliste )= getlist waffenliste 
getlist ((Waffe a n _ sell):xs) = if sell && a == Schwert then [n] ++ getlist xs else getlist xs
getlist [] = []

{- Tests
*Main> namenSchwerterVerkaufbar testset
["Silberschwert","Silberschwert"]
*Main> namenSchwerterVerkaufbar ausruestung
[]-}
-- - b3)
-- -

nunVerkaufbar :: Ausruestung -> Name -> Ausruestung
nunVerkaufbar (Ausruestung given_set) given_name = Ausruestung (selllist given_set given_name) 

selllist ((Waffe a n s sell):xs) given_name = if (n == given_name) 
then [Waffe a n s True] ++ selllist xs given_name
else [Waffe a n s sell] ++ selllist xs given_name

selllist [] g = [] 
{-Tests:
*Main> nunVerkaufbar ausruestung "Bogen des Paris"
Ausruestung [Waffe Schwert "Damoklesschwert" Legendaer False,
Waffe Dolch "Dolch des Kronos" Legendaer False,
Waffe Bogen "Bogen des Paris" Legendaer True]
*Main> nunVerkaufbar ausruestung "Damoklesschwert"
Ausruestung [Waffe Schwert "Damoklesschwert" Legendaer True,
Waffe Dolch "Dolch des Kronos" Legendaer False,
Waffe Bogen "Bogen des Paris" Legendaer False] -}
-- -
-- - b4)
-- -

dreiWaffenEinBogen :: Ausruestung -> Bool
dreiWaffenEinBogen (Ausruestung list) = if length (list) == 3 && (countbow list 0) == 1 then True else False

countbow ((Waffe a _ _ _):xs) count = if a == Bogen
then countbow xs (count +1)
else countbow xs (count)

countbow [] c = c

{-Tests:
*Main> dreiWaffenEinBogen ausruestung
True
*Main> dreiWaffenEinBogen testset
False
-}
-- =
-- =
-- =============================================================================

-- =============================================================================
-- =
-- = Aufgabe 2
-- =

-- -
-- - a)
-- -
aufgabeA = [n^2| n<- [1..], even n, (n `mod` 9 == 2), (n `mod` 35==3)]
-- *Main> take 5 aufgabeA
--[1444,446224,1684804,3717184,6543364]
-- -
-- - b)
-- -
aufgabeB tripel = [ (a*b, b+c)| (a,b,cs) <- tripel, c<- cs, (a*b > b+c)]
-- *Main> aufgabeB  [(10,20,[1,10,100]), (4,5,[5,15,25])]
-- [(200,21),(200,30),(200,120),(20,10)]
-- -
-- - c)
-- -
aufgabeC = [(a,b,c,d)|d <- "YZ", c<-[False, True],b<-"prg",a<-[10, 34, 77, 180] ]
-- *Main> take 5 aufgabeC
-- [(10,'p',False,'Y'),(34,'p',False,'Y'),(77,'p',False,'Y'),(180,'p',False,'Y'),(10,'r',False,'Y')]
-- -
-- - d)
-- -
aufgabeD = [p| p <- [2..], ([k|k<- [2..(p-1)],p `mod` k == 0] == [])]
-- p in der liste wenn: die liste aller k aus 2bis(p-1) die p teilen leer ist. 1 ist keine Primzahl!
-- *Main> take 12 aufgabeD
-- [2,3,5,7,11,13,17,19,23,29,31,37]

-- -
-- - e)
-- -
aufgabeE = [(a,b*b)|paarsumme <- [2..], a<-[1..(paarsumme-1)],b<-[paarsumme-a], a `mod` 7 == 0 ]
-- so werden all paare geprueft, die eine bestimmte Summe bilden. Damit wird fair inkrementiert.
{- Tests:
*Main> (49,4) `elem` aufgabeE
True
*Main> (49,81) `elem` aufgabeE
True
*Main> (7,81) `elem` aufgabeE
True
*Main> (707,81) `elem` aufgabeE
True
*Main> (707,900) `elem` aufgabeE
True -}

-- =
-- =
-- =============================================================================
