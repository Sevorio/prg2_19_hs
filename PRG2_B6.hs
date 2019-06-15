-- ============================================================================
-- Grundlagen der Programmierung 2
-- Aufgabenblatt 6
-- ============================================================================
import Prelude hiding ((<*>),(*>),(<*))
import Data.Char
import CombParser
-- =============================================================================
-- = Aufgabe 1
-- =
data Command = Tempo TempoMode
             | Junction Int
             | Repeat Int [Command]
  deriving(Eq,Show)

data TempoMode = Adaptive | Specific Int | Turboboost
  deriving(Eq,Show)

type Program = [Command]

-- c)

parseDrive :: Parser Char Program
parseDrive = parseComs

parseComs = parseCom <|> ((parseCom <*> token ";" <*> parseComs) <@ \(e3,(e1,e2)) -> (e3 ++ e2)) -- e1=";"
parseCom = parseSpeed <|> parseJunc  <|> parseRepeat

parseSpeed =  ((token "Adaptiv") <@ \x -> [Tempo Adaptive])  <|>
    ((token "Turboboost") <@ \x -> [Tempo Turboboost]) <|>
    ((token "30") <@ \x -> [Tempo (Specific 30)]) <|>
    ((token "35") <@ \x -> [Tempo (Specific 35)]) <|>
    ((token "45") <@ \x -> [Tempo (Specific 45)]) <|>
    ((token "55") <@ \x -> [Tempo (Specific 55)]) <|>
    ((token "65") <@ \x -> [Tempo (Specific 65)]) <|>
    ((token "70") <@ \x -> [Tempo (Specific 70)]) <|>
    ((token "75") <@ \x -> [Tempo (Specific 75)]) <|>
    ((token "80") <@ \x -> [Tempo (Specific 80)]) <|>
    ((token "85") <@ \x -> [Tempo (Specific 85)]) <|>
    ((token "200") <@ \x -> [Tempo (Specific 200)])
    
parseJunc = ((token "Abzweigung " *> natural) <@ \x -> [Junction x])
parseRepeat = ((token "wiederhole " *> natural) <*> ( token " ("*> parseComs <* symbol ')' ) <@ \(x,y) -> [Repeat x y])
--
-- Tests siehe unten
-- d)
--

parseDriveD :: Parser Char Int
parseDriveD = parseComsD

parseComsD = parseComD <|> ((parseComD <*> token ";" <*> parseComsD) <@ \(e3,(e1,e2)) -> (e3 + e2)) -- e1=";"
parseComD = parseSpeedD <|> parseJuncD  <|> parseRepeatD

parseSpeedD =  ((token "Adaptiv") <@ \x -> 0)  <|> ((token "Turboboost") <@ \x -> 0) <|>
    ((token "30") <@ \x -> 0) <|>
    ((token "35") <@ \x -> 0) <|>
    ((token "45") <@ \x -> 0) <|>
    ((token "55") <@ \x -> 0) <|>
    ((token "65") <@ \x -> 0) <|>
    ((token "70") <@ \x -> 0) <|>
    ((token "75") <@ \x -> 0) <|>
    ((token "80") <@ \x -> 0) <|>
    ((token "85") <@ \x -> 0) <|>
    ((token "200") <@ \x -> 0)
-- All Speeds are irrelevant
parseJuncD = ((token "Abzweigung " *> natural) <@ \x -> 100)
parseRepeatD = ((token "wiederhole " *> natural) <*> ( token " ("*> parseComsD <* symbol ')' ) <@ \(x,y) -> x*y)

-- =
--Testfahrten:
fahrt1 = "Adaptiv;Abzweigung 2;Abzweigung 3;75;wiederhole 2 (Abzweigung 0);Turboboost"
fahrt2 = "85;Abzweigung 2;wiederhole 6 (Abzweigung 3;Turboboost)"
{-Tests
*Main> parseDrive "Adaptiv;wiederhole 3 (Abzweigung 2;Abzweigung 4)"
[(";wiederhole 3 (Abzweigung 2;Abzweigung 4)",[Tempo Adaptive]),
("",[Tempo Adaptive,Repeat 3 [Junction 2,Junction 4]])]

*Main>  parseDriveD "Adaptiv;wiederhole 3 (wiederhole 5 (Abzweigung 2;Abzweigung 1))"
[(";wiederhole 3 (wiederhole 5 (Abzweigung 2;Abzweigung 1))",0),("",3000)]

*Main> parseDrive fahrt1
[(";Abzweigung 2;Abzweigung 3;75;wiederhole 2 (Abzweigung 0);Turboboost",[Tempo Adaptive]),
(";Abzweigung 3;75;wiederhole 2 (Abzweigung 0);Turboboost",[Tempo Adaptive,Junction 2]),
(";75;wiederhole 2 (Abzweigung 0);Turboboost",[Tempo Adaptive,Junction 2,Junction 3]),
(";wiederhole 2 (Abzweigung 0);Turboboost",[Tempo Adaptive,Junction 2,Junction 3,Tempo (Specific 75)]),
(";Turboboost",[Tempo Adaptive,Junction 2,Junction 3,Tempo (Specific 75),Repeat 2 [Junction 0]]),
("",[Tempo Adaptive,Junction 2,Junction 3,Tempo (Specific 75),Repeat 2 [Junction 0],Tempo Turboboost])]

*Main> parseDriveD fahrt1
[(";Abzweigung 2;Abzweigung 3;75;wiederhole 2 (Abzweigung 0);Turboboost",0),
(";Abzweigung 3;75;wiederhole 2 (Abzweigung 0);Turboboost",100),
(";75;wiederhole 2 (Abzweigung 0);Turboboost",200),
(";wiederhole 2 (Abzweigung 0);Turboboost",200),
(";Turboboost",400),("",400)]

*Main> parseDrive fahrt2
[(";Abzweigung 2;wiederhole 6 (Abzweigung 3;Turboboost)",[Tempo (Specific 85)]),
(";wiederhole 6 (Abzweigung 3;Turboboost)",[Tempo (Specific 85),Junction 2]),
("",[Tempo (Specific 85),Junction 2,Repeat 6 [Junction 3,Tempo Turboboost]])]

*Main> parseDriveD fahrt2
[(";Abzweigung 2;wiederhole 6 (Abzweigung 3;Turboboost)",0),
(";wiederhole 6 (Abzweigung 3;Turboboost)",100),("",700)]
-} 
-- =============================================================================
