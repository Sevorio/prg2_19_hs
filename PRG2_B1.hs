-- ============================================================================
-- Grundlagen der Programmierung 2
-- Aufgabenblatt 1
-- ============================================================================

import Data.List

-- =============================================================================
-- = Aufgabe 1 ist ohne Abgabe
-- = Aufgabe 2
-- =
berechneNote :: Float -> Float -> String
berechneNote klausurpunkte bonuspunkte -- Guards fuer die Uebersicht
    |klausurpunkte < 40 = "nicht bestanden"
    | bonuspunkte > 20 = error "mehr als 20 bonuspunkte"
    | klausurpunkte > 100 = error "mehr als 100 Klausurpunkte"
    | g_P < 50 = "nicht bestanden" -- guards fuer die gesamtpunkte
    | g_P < 54 = "4,0"
    | g_P < 58 = "3,7"
    | g_P < 62 = "3,3"
    | g_P < 66 = "3,0"
    | g_P < 70 = "2,7"
    | g_P < 74 = "2,3"
    | g_P < 78 = "2,0"
    | g_P < 82 = "1,7"
    | g_P < 86 = "1,3"
    | g_P >= 86 = "1,0"
    where g_P = klausurpunkte + bonuspunkte --g_P sind gesamtpunkte
-- =
{-- ! Tests: 
*Main> berechneNote 30 22
"*** Exception: mehr als 20 bonuspunkte
CallStack (from HasCallStack):
  error, called at C:\Users\sevor\Desktop\SS19\PRG2\B1\PatrikSchlund_PRG2_B1.hs:16:26 in main:Main
*Main> berechneNote 111 5
"*** Exception: mehr als 100 Klausurpunkte
CallStack (from HasCallStack):
  error, called at C:\Users\sevor\Desktop\SS19\PRG2\B1\PatrikSchlund_PRG2_B1.hs:17:29 in main:Main

*Main> berechneNote 30 10
"nicht bestanden"
*Main> berechneNote 40 14
"3,7"
*Main> berechneNote 50 0
"4,0"
*Main> berechneNote 100 5
"1,0"
*Main> berechneNote 44 5
"nicht bestanden"
*Main> berechneNote 60 5
"3,0"
*Main> berechneNote 60 6
"2,7"
*Main> berechneNote 80 5.5
"1,3"
*Main> berechneNote 80.5 5.5
"1,0"
-}
-- =
-- =============================================================================

-- =============================================================================
-- =
-- = Aufgabe 2
-- =
-- = ---------------------------------------------------------------------------
-- = - zeigeLabyrinth und Beispiellabyrinthe.
-- = -

zeigeLabyrinth labyrinth =
   putStrLn $ unlines $ [[labyrinth j i | i <- [1..dimH]] | j <- [1..dimV]]
 where
   dimH = length . takeWhile (/='O') $ [labyrinth 1 i | i <- [1..]]
   dimV = length . takeWhile (/='O') $ [labyrinth i 1 | i <- [1..]]

labyrinthA 9 _  = 'O'
labyrinthA _ 16 = 'X'
labyrinthA _ 17 = 'O'
labyrinthA 1 _  = 'X'
labyrinthA 2 1  = 'X'
labyrinthA 2 8  = 'X'
labyrinthA 2 12 = 'X'
labyrinthA 2 _  = ' '
labyrinthA 3 1  = 'X'
labyrinthA 3 3  = 'X'
labyrinthA 3 5  = 'X'
labyrinthA 3 6  = 'X'
labyrinthA 3 8  = 'X'
labyrinthA 3 9  = 'X'
labyrinthA 3 10 = 'X'
labyrinthA 3 12 = 'X'
labyrinthA 3 14 = 'M'
labyrinthA 3 _  = ' '
labyrinthA 4 1  = 'X'
labyrinthA 4 2  = 'X'
labyrinthA 4 3  = 'X'
labyrinthA 4 5  = 'X'
labyrinthA 4 6  = 'X'
labyrinthA 4 12 = 'X'
labyrinthA 4 _  = ' '
labyrinthA 5 1  = 'X'
labyrinthA 5 5  = 'X'
labyrinthA 5 8  = 'X'
labyrinthA 5 9  = 'X'
labyrinthA 5 10 = 'X'
labyrinthA 5 12 = 'X'
labyrinthA 5 13 = 'X'
labyrinthA 5 15 = 'X'
labyrinthA 5 _  = ' '
labyrinthA 6 1  = 'X'
labyrinthA 6 3  = 'X'
labyrinthA 6 4  = 'X'
labyrinthA 6 5  = 'X'
labyrinthA 6 7  = 'X'
labyrinthA 6 8  = 'X'
labyrinthA 6 10 = 'X'
labyrinthA 6 _  = ' '
labyrinthA 7 1  = 'X'
labyrinthA 7 6  = 'T'
labyrinthA 7 9  = 'X'
labyrinthA 7 10 = 'X'
labyrinthA 7 11 = 'X'
labyrinthA 7 12 = 'X'
labyrinthA 7 13 = 'X'
labyrinthA 7 14 = 'X'
labyrinthA 7 15 = 'X'
labyrinthA 7 _  = ' '
labyrinthA 8 6  = 'E'
labyrinthA 8 _  = 'X'

labyrinthB 7 6  = '.'
labyrinthB 6 6  = '.'
labyrinthB 5 6  = '.'
labyrinthB 5 7  = '.'
labyrinthB 4 7  = '.'
labyrinthB 4 8  = '.'
labyrinthB 4 9  = '.'
labyrinthB 4 10 = '.'
labyrinthB 4 11 = '.'
labyrinthB 5 11 = '.'
labyrinthB 6 11 = '.'
labyrinthB 6 12 = '.'
labyrinthB 6 13 = '.'
labyrinthB 6 14 = '.'
labyrinthB 5 14 = '.'
labyrinthB 4 14 = 'T'
labyrinthB x y  = labyrinthA x y

labyrinthC 3 14 = ' '
labyrinthC 4 14 = ' '
labyrinthC x y  = labyrinthB x y


testlab 2 2 = 'M'
testlab 2 3 = '.'
testlab 2 4 = '.'
testlab 2 5 = 'T'
testlab 3 2 = ' '
testlab 3 3 = ' '
testlab 3 4 = ' '
testlab 3 5 = ' '
testlab 4 2 = ' '
testlab 5 2 = 'E'

testlab x y
    | (0 < x && x < 6) && ( 0 < y && y < 8) = 'X'
    | otherwise = 'O'
    
testlab2 2 2 = ' '
testlab2 2 5 = ' '
testlab2 x y = testlab x y


-- = -
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe a)
-- = -

breite laby = findeBreiteRek laby 1 -- rufe die rekursive suche ab 1 auf

findeBreiteRek laby col
    | (laby 1 col == 'O') = (col - 1)
    | otherwise = findeBreiteRek laby (col + 1)
    
    
hoehe laby = findeHoeheRek laby 1 -- rufe die rekursive suche ab 1 auf

findeHoeheRek laby row
    | (laby row 1 == 'O') = (row - 1)
    | otherwise = findeHoeheRek laby (row + 1)
-- = -
{-Tests:
*Main> breite labyrinthA
16
*Main> hoehe labyrinthA
8
*Main> breite testlab
7
*Main> hoehe testlab
5
-}
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe b)
-- = -

xKoord item laby = reksearchx item laby 1 1 (hoehe laby) (breite laby)

reksearchx item laby row col max_x max_y
    | ((laby row col) == item) = row -- gefunden
    | row > max_x = 0 -- !!! return 0 , da nicht gefunden
    | col > max_y = reksearchx item laby (row + 1) 1 max_x max_y -- eine zeile weiter
    | otherwise = reksearchx item laby row (col + 1) max_x max_y -- ein feld weiter


yKoord item laby = reksearchy item laby 1 1 (hoehe laby) (breite laby)

reksearchy item laby row col max_x max_y
    | ((laby row col) == item) = col -- gefunden
    | row > max_x = 0 -- !!! return 0 , da nicht gefunden
    | col > max_y = reksearchy item laby (row + 1) 1 max_x max_y -- eine zeile weiter
    | otherwise = reksearchy item laby row (col + 1) max_x max_y -- ein feld weiter

-- = -
{-TESTS:
*Main> xKoord 'M' labyrinthA
3
*Main> yKoord 'M' labyrinthA
14
*Main> xKoord 'T' labyrinthB
4
*Main> yKoord 'T' labyrinthB
14
*Main> xKoord 'M' testlab
2
*Main> yKoord 'M' testlab
2
*Main> xKoord 'T' testlab
2
*Main> yKoord 'T' testlab
5
-}
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe c)
-- = -

laufe dir laby = lauffunktion dir laby (xKoord 'T' laby) (yKoord 'T' laby) -- hilfsfunktion mit originalpos

-- Fallunterscheidung und pruefung
lauffunktion dir laby xT yT
    | dir == 1 && (isfree laby (xT -1 ) yT ) = drawnew laby xT yT (xT - 1) yT
    | dir == 3 && (isfree laby (xT +1 ) yT ) = drawnew laby xT yT (xT + 1) yT
    | dir == 0 && (isfree laby xT (yT - 1) ) = drawnew laby xT yT xT (yT - 1)
    | dir == 2 && (isfree laby xT (yT + 1) ) = drawnew laby xT yT xT (yT + 1)
    | otherwise = drawnew laby xT yT xT yT -- eine wand, alte und neue pos gleich


isfree laby x y
    | (laby x y) == 'X' = False
    | (laby x y) == 'M' = False
    | (laby x y) == 'E' = False
    | (laby x y) == 'O' = False
    | otherwise = True
    
drawnew laby altx alty newx newy = \x y -> 
    if x == newx && y == newy then 'T'
    else if x == altx && y == alty then '.'
    else laby x y
    
    
    
-- = -
-- = -
{-Tests:
*Main> zeigeLabyrinth (laufe 0 (laufe 0 (laufe 0 labyrinthA) ) )
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   X  XXX XX XX
X XXX XX X     X
X T...  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 0 (laufe 1 (laufe 0 labyrinthA) ) )
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   X  XXX XX XX
X XXX XX X     X
X  T..  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 0 (laufe 1 (laufe 1 labyrinthA) ) )
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   XT XXX XX XX
X XXX.XX X     X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 2 (laufe 0 (laufe 1 (laufe 1 labyrinthA) ) ))
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   X.TXXX XX XX
X XXX.XX X     X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 2 (laufe 0 (laufe 1 (laufe 1 (laufe 2(laufe 0 labyrinthA))) ) ))
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   X.TXXX XX XX
X XXX.XX X     X
X   ..  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth labyrinthB
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX.....X T X
X   X..XXX.XX.XX
X XXX.XX X.... X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 0 labyrinthB)
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX.....XT. X
X   X..XXX.XX.XX
X XXX.XX X.... X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 0 (laufe 1 labyrinthB))
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX.....XT. X
X   X..XXX.XX.XX
X XXX.XX X.... X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (laufe 3 (laufe 3 labyrinthA))
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X   X
X   X  XXX XX XX
X XXX XX X     X
X    T  XXXXXXXX
XXXXXEXXXXXXXXXX
-}
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe d)
-- = -

nebenMinotauros laby = fight laby (xKoord 'M' laby) (yKoord 'M' laby)
-- Die Funktion fight ist true, wenn T neben M steht
fight laby xM yM
   | laby xM (yM + 1) == 'T' = True
   | laby xM (yM - 1) == 'T' = True
   | laby (xM + 1) yM == 'T' = True
   | laby (xM - 1) yM == 'T' = True
   | otherwise = False


-- = -
{- TESTS:
*Main> nebenMinotauros labyrinthA
False
*Main> nebenMinotauros labyrinthB
True
*Main> zeigeLabyrinth testlab
XXXXXXX
XM..TXX
X    XX
X XXXXX
XEXXXXX

*Main> nebenMinotauros testlab
False
*Main> nebenMinotauros (laufe 0(laufe 0 testlab))
True
-}
-- = -
-- = ---------------------------------------------------------------------------
-- = -
-- = - Aufgabe e)
-- = -

antikePutzkraft laby = \x y -> if laby x y == '.' then ' ' else laby x y


{- TESTS:
*Main> zeigeLabyrinth labyrinthB
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX.....X T X
X   X..XXX.XX.XX
X XXX.XX X.... X
X    .  XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth (antikePutzkraft labyrinthB)
XXXXXXXXXXXXXXXX
X      X   X   X
X X XX XXX X M X
XXX XX     X T X
X   X  XXX XX XX
X XXX XX X     X
X       XXXXXXXX
XXXXXEXXXXXXXXXX

*Main> zeigeLabyrinth testlab
XXXXXXX
XM..TXX
X    XX
X XXXXX
XEXXXXX

*Main> zeigeLabyrinth (antikePutzkraft testlab)
XXXXXXX
XM  TXX
X    XX
X XXXXX
XEXXXXX
-}