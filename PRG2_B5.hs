-- ============================================================================
-- Grundlagen der Programmierung 2
-- Aufgabenblatt 5
-- ============================================================================
import Data.Char
import Data.List
-- = Aufgabe 1
-- =

data BBaum a = BBlatt a | BKnoten a (BBaum a) (BBaum a)
  deriving(Eq,Show)

data NBaum a = NBlatt a | NKnoten a [NBaum a]
  deriving(Eq,Show)

--
-- b)
--
techno_nk = NKnoten "Techno" [NBlatt "Sash - Ecuador", NBlatt "The Prodigy - Need Some1", NBlatt "Hardsequencer - Plastic Fantastic"]
rock_nk = NKnoten "Rock" [NBlatt "Offspring - Da Hui", NBlatt "Shark - California Grrls", NBlatt "Audioslave - Your Time Has Come"]
pop_nk = NKnoten "Pop" [NBlatt "Ed Sheeran - Shape Of You", NBlatt "Pink - JUst Like a Pill", NBlatt "Mark Forster - Sowieso"]
musik = NKnoten "Musik" [techno_nk,rock_nk,pop_nk]

-- 
-- c)
--
testbb1 = BKnoten 3 (BBlatt 3) (BKnoten 5 (BBlatt 8) (BBlatt 8)) -- True
testbb2 = BKnoten 3 (BBlatt 3) (BKnoten 5 (BBlatt 8) (BBlatt 9)) -- False

alladded tree = summiere tree 0 -- start with 0

summiere (BBlatt x) summe_hierher = (x == summe_hierher)
summiere (BKnoten x b1 b2) summe_hierher = (summiere (b1) (summe_hierher + x)) && (summiere (b2) (summe_hierher + x))

{-Tests:
*Main> alladded testbb1
True
*Main> alladded testbb2
False -}
--
-- d)
wtf1 = NKnoten testbb1 [NBlatt testbb1,NBlatt testbb2]
--

btreestontrees ntree = runthrough ntree

makenatural (BBlatt a) = NBlatt a
makenatural (BKnoten a x y) = NKnoten a [(makenatural x),(makenatural y)]
runthrough (NBlatt bbaumx) = NBlatt (makenatural bbaumx)
runthrough (NKnoten bbaumx listofnext) = NKnoten (makenatural bbaumx) (map runthrough listofnext)

{-Tests:
*Main> wtf1
NKnoten (BKnoten 3 (BBlatt 3) (BKnoten 5 (BBlatt 8) (BBlatt 8)))
[NBlatt (BKnoten 3 (BBlatt 3) (BKnoten 5 (BBlatt 8) (BBlatt 8))),NBlatt (BKnoten 3 (BBlatt 3) (BKnoten 5 (BBlatt 8) (BBlatt 9)))]
*Main> btreestontrees wtf1
NKnoten (NKnoten 3 [NBlatt 3,NKnoten 5 [NBlatt 8,NBlatt 8]])
[NBlatt (NKnoten 3 [NBlatt 3,NKnoten 5 [NBlatt 8,NBlatt 8]]),NBlatt (NKnoten 3 [NBlatt 3,NKnoten 5 [NBlatt 8,NBlatt 9]])]
-}
-- =
-- Aufgabe 2a)
-- 

tempoAnpassen :: Entwuerfe -> Entwuerfe
tempoAnpassen (Entwuerfe b1 b2 bpm)= Entwuerfe (dupelen b1) (dupelen b2) 185

dupelen (Blatt listofnotes) = Blatt (map enlen listofnotes)
dupelen (Knoten listofnotes more) = Knoten (map enlen listofnotes) (map dupelen more)

enlen (oc,th,len,vol) = (oc,th,len*2,vol)

{-Tests: entwuerfe letzte 5 noten + tempo:
,(3,4,3,100),(3,3,2,100),(3,1,2,100),(3,4,2,100),(3,6,2,100)]]]]) 152
und letzten 5 von tempoAnpassen entwuerfe:
,(3,4,6,100),(3,3,4,100),(3,1,4,100),(3,4,4,100),(3,6,4,100)]]]]) 185
-}
--
-- b)
--
testnoten = [(5,3,2,100),(-1,-1,1,100),(2,3,2,100),(5,6,8,100),(5,9,4,100),(5,10,4,100),(6,1,2,100),(5,12,2,100)]

transponiere :: Entwuerfe -> Int -> Entwuerfe
transponiere (Entwuerfe b1 b2 bpm) plusx = Entwuerfe (transp_b plusx b1) (transp_b plusx b2) bpm

--For Trees
transp_b plusx (Blatt listofnotes) = Blatt (map (transp plusx) listofnotes)
transp_b plusx (Knoten listofnotes more) = Knoten (map (transp plusx) listofnotes) (map (transp_b plusx) more)

--For Notes:
transp plusx (oc,-1,l,v) = (oc,-1,l,v) -- Pause

transp plusx (oc,th,l,v) = fitto12 (oc,th+plusx,l,v)

fitto12 (oc,th,l,v)
    | th < 1 = fitto12 (oc-1,th+12,l,v)
    | th <= 12 = (oc,th,l,v)
    | otherwise = fitto12 (oc+1,th-12,l,v)

-- =
{-Tests der transp:
*Main> testnoten
[(5,3,2,100),(-1,-1,1,100),(2,3,2,100),(5,6,8,100),(5,9,4,100),(5,10,4,100),(6,1,2,100),(5,12,2,100)]
*Main> map (transp 1) testnoten
[(5,4,2,100),(-1,-1,1,100),(2,4,2,100),(5,7,8,100),(5,10,4,100),(5,11,4,100),(6,2,2,100),(6,1,2,100)]
*Main> map (transp 11) testnoten
[(6,2,2,100),(-1,-1,1,100),(3,2,2,100),(6,5,8,100),(6,8,4,100),(6,9,4,100),(6,12,2,100),(6,11,2,100)]
*Main> map (transp (-18)) testnoten
[(3,9,2,100),(-1,-1,1,100),(0,9,2,100),(3,12,8,100),(4,3,4,100),(4,4,4,100),(4,7,2,100),(4,6,2,100)]
====================
Tests:
letzte 5 plus tempo orig:
,(3,4,3,100),(3,3,2,100),(3,1,2,100),(3,4,2,100),(3,6,2,100)]]]]) 152
von transponiere entwuerfe 8
(3,12,3,100),(3,11,2,100),(3,9,2,100),(3,12,2,100),(4,2,2,100)]]]]) 152
von transponiere entwuerfe (-8)
,(2,8,3,100),(2,7,2,100),(2,5,2,100),(2,8,2,100),(2,10,2,100)]]]]) 152
-}
-- =
-- =============================================================================