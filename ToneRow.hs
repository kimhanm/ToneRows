module MusicNotes where

import qualified Data.List as L

--------------------------
--  --  Data Types  --  --
--------------------------

--  --  Classical Notation  --  --

data Letter = C | D | E | F | G | A | B deriving Show

ltrs  = [C, D, E, F, G, A, B]


data Alteration = Natural | Flat | DoubleFlat | Sharp | DoubleSharp 
                deriving Show

alts = [Natural, Sharp, Flat, DoubleSharp, DoubleFlat]


data Note   = Note Letter Alteration

type Notes  = [Note]

instance Show Note 
    where
        show (Note l Natural) = show l
        show (Note l a) = show l ++ "-" ++ show a



--  --  String Notation --  --

cltrs      = ['A' .. 'G']
-- Natural, Sharp, Flat, DoubleSharp, DoubleFlat
calts  = ['§','#', 'b', 'x', 'p']



--  --  Pitch Class Notation  --  --

type PitchClass = Int
type PCSet      = [PitchClass]



----------------------------------------
--  --  Conversion Between Types  --  --
----------------------------------------

--  --  Classical -> Pitch Class  --  --

pitchClass :: Note -> PitchClass
pitchClass (Note l a) = mod (l2Class l + a2Class a) 12


pitchClasses :: Notes -> PCSet
pitchClasses = map pitchClass


l2Class :: Letter -> PitchClass
l2Class C = 0
l2Class D = 2
l2Class E = 4
l2Class F = 5
l2Class G = 7
l2Class A = 9
l2Class B = 11


a2Class :: Alteration -> PitchClass
a2Class Natural     = 0
a2Class Sharp       = 1
a2Class Flat        = -1
a2Class DoubleSharp = 2
a2Class DoubleFlat  = -2


--  --  Pitch Class -> Classical  --  --

-- prefers Sharps > Flats
pc2Note :: PitchClass -> Note
pc2Note n = head  [ Note l a  
                  | a <- alts
                  , l <- ltrs 
                  , pitchClass (Note l a) == n
                  ]

pcs2Notes :: PCSet -> Notes
pcs2Notes = map pc2Note



--  --  Strings -> Classical/ToneRows --  -- 

char2l :: Char -> Letter
char2l 'C' = C
char2l 'D' = D
char2l 'E' = E
char2l 'F' = F
char2l 'G' = G
char2l 'A' = A
char2l 'B' = B


char2a :: Char -> Alteration
char2a '§' = Natural
char2a '#' = Sharp
char2a 'b' = Flat
char2a 'x' = DoubleSharp
char2a 'p' = DoubleFlat


-- invalid string becomes empty list
str2Notes :: String -> Notes
str2Notes xs = aux xs [] where
    -- aux keeps track of stack, containing a note
    aux :: String -> [Char] -> Notes
    aux ""     []           = []
    aux (c:cs) [] 
        | c `elem` cltrs    = aux cs [c]
        | otherwise         = [] -- invalid char
    aux ""     (d:ds) 
        | d `elem` cltrs    = [Note (char2l d) Natural]
        | otherwise         = [] -- invalid char
    aux (c:cs) (d:ds)
        | d `elem` cltrs -- Apply Alteration to Note
          && c `elem` calts = [Note (char2l d) (char2a c)] ++ aux cs []
        | c `elem` cltrs    = [Note (char2l d) Natural] ++ aux cs [c]
        | otherwise         = [] -- invalid char


str2PCS :: String -> PCSet
str2PCS = (map pitchClass) . str2Notes



--------------------------------
--  --  Analytical Tools  --  --
--------------------------------

--  --  General --  --  

isUnique :: (Num a, Eq a) => [a] -> Bool
isUnique []       = True
isUnique (x:[])   = True
isUnique (x:y:xs) = not ( 0 `elem` [x - z | z <- (y:xs)]) && isUnique (y:xs)

--  --  Classical Methods


-- prefers sharps over flats
enharms :: Note -> Notes
enharms x = [Note l a | a <- alts, l <- ltrs, pitchClass (Note l a) == pitchClass x]


--  --  Tone Row Tools

is12ToneRow :: PCSet -> Bool
is12ToneRow xs
    | length xs == 12 = isUnique xs
    | otherwise       = False



--------------------------
--  --  Operations  --  --
--------------------------

--  --  Tone Row Operations 

transpose :: Int -> PCSet -> PCSet
transpose n = (map (`mod` 12)) . (map (+ n))

-- cycles elements leftwards
-- cycle :: Int -> PCset -> PCSet
-- cycle 0        = id
-- cycle _ []     = []
-- cycle n (x:xs) = cycle (n-1) (xs ++ [x])

retrograde :: PCSet -> PCSet
retrograde = reverse

inverse :: PCSet -> PCSet
inverse []      = []
inverse (x:xs)  = transpose (2 * x) $ map ((-) 12) (x:xs)


-- Finding the interval symmetric tone rows

-- Symmetric tone rows are uniquely determined by
-- first 6 notes and up to 5 notes up to shift.
-- Decreases search space from 12! = 479 001 600
-- by factor of 1 million to (11 `choose` 5) = 462

-- choose xs k generates all 
-- unique k element subsets of xs
--choose :: [a]-> Int -> [a]
--choose xs k = [x : (chose xs (k-1) 
  -- | not $]


-- brings collection of pitchClasses to normal form.
-- sorted in ascending order, minimizing largest interval
normalForm :: PCSet -> PCSet
normalForm = undefined


 intervalClasses [] = []
 intervalClasses = undefined
--intervalClasses (x:xs) = L.sort [x - y `mod` 12 | y <- xs] ++ intervalClasses xs

-- divides the 12 notes into two symmetric partitions
-- the interval classes of each partition are the same
sympartition :: ([PitchClass],[PitchClass])
sympartition = undefined





