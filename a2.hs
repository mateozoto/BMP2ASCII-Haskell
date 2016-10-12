-- Mateo Zoto
-- 10082263
-- Assignment 2 / CPSC 449
import Codec.BMP
import Data.ByteString
import GHC.Word
import Data.Maybe

-- Question 1
loadItWithBMP :: FilePath -> IO ([Word8], Int, Int)
loadItWithBMP file = do	Right bmp <- readBMP file
			return ((unpack (unpackBMPToRGBA32 bmp)), (fst (bmpDimensions bmp)), (snd (bmpDimensions bmp)))
-- Convert Word to an array of integers. Grabs the word, int and int from loadItWithBMP
-- passes the word into a helper function so the other integers remain untouched
-- and in wordsToInteger, fromIntegral and toInteger are applied. Reason why fromIntegral
-- is used is because toInteger only converts to Integer which is not what we are trying to get.
convertToIntegers :: ([GHC.Word.Word8], Int, Int) -> ([Int], Int, Int)
convertToIntegers (x, y, z) = (wordToInteger(x), y, z)
-- Pass in the words generated from loadItWithBMP and recursively convert to an Integer through the use of fromIntegral.
-- toInteger converts to type Integer which is not equivalent to what we want (Int)
wordToInteger :: [GHC.Word.Word8] -> [Int] 
wordToInteger [] = []
wordToInteger (h:t) = fromIntegral (toInteger h) : wordToInteger t
-- Convert an array of integers with the width and height to a matrix
-- First take the list of numbers with their width and height and transform them into a single tuple.
-- Then for every tuple pair, essentially concatenate them with each other from top to bottom.
convertToMatrix :: ([Int], Int, Int) -> [[(Int, Int, Int, Int)]]
convertToMatrix (x, y, z) = matrixCreate(tupleCreate x) y z
-- Traverse from 0 to the max of width in the array of tuples and concatenate them from 0 to the max length
-- Input is a list of tuples with the width and height of the matrix. Width and height are the width and height
-- that determine the size of the picture. The matrix replicates that
matrixCreate :: [(Int, Int, Int, Int)] -> Int -> Int -> [[(Int, Int, Int, Int)]]
matrixCreate _ _ 0 = []
matrixCreate pair w h = [pair!!i | i <- [0..(w-1)]] : matrixCreate (([pair!!j | j <- [w..((Prelude.length pair) - 1)]])) w (h - 1)
-- Grab a list of integer and turn it into a list of tuples. Grab the first 4 elements you see in the head, and then pass the tail into the 
-- into the function. Rinse and repeat. If the tuple list is empty, return an empty list.
tupleCreate :: [Int] -> [(Int, Int, Int, Int)]
tupleCreate [] = []
-- grab the index of the 0th, 1st, 2nd and 3rd tuple, return that in as the header, and recursively call the tail which should be the 4th element (hence 3 tail calls)
tupleCreate x = (x!!0, x!!1, x!!2, x!!3) : tupleCreate(Prelude.tail(Prelude.tail(Prelude.tail(Prelude.tail x))))
-- Take the list of tuples containing R G B A, only focus on R G B, divide them by two magic numbers
-- to convert them into greyscale. Recursively call the tail
convertToGreyscale :: [[(Int, Int, Int, Int)]] -> [[Float]]
convertToGreyscale [] = []
convertToGreyscale (h:t) = colorToGrey h : convertToGreyscale t

colorToGrey :: [(Int, Int, Int, Int)] -> [Float]
colorToGrey [] = []
-- take the sum of red blue and green, divide by 3, then  divide by 256 which makes the range of numbers go from 0-1 which is greyscale
-- keep recursively calling the tail until every number in the list of tiples has converted into a grey scale number
colorToGrey ((r, g, b, a):t) = ((0.21 * fromIntegral r) + (0.71 * fromIntegral g) + (0.07 * fromIntegral b)) / 256 : colorToGrey t
-- Take the floating point number which we know is between 0 and 1, and pass it into floatToChar which will replace characters
-- as it parses through the array depending on what values it sees.
convertToCharacters :: [[Float]] -> [[Char]]
convertToCharacters [] = []
convertToCharacters (h:t) = floatToChar h : convertToCharacters t
-- We know that every value in the matrix will be from 0 - 1 where 0 is dark and 1 is light. 
-- Based on these values, we will generalize them from 1/12 to 1 and replace what we see
-- with the necessary ASCII symbols
floatToChar :: [Float] -> [Char]
floatToChar [] = []
floatToChar (x:xx)
		| x < (1/12) = '@': floatToChar xx
		| x < (2/12) = '&': floatToChar xx
		| x < (3/12) = '#': floatToChar xx
		| x < (4/12) = '^': floatToChar xx
		| x < (5/12) = '!': floatToChar xx
		| x < (6/12) = '-': floatToChar xx
		| x < (7/12) = '*': floatToChar xx
		| x < (8/12) = '`': floatToChar xx
		| x < (9/12) =  ',': floatToChar xx
		| x < (10/12) =  '.': floatToChar xx
		| x < (11/12) =  '"': floatToChar xx
		| x < (1) = ' ': floatToChar xx
-- Helper function to do all the conversitions and convert from BMP to ASCII.
-- Based of LoadItWithBMP
-- ConvertToInt is called first, followed by converting into a matrix which takes care of tuples as well, then greyscale conversion and then character conversion. This is then
-- reversed and we use putStr and unlines as per assignment spec.
create file = do
	Prelude.putStr (unlines (Prelude.reverse (convertToCharacters (convertToGreyscale (convertToMatrix (convertToIntegers (file)))))))

-- Question 2
-- Data structure consisting of True, False and None
-- deriving S
data TripleBoolean = Troo | Falz | None deriving (Show)

-- Based off Klene Logic
-- True is false, false is true, none is none.
ternaryNOT :: TripleBoolean -> TripleBoolean
ternaryNOT Troo = Falz
ternaryNOT Falz = Troo
ternaryNOT None = None

--Input is two operands which can be True, False or None. Necessary truth values are assigned.
ternaryAND :: TripleBoolean -> TripleBoolean -> TripleBoolean
ternaryAND Falz _ = Falz
ternaryAND _ Falz = Falz
ternaryAND None _ = None
ternaryAND _ None = None
ternaryAND _ _ = Troo

--Input is two operands which can be True, False or None. Necessary truth values are assigned.
ternaryOR :: TripleBoolean -> TripleBoolean -> TripleBoolean
ternaryOR Troo _ = Troo
ternaryOR _ Troo = Troo
ternaryOR None _ = None
ternaryOR _ None = None
ternaryOR _ _ = Falz

--Input is a Maybe Bool of which consists of a Just value or Nothing. In this case the value
--is the actual boolean in True or False. Same logic applies to the bottom tables
--Truth values based of Kleene Logic and Lazy Evaluation is used for all these
--truth functions in order to minimize redundancy 
ternaryNOT' :: (Maybe Bool) -> (Maybe Bool)
ternaryNOT' (Just True) = (Just False)
ternaryNOT' (Just False) = (Just True)
ternaryNOT' (Nothing) = (Nothing)

ternaryAND' :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
ternaryAND' (Just False) _ = (Just False)
ternaryAND' _ (Just False) = (Just False)
ternaryAND' (Nothing) _ = (Nothing)
ternaryAND' _ (Nothing) = (Nothing)
-- Everything else would be true
ternaryAND' _ _ = (Just True)

ternaryOR' :: (Maybe Bool) -> (Maybe Bool) -> (Maybe Bool)
ternaryOR' (Just True) _ = (Just True)
ternaryOR' _ (Just True) = (Just True)
ternaryOR' (Nothing) _ = (Nothing)
ternaryOR' _ (Nothing) = (Nothing)
-- Everything else would be false
ternaryOR' _ _ = (Just False)


-- Question 3
data GTree a = Leaf a | Gnode [GTree a] deriving (Eq, Show)

sampleTree :: GTree Int
sampleTree = Gnode [Gnode [Leaf 1, Leaf 2, Leaf 3], Gnode [Leaf 4, Leaf 5, Leaf 7, Gnode [Leaf 8]], Gnode [Leaf 15], Gnode[Leaf 20]]
-- Recursively count every leaf a in the tree. If it sees 1 leaf, it will add it to the count, pass the tail into the 
-- recursion, add 1 if it sees a leaf, pass the tail into recursion and will stop until the Gnode is empty.
-- Base case
calculateLeafCount :: (GTree a) -> Int
-- Terminating condition and Base case
calculateLeafCount (Gnode []) = 0
-- Base case when there is one leaf
calculateLeafCount (Leaf a) = 1
-- If you see a leaf, add it to the leaf count and keep recursively calling the tree until we run out of leaves
calculateLeafCount (Gnode (h:t)) =  calculateLeafCount h + calculateLeafCount (Gnode t)
-- Same thing as the above but instead we map the tree, and grab go towards the max element to traverse through the tree
-- while passing in the tail of the mapped max into the recurssion. keep incrementing by one until we hit an empty tree
calculateTreeDepth :: (GTree a) -> Int
calculateTreeDepth (Gnode []) = 0
calculateTreeDepth (Leaf a) = 1
calculateTreeDepth (Gnode a) = 1 + Prelude.maximum (Prelude.map calculateTreeDepth a)
-- Similar to leafCount, same logic with the only difference being we take the input of the Leaf int's instead of the actual objects and increment based of
-- whatever integers the leafs have.
calculateSumTree :: (GTree Int) -> Int
calculateSumTree (Gnode []) = 0
calculateSumTree (Leaf a) = a
calculateSumTree (Gnode (h:t))  = calculateSumTree h + calculateSumTree (Gnode t)
-- Grab input that the user is trying to look for an traverse through the tree and check each leaf if it equals the value.
-- If it does, return true eitherwise return false.
findElemInTree :: Eq a => a -> (GTree a) -> Bool
findElemInTree ind (Gnode (Gnode h:t))
	| findElemInTree ind (Gnode h) == False = findElemInTree ind (Gnode t)
	| findElemInTree ind (Gnode h) /= False = True
findElemInTree ind (Gnode []) = False
findElemInTree ind (Gnode (Leaf h:t))
	| ind == h = True
	| otherwise = findElemInTree ind (Gnode t)
-- Once we actually hit the leaf, return true if a is equal to the value we are searching for, eitherwise it's false.
findElemInTree ind (Leaf a)
	| ind /= a = False
	| ind == a = True