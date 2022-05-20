{- | Author:   Tuohuang Li <tuohuangl@unimelb.edu.au>
     Student ID: 1205166

Purpose:  Play a two player guessing game - Battleship Game on a 4×8 grid. 
   
-------------------------------The Game------------------------------
There are two roles - a searcher and a hider. The searcher will repeatedly 
trying to find the locations of three battleships according to the feedback 
provided by the hider. The feedback is a set of 3 numbers indicates how many 
guesses out of three either hit the hidden battleship or is 1 distance away 
or 2 distance away from the hidden locations. The game continues the above 
steps until the searcher successfully found all 3 hidden ballteship.

-}

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where
              
import Data.List
import Data.Char

-- | Created data constructor "Location" which takes two Chars as its values.
data Location = Location Char Char deriving (Eq, Ord)

-- | The instance declareation enables the Location datatype to access the 
-- Show class.
instance Show Location where
    show (Location col row) = [col, row]

-- | The toString function converts Location variables to a String type:
-- 'A' '1' -> "A1"
toString :: Location -> String
toString (Location col row) = [col, row]

-- | Create 'type' - GameState
type GameState = [[Location]]

-- | allGameState generates 4960 different combinations of 3 Locations.
allGameState :: [[Location]]
allGameState = [[Location a1 b1, Location a2 b2, Location a3 b3] |
   a1 <- ['A' .. 'H'],
   a2 <- ['A' .. 'H'],
   a3 <- ['A' .. 'H'],
   b1 <- ['1' .. '4'],
   b2 <- ['1' .. '4'],
   b3 <- ['1' .. '4'],
   [a1, b1] < [a2, b2],
   [a2, b2] < [a3, b3]]

-- | The toLocation function will Check if the Location is valid.
-- the colomn of the location must in the range 'A' up to and including 
-- 'H'; The row must in the range '1' up to and including '4'.
toLocation :: String -> Maybe Location
toLocation loc =
    if length loc == 2 && (c >= 'A' && r <= 'H') 
        && (r >= '1' && r <='4')
    then Just (Location c r)
    else Nothing
    where c = head loc
          r = last loc

-- | The from Location function gives back the specified location in the
-- form of String
fromLocation :: Location -> String
fromLocation = show

-- | The feedback function has two arguments: target and guess; it gives
-- back a a tuple with 3 elements - (d0, d1, d2).
-- Where d0 denotes the total number of correct guesses; d1 denotes total 
-- number of guesses that are 1 distance away from the target; d2 denotes 
-- the total number of guesses that are 2 distance away from the target.
-- Where the target denotes 3 hidden battleship locations and the guess
-- denotes 3 guessed Location List
-- This function will calculate the distance for each guessed location 
-- with the target locations, and choose the smallest distance.
-- After obtained the distances for each guessed Location, the result 
-- will be stored in tuple - (d0, d1, d2).
-- If the smallest distance is greater than 2, discard.
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _   []               = (0, 0, 0)
feedback tg (gu:guess)
    | gu `elem` tg            =  (d0 + 1, d1, d2)
    | checkLength 1 distance  =  (d0, d1 + 1, d2)
    | checkLength 2 distance  =  (d0, d1, d2 + 1)
    | otherwise               =  (d0, d1, d2)
    where
        distance = minimum (map (locDiff gu) tg)
        (d0, d1, d2) = feedback tg guess  

-- | The checkLength function has two Int arguments and returns a Boolean
checkLength :: Int -> Int -> Bool
checkLength n dis =
    n == dis

-- | The locDiff function takes two Locations as arguments; it returns
-- the distance between them.
-- The method to find the distance between two Locations is to calculate
-- the distance between rows and columns seperately, then choose the maximum
-- value between 2 results.
-- Before the calculation, row values will be converted to Int; column
-- values will be converted to ASCII codes, for example:
-- A' = 65; 'B' = 66
locDiff :: Location -> Location -> Int
locDiff (Location c1 r1) (Location c2 r2) =
    max (absDistance col1 col2) (absDistance row1 row2)
    where col1 = ord c1
          col2 = ord c2
          row1 = digitToInt r1
          row2 = digitToInt r2
          
-- | The distance function takes two Int arguments and returns a value
-- of the absolute difference between them.
absDistance :: Int -> Int -> Int
absDistance x y =
    if x - y < 0
    then y - x
    else x - y

-- | The initialGuess function gives a tuple of two, containing the Manually
-- selected 3 Locations and the initialGameState - allGameState substracting
-- the initial guessed Location.
-- For the manually selection: after many times of testing, the current
-- initialGuess yields the best result so far - "A3", "H1", "H3".
-- First of all, I decided to choose locations which are far away from
-- each other accross the whole game board (such as: "A1", "E1", "H2"). 
-- However, it will takes around 6 guesses to hit the targets. 
-- Then, I found that if I choose the initialGuesses from the side of the
-- board, by applying the techniques of remove symmetry, the impossible
-- locations can be eliminated efficiently but computationally expensive.
-- The program can easily ran for too long with a bad decision for the 
-- initial guesses (for example: "D2", "D3", "E3").
initialGuess :: ([Location], GameState)
initialGuess = (guess, initGameState)
    where
        guess         = [Location 'A' '3', Location 'H' '3', Location 'H' '1']
        initGameState = allGameState

-- | The nextGuess takes two variables: a tuple and the previous feedback, 
-- where the tuple is the combination of previous guesses and previous 
-- game state.
-- By using the "remove symmetry" techniques, the remaining possible 
-- candidates list contains those locations that are yield the same feedback
-- when checking the distance of each member in the previous GameStat with
-- the previous guess.
-- This function returns the best nextGuess and the new GameState.
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (prevGuess, prevGameState) fb = (newGuess, nextGameState)
    where
        updatedGameStates = updateGameState prevGameState prevGuess fb
        nextGameState = delete prevGuess updatedGameStates
        newGuess = bestGuess nextGameState
        
-- | The updateGameState function takes previous gamestage and the previous 
-- guess as an input, and returns a new GameState where all candidates 
-- By using the "remove symmetry" techniques, the remaining possible 
-- GameState contains gamestates when choose locations that are yield 
-- the same feedback.
updateGameState :: GameState -> [Location] -> (Int,Int,Int) -> GameState
updateGameState [] _ _ = []
updateGameState (x:xs) prevGuess fb
    | feedback x prevGuess == fb = 
        x:updateGameState xs prevGuess fb
    | otherwise = updateGameState xs prevGuess fb
          
-- | The bestGuess function take one variable - the previous GameState.
-- For each member in the GameStates, compute the expected value from 
-- a predefined formular. With this formular, the expected average 
-- number of candidates when choosing a certain guess from the previous
-- GameState can be obtained.
-- The function returns a new guess with the minimun number of candidates
--left on the board.
bestGuess :: GameState -> [Location]
bestGuess gameState = fst bestGuess 
    where
        expectedValueList  = [avgGuesses guess gameState | guess <- gameState]
        stateValueTuple    = zip gameState expectedValueList
        sortedPair         = sortBy sortTuple stateValueTuple
        bestGuess          = head sortedPair

-- | The sortTuple function is a helper for the bestGuess function.
-- It takes two tuples and sort by the second values in each tuple.
-- The function returns the sorted tuples in ascending order.
sortTuple :: (Ord b) => (a, b) -> (a, b) -> Ordering
sortTuple x y =
    if snd x < snd y
    then GT
    else LT

-- | The averageGuesses function takes two veriables: the guess and the 
-- GameState.
-- It will calculate and obtain a list of feedbacks between the guess and 
-- each member of GameStates without the guess itself.
-- Then sort and group the feedbacks list to find the distinct feedback lists
-- as well as a list of the number of each distinct lists.
-- Finally, by calculating using the predefined formular, the function can
-- find the minimun number of remaining candidates on board when use the
-- current guess.
avgGuesses :: [Location] -> GameState -> Double
avgGuesses guess restGamestate = sum (map (\x -> x * x/total) occurrenceList)
    where
        middlestate = delete guess restGamestate
        stateFeedBacks = [feedback rest guess | rest <- middlestate] 
        sortedFbList = sort stateFeedBacks
        groupedList = group sortedFbList
        occurrenceList = map (fromIntegral . length) groupedList
        total = fromIntegral $ length stateFeedBacks 

