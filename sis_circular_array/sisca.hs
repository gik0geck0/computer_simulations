
import System.Environment

data State = State {
    demand :: Int,
    ordered :: Int,
    inventory :: Int
} deriving (Show)

-- Model specifications
-- Start AND End with maxInventory
--

-- Evaluated at each step of the pseudo code's while loop
-- Each step will take the current list of states, and add its state to the
-- beginning. This means that the state for (i-1) will be the head of the list
-- until the i'th state is added to the beginning
oneStep :: Int -> Int -> [State] -> Int -> [State]
-- Initialization step.
oneStep _ _ [] _ = []
-- The "rest" of the steps will do this
oneStep minInventory maxInventory (prev:rest) dmnd = 
    ( State dmnd 0 (inventory prev + (if inventory prev < minInventory then maxInventory - inventory prev else 0) - dmnd) )
    : ( State (demand prev) (if inventory prev < minInventory then maxInventory - inventory prev else 0) (inventory prev) )
    : rest

lastStep :: Int -> Int -> [State] -> [State]
lastStep _ _ []                                 = []
lastStep minInventory maxInventory (prev:rest)  = (State (demand prev) (maxInventory - inventory prev) maxInventory ):rest

stateTransform :: State -> (Int, Int, Int)
stateTransform s = (demand s, ordered s , inventory s)

main = do 
    f <- getArgs
    demandFile <- readFile $ head f
    print $ map stateTransform $ tail ( reverse $ lastStep 6 10 ( foldl (oneStep 6 10) [State 0 0 10] $ map read $ lines demandFile) )
