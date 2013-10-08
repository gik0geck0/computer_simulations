
import System.Environment
import Debug.Trace

data State = State {
    demand :: Float,
    ordered :: Float,
    inventory :: Float
} deriving (Show)

getDem :: State -> Float
getDem st = demand st

getOrd :: State -> Float
getOrd st = ordered st

getInv :: State -> Float
getInv st = inventory st


-- Model specifications
-- Start AND End with maxInventory
--

-- Evaluated at each step of the pseudo code's while loop
-- Each step will take the current list of states, and add its state to the
-- beginning. This means that the state for (i-1) will be the head of the list
-- until the i'th state is added to the beginning
oneStep :: Float -> Float -> [State] -> Float -> [State]
oneStep _ _ [] _ = []
oneStep minInventory maxInventory (prev:rest) dmnd = 
    ( State dmnd 0 (inventory prev + (if inventory prev < minInventory then maxInventory - inventory prev else 0) - dmnd) )
    : ( State (demand prev) (if inventory prev < minInventory then maxInventory - inventory prev else 0) (inventory prev) )
    : rest

lastStep :: Float -> Float -> [State] -> [State]
lastStep _ _ []                                 = []
lastStep minInventory maxInventory (prev:rest)  = (State (demand prev) (inventory prev) maxInventory ):rest

stateTransform :: State -> (Float, Float, Float)
stateTransform s = (demand s, ordered s , inventory s)

-- Returns a list of points that represent the Inventory AT that time (one 
-- state = 1 interval), the demand that got it to that inventory, and the amount
-- that was ordered just after the inventory was recorded
simulateSisca :: Float -> String -> Int -> Float -> [State]
simulateSisca maxInv dmndFile shift minInv = reverse $ lastStep minInv maxInv ( foldl (oneStep minInv maxInv) [State 0 0 80] $ dataShift shift $ map read $ lines dmndFile)
-- [30, 15, 25, 15, 45, 30, 25, 15, 20, 35, 20, 30]))

dataShift :: Int -> [Float] -> [Float]
--dataShift nShift dataList
--     | trace (show $ drop nShift dataList ++ take nShift dataList) False = undefined
dataShift nShift dataList = drop nShift dataList ++ take nShift dataList

-- Calculation definitions
-- Holding cost = chold * avg inventory > 0
-- shortage cost = cshort * avg inventory < 0

madeOrder :: State -> Float
madeOrder s = if getOrd s > 0 then 1 else 0

-- Find an average statistic on a list of States. The first function will be used as an extractor function
avgStat :: (State -> Float) -> [State] -> Float
avgStat getter states = (sumStates getter states 0) / (fromIntegral (length states))

sumStates :: (State -> Float) -> [State] -> Float -> Float
sumStates _ [] accum      = accum
sumStates getter (x:xs) accum  =  sumStates getter xs $ accum + (getter x)

avgOrd = avgStat getOrd
avgInv states = avgPosInv states - avgNegInv states
avgDem = avgStat getDem
avgU = avgStat madeOrder

getPosInv :: State -> Float
getPosInv state = if inventory state > 0 
    then inventory state + demand state / (2)
    else (( inventory state + demand state ) ^ 2) / ( 2 * demand state)

getNegInv :: State -> Float
getNegInv state = if inventory state > 0
    then 0
    else ( inventory state ) ^ 2 / ( 2 * demand state )

avgPosInv :: [State] -> Float
avgPosInv stateList = ( avgStat getPosInv stateList )

avgNegInv :: [State] -> Float
avgNegInv = avgStat getNegInv

-- Item cost = citem * avg ordered
itemCost :: [State] -> Float
itemCost = (*8000) . avgOrd

-- setup cost = csetup * (orders / n)
-- U is either 1 or 0, for whether or not there was an order
setupCost :: [State] -> Float
setupCost = (*1000) . avgU

-- Holding cost is 25$ per week
holdingCost :: [State] -> Float
holdingCost = (*25) . avgPosInv

-- Shortage cost is 100$ per week
shortageCost :: [State] -> Float
shortageCost = (*700) . avgNegInv

dependantCost :: [State] -> Float
dependantCost states = setupCost states + holdingCost states + shortageCost states

--minimizer :: ([State] -> Float) -> [State] -> Int
--minimizer 

optimizer :: (Float -> [State]) -> Float -> Float -> ([State] -> Float) -> [(Float, Float)]
optimizer simulator begInv endInv statFunction = zip [begInv..endInv] $ map statFunction $ map simulator [begInv..endInv]

put = putStr . show
putLn = putStrLn . show

main = do 
    args <- getArgs
    demandFile <- readFile $ args !! 2
    if (args !! 0) == "R"
        then let stateList = simulateSisca 60 demandFile 20 (read $ args !! 1)
            in do
                putStr "OUTPUT "
                put $ setupCost stateList
                putStr " "
                put $ shortageCost stateList
                putStr " "
                put $ holdingCost stateList
                putStr " "
                put $ dependantCost stateList
                putStrLn ""
    else if (args !! 0) == "C"
        then let stateList = simulateSisca 80 demandFile (read $ args !! 3) (read $ args !! 1)
            in do
                putStr "OUTPUT "
                put $ setupCost stateList
                putStr " "
                put $ shortageCost stateList
                putStr " "
                put $ holdingCost stateList
                putStr " "
                put $ dependantCost stateList
                putStrLn ""
    else if (args !! 0) == "G1"
        then let stateList = simulateSisca 80 demandFile 0 (read $ args !! 1)
            in do
                mapM_ putLn $ map getInv stateList
    else if (args !! 0) == "G2"
        then let stateList = optimizer (simulateSisca 80 demandFile (read $ args !! 3)) 0 50 setupCost
            in do
                mapM_ putLn $ map snd stateList
    else if (args !! 0) == "G3"
        then let stateList = optimizer (simulateSisca 80 demandFile (read $ args !! 3)) 0 50 shortageCost
            in do
                mapM_ putLn $ map snd stateList
    else if (args !! 0) == "G4"
        then let stateList = optimizer (simulateSisca 80 demandFile (read $ args !! 3)) 0 50 holdingCost
            in do
                mapM_ putLn $ map snd stateList
    else if (args !! 0) == "G5"
        then let stateList = optimizer (simulateSisca 80 demandFile (read $ args !! 3)) 0 50 dependantCost
            in do
                print $ args !! 3
                mapM_ putLn $ map snd stateList
    else putStr "Unacceptable simulation. Please use C or R"
