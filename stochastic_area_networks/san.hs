
import Data.List (intercalate, unfoldr, foldl')
import Data.Traversable (mapAccumL)
import Debug.Trace
import System.Environment
import System.Random
import Text.Printf

--            A -> B with edgeWeight
type Edge = (Int, Int, Double)
type Path = [Edge]
getSNode :: Edge -> Int
getSNode (s, e, w) = s

getENode :: Edge -> Int
getENode (s, e, w) = e

getWNode :: Edge -> Double
getWNode (s, e, w) = w

type SANGraph = [Edge]

getNeighbors :: [Edge] -> Int -> [Edge]
getNeighbors edgeList startNode = [ x | x <- edgeList, getSNode x == startNode ]

append :: [a] -> a -> [a]
append l i = l ++ [i]

-- take a path, and expand it to a list of possible paths
-- Map the end-node to all its neighbors
addNext :: [Edge] -> Path -> [Path]
addNext fullEdgeList path = map (append path) $ getNeighbors fullEdgeList $ getENode $ last path

routeFolder :: Int -> [Edge] -> [Path] -> [Path]
--routeFolder endNode edgeList  accumList
--    | trace ("RouteFolder iteration: " ++ show accumList) False = undefined
routeFolder endNode [] accumList = accumList
routeFolder endNode edgeList [] = []
routeFolder endNode edgeList accumList =
    [x | x <- accumList, getENode (last x) == endNode] ++
    routeFolder endNode edgeList ( foldl (++) [] $ map (addNext edgeList) [ x | x <- accumList, getENode (last x) /= endNode] )
--                    takes accumList [Path], and produces [[Path]] which contains all possible paths

getAllRoutes :: [Edge] -> Int -> Int -> [Path]
getAllRoutes edgeList startNode endNode = routeFolder endNode edgeList $ map (\x -> [x]) $ (getNeighbors edgeList startNode)

-- Read in a file, and produce a list of edges
processDatFile :: String -> [Edge]
processDatFile datfile = map processLine $ map words $ lines $ datfile

-- Takes a worded-line, and creates an edge-node for it
processLine :: [String] -> Edge
processLine (a:b:w:[]) = ((read a), (read b), (read w))

randomizeEdgeWeights :: [Edge] -> StdGen -> (StdGen, [Edge])
randomizeEdgeWeights edgeList gen = mapAccumL edgeRandomize gen edgeList

edgeRandomize :: StdGen -> Edge -> (StdGen, Edge)
edgeRandomize gen edge = genEdgeTransform edge $ randomR (0.0, getWNode edge) gen

-- Want:
-- (StdGen, Edge)
-- (StdGen, Edge(SNode ENode randomWeight) )
-- randomR (0.0, WNode) -> (randomWeight, gen)
genEdgeTransform :: Edge -> (Double, StdGen) -> (StdGen, Edge)
genEdgeTransform edge (edgeW, gen) = (gen, ((getSNode edge), (getENode edge), edgeW) )

randomizePaths :: [Edge] -> [Path] -> StdGen -> (StdGen, [Path])
randomizePaths origEdgeList plist gen = (fst $ randomizeEdgeWeights origEdgeList gen,
    map (getRPath $ snd (randomizeEdgeWeights origEdgeList gen)) plist)

getRPath :: [Edge] -> Path -> Path
getRPath rEdgeList oPath = map (getEdge rEdgeList) oPath

-- Lookup an edge in an edgeList.
getEdge :: [Edge] -> Edge -> Edge
getEdge reList tgt = head $ filter (\x -> (getSNode x) == (getSNode tgt) && (getENode x) == (getENode tgt)) reList

sumEdgeWeights :: Path -> Double
sumEdgeWeights p = foldl (\accum x -> accum + getWNode x) 0 p

midExpander :: (StdGen, [Path]) -> (StdGen, [(Double, Path)])
midExpander x = (fst x, map (\a -> (sumEdgeWeights a, a)) $ snd x)

getCriticalPath :: [(Double, Path)] -> (Double, Path)
getCriticalPath [] = (0, [])
getCriticalPath (x:xs) = if ((fst x) > (fst (getCriticalPath xs))) then x else getCriticalPath xs
--getCriticalPath x = x

criticalMapper :: [Path] -> (Double, Path)
criticalMapper plist = getCriticalPath $ map (\a -> (sumEdgeWeights a, a)) plist

-- Runs a monteCarloExperiment runs times, and generates a list of the critical paths to be used for analysis
monteCarloExperiment :: [Edge] -> Int -> Int -> [(Double, Path)]
monteCarloExperiment edgeList seed runs = map criticalMapper $ take runs $ unfoldr (\gen -> justRTransform $ randomizePaths edgeList (getAllRoutes edgeList 1 (getEndNode edgeList)) gen) $ mkStdGen seed

-- Turns a list of critical paths into the win-percentages
-- the double in the input holds the length of the critical path
winStatistics :: [Path] -> Int -> [(Double, Path)] -> [(Double, Path)]
-- This one may not be memory-omptimal, but it does NOT stack overflow
winStatistics pathList runs crits = map (\x -> (((freq (map snd crits) x) / fromIntegral runs), x)) $ pathList
--
--This implementation causes stack/hunk overflow
--winStatistics pathList runs crits = foldl statAccum (zip (repeat 0.0) pathList) crits
--(\accum x -> (((freq (map snd crits) x) / fromIntegral runs), x)) $ pathList

makePercentages :: [(Double, Path)] -> [(Double, Path)]
makePercentages stats = map (makePct $ sum $ map fst stats) stats

makePct :: Double -> (Double, Path) -> (Double, Path)
makePct total (count, path) = (count/total, path)

-- Given a list/mapping of path to number of encounters, place the given path into the right 'state'
statAccum :: [(Double, Path)] -> (Double, Path) -> [(Double, Path)]
statAccum accum critpath = [ if pathsMatch (snd x) (snd critpath) then (fst x + 1.0, snd x) else x | x <- accum]
-- the same list of paths, but with the path found having 1 incremented to its double

freq :: [Path] -> Path -> Double
freq list x = fromIntegral $ length [a | a <- list, pathsMatch a x]

pathsMatch :: Path -> Path -> Bool
pathsMatch (x:[]) (y:[])        = edgesMatch x y
pathsMatch (x:xs:xss) (y:[])    = False
pathsMatch (x:[]) (y:ys:yss)    = False
pathsMatch (x:xs) (y:ys)        = (edgesMatch x y) && pathsMatch xs ys

edgesMatch :: Edge -> Edge -> Bool
edgesMatch x y = (getSNode x) == (getSNode y) && (getENode x) == (getENode y)
-------------------------------------------------------------------------------
------------------------------------- IO --------------------------------------
---------------------------------- & Monads -----------------------------------
-------------------------------------------------------------------------------

justRTransform :: (StdGen, [Path]) -> Maybe ([Path], StdGen)
justRTransform (gen, rPath) = Just (rPath, gen)

--showStartingEdges :: [Edge] -> IO
showStartingEdges edgeList = do
    putStrLn "The starting edges:"
    mapM_ putStrLn $ map show $ map (\x -> [x]) $ getNeighbors edgeList 1

showOneStep edgeList = do
    putStrLn "The next-edge added:"
    mapM_ putStrLn $ map show $ foldl (++) [] $ map (addNext edgeList) $ map (\x -> [x]) $ getNeighbors edgeList 1

showRouteFolding edgeList = do
    putStrLn "Finding all paths:"
    mapM_ putStrLn $ map show $ getAllRoutes edgeList 1 5

routeFoldingRandom edgeList gen = do
    mapM_ showPath $ snd $ midExpander $ randomizePaths edgeList (getAllRoutes edgeList 1 5) gen

showMCE :: [Edge] -> Int -> Int -> IO()
showMCE edgeList seed runs = do
    --mapM_ showPath $ snd $ monteCarloExperiment edgeList seed runs
    mapM_ putStrLn $ map show $ monteCarloExperiment edgeList seed runs

getEndNode :: [Edge] -> Int
getEndNode edgeList = maximum $ map getENode edgeList

showWinStatistics edgeList seed runs = mapM_ (showMsgPath "OUTPUT:\t") $ makePercentages $ winStatistics (getAllRoutes edgeList 1 (getEndNode edgeList)) runs $ monteCarloExperiment edgeList seed runs

-- Print a message before showing the path
showMsgPath :: String -> (Double, Path) -> IO()
showMsgPath msg p = do
    putStr msg
    showPath p

-- Show the path as:
-- :a12,a23...:    Distance|WinPct
showPath :: (Double, Path) -> IO()
showPath path = do
    putStr ":"
    putStr $ intercalate "," $ map edgeStr $ snd path
    putStr ":\t"
    printf "%.3f\n" (fst path)
    --putStrLn $ show $ fst path

edgeStr :: Edge -> String
edgeStr edge = "a" ++ show (getSNode edge) ++ show (getENode edge)

main = do
    args <- getArgs
    if args !! 0 == "F"
        then do
        datfile <- readFile $ args !! 3
        let
            edgeList = processDatFile datfile
            seed = read (args !! 1)
            runs = read (args !! 2)
        showWinStatistics edgeList seed runs
    else if args !! 0 == "B"
        then do
        let
            datfile = unlines ["1 2 3", "1 3 6", "1 4 13", "2 5 3", "2 3 9", "3 4 9", "3 6 7", "4 6 6", "5 6 3"]
            edgeList = processDatFile datfile
            seed = read (args !! 1)
            runs = read (args !! 2)
        showWinStatistics edgeList seed runs
    else putStrLn "Sorry, that simulation is not supported!"
    --showStartingEdges edgeList
    --showOneStep edgeList
    --showRouteFolding edgeList
    --routeFoldingRandom edgeList rGen
    --showMCE edgeList seed runs
