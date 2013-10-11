
import Data.List (intercalate, unfoldr)
import Data.Traversable (mapAccumL)
import Debug.Trace
import System.Environment
import System.Random

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

-------------------------------------------------------------------------------
------------------------------------- IO --------------------------------------
-------------------------------------------------------------------------------

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

showMCE edgeList seed runs = do
    --mapM_ showPath $ snd $ monteCarloExperiment edgeList seed runs
    putStrLn $ show $ monteCarloExperiment edgeList seed 1

-- Returns the last StdGen, and a zip of each path with its critical-win percentage
monteCarloExperiment :: [Edge] -> Int -> Int -> [Path] -- [(Double, Path)]
monteCarloExperiment edgeList seed runs = take runs $ unfoldr (\gen -> justRTransform $ randomizePaths edgeList (getAllRoutes edgeList 1 5)) $ mkStdGen seed

justRTransform :: (StdGen, [Path]) -> Maybe ([Path], StdGen)
justRTransform gen rPath = Just (rPath, gen)

showPath path = do
    putStr ":"
    putStr $ intercalate "," $ map edgeStr $ snd path
    putStr ":\t"
    putStrLn $ show $ fst path

edgeStr :: Edge -> String
edgeStr edge = "a" ++ show (getSNode edge) ++ show (getENode edge)

main = do
    args <- getArgs
    datfile <- readFile $ args !! 0
    let edgeList = processDatFile datfile
        seed = read (args !! 1)
        runs = read (args !! 2)
    --showStartingEdges edgeList
    --showOneStep edgeList
    --showRouteFolding edgeList
    --routeFoldingRandom edgeList rGen
    monteCarloExperiment edgeList seed runs
    --putStrLn $ show $ take 20 $ infMonteCarlo
