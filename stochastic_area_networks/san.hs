
import System.Environment

--            A -> B with edgeWeight
type Edge = (Int, Int, Int)
type Path = [Edge]
getSNode :: Edge -> Int
getSNode (s, e, w) = s

getENode :: Edge -> Int
getENode (s, e, w) = e

getWNode :: Edge -> Int
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

--combinePaths :: [[Path]] -> [Path]
--combinePaths x = 

-- routeFolder :: Int -> [Path] -> [Edge] -> [Path]
-- routeFolder endNode accumList [] = accumList
-- routeFolder endNode accumList edgeList =
--     [x | x <- accumList, getENode (last x) == endNode] ++
--     map (addNext edgeList) accumList
--
-- Pick the paths that have not been 'completed'
-- Evaluate the next edge/s for those pats
-- modify the 'entry' in the accumlist to now contain the next path
-- recurse until all entries in the accum list have an ending point equal to the end-node
--

--getAllRoutes :: [Edge] -> Int -> Int -> [[Edge]]
--getAllRoutes edges endNode startNode = 

-- Read in a file, and produce a list of edges
processDatFile :: String -> [Edge]
processDatFile datfile = map processLine $ map words $ lines $ datfile

-- Takes a worded-line, and creates an edge-node for it
processLine :: [String] -> Edge
processLine (a:b:w:[]) = ((read a), (read b), (read w))

main = do
    args <- getArgs
    datfile <- readFile $ args !! 0
    let edgeList = processDatFile datfile
    mapM_ putStrLn $ map show $ map (addNext edgeList) [[head edgeList]]
