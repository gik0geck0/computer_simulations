
import Debug.Trace
import System.Environment
import System.Random
import Text.Printf

---- Estimate N-hat for a given sample  ----
maxLikely :: [Int] -> Int
maxLikely sample = maximum sample

meanMatching :: [Int] -> Int
meanMatching sample = ((sum sample) * 2 `div` (length sample)) - 1

gaps :: [Int] -> Int
gaps sample = ( ((maximum sample) - (length sample)) `div` (length sample)) + 1 + (maximum sample)

-- Minimum Variance Unbiased Estimator
mvue :: [Int] -> Int
mvue sample = let y = maximum sample
                  n = length sample
              in (y^(n+1) - (y-1)^(n+1)) `div` (y^n - (y-1)^n)

-- Statistic of running avg, running std-dev, and count
type Stat = (Double, Double)

collectStatistics :: (Double, Stat, Stat, Stat, Stat) -> (Int, Int, Int, Int) -> (Double, Stat, Stat, Stat, Stat)
collectStatistics (count, (mlsa, mlsv), (mmsa, mmsv), (gapsa, gapsv), (mvuea, mvuev)) (ml, mm, gap, mvue) = (count+1,
    (welfordAvg mlsa (count+1) $ fromIntegral ml, welfordVar mlsv mlsa (count+1) $ fromIntegral ml),
    (welfordAvg mmsa (count+1) $ fromIntegral mm, welfordVar mmsv mmsa (count+1) $ fromIntegral mm),
    (welfordAvg gapsa (count+1) $ fromIntegral gap, welfordVar gapsv gapsa (count+1) $ fromIntegral gap),
    (welfordAvg mvuea (count+1) $ fromIntegral mvue, welfordVar mvuev mvuea (count+1) $ fromIntegral mvue)
    )

welfordAvg :: Double -> Double -> Double -> Double
--welfordAvg lastAvg newCount nextVal
--    | trace ("Last Welford Avg: " ++ show lastAvg ) False = undefined
welfordAvg lastAvg newCount nextVal = lastAvg + (nextVal - lastAvg) / newCount

welfordVar :: Double -> Double -> Double -> Double -> Double
--welfordVar lastVar lastAvg newCount nextVal
--    | trace ("Last Welford Variance: " ++ show lastVar ) False = undefined
welfordVar _ _ 0 nextVal = 0
welfordVar _ _ 1 nextVal = 0
welfordVar lastVar lastAvg newCount nextVal = lastVar + ((newCount-1)/newCount) * ((nextVal - lastAvg)**2)
-- This one was from wikipedia, and basically just expnentiated to infinity
--welfordVar lastVar lastAvg newCount nextVal = ((newCount-2)/(newCount-1))*(lastVar**2) + (((nextVal - lastAvg)**2) / newCount)

lastFuckingWelfordVarianceStep :: (Double, Stat, Stat, Stat, Stat) -> (Double, Stat, Stat, Stat, Stat)
lastFuckingWelfordVarianceStep (count, (mlsa, mlsv), (mmsa, mmsv), (gapsa, gapsv), (mvuea, mvuev)) = (count, (mlsa, mlsv/count), (mmsa, mmsv/count), (gapsa, gapsv/count), (mvuea, mvuev/count))

makeStdDev :: (Double, Stat, Stat, Stat, Stat) -> (Double, Stat, Stat, Stat, Stat)
makeStdDev (count, (mlsa, mlsv), (mmsa, mmsv), (gapsa, gapsv), (mvuea, mvuev)) = (count, (mlsa, sqrt mlsv), (mmsa, sqrt mmsv), (gapsa, sqrt gapsv), (mvuea, sqrt mvuev))

-- takes one sample (4 numbers from N), and makes guesses (estimations) for each function
-- -> (ml, mm, gap, mvue)
oneSampleRun :: [Int] -> (Int, Int, Int, Int)
oneSampleRun sample = (
        maxLikely sample,
        meanMatching sample,
        gaps sample,
        mvue sample
    )

makeRuns :: Int -> Int -> Int -> [(Int, Int, Int, Int)]
makeRuns n runs seed = seqMapAccum oneSampleRun 4 runs $ randomRs (1, n) $ mkStdGen seed

-- Take an infinte list (iList), take n number of objects from it, run a function across them, and add them to a list.
-- there is a specified number of iterations to be done to prevent infinte recursion, but technically, this COULD go on forever. I just put a stop to it
seqMapAccum :: ([Int] -> b) -> Int -> Int -> [Int] -> [b]
--seqMapAccum _ n i iList
--    | trace ("MappingIteration: " ++ show i ++ " with sample = " ++ show (take n iList)) False = undefined
seqMapAccum _ _ 0 _ = []
seqMapAccum f n i iList = (f (take n iList)):(seqMapAccum f n (i-1) $ drop n iList)

showMaxLikelihood sample        = putStrLn $ show $ maxLikely sample
showMeanMatching sample         = putStrLn $ show $ meanMatching sample
showGaps sample                 = putStrLn $ show $ gaps sample
showMinVarUnbiEstim sample      = putStrLn $ show $ mvue sample

showOutputStatistics :: (Double, Stat, Stat, Stat, Stat) -> IO()
showOutputStatistics (count, (mlsa, mlsv), (mmsa, mmsv), (gapsa, gapsv), (mvuea, mvuev)) = do
    printf "OUTPUT\t:ml:\t%.3f\t%.3f\n" mlsa mlsv
    printf "OUTPUT\t:mm:\t%.3f\t%.3f\n" mmsa mmsv
    printf "OUTPUT\t:gaps:\t%.3f\t%.3f\n" gapsa gapsv
    printf "OUTPUT\t:mvue:\t%.3f\t%.3f\n" mvuea mvuev

showGraphStats :: Double -> (Double, Stat, Stat, Stat, Stat) -> IO()
showGraphStats n (count, (mlsa, mlsv), (mmsa, mmsv), (gapsa, gapsv), (mvuea, mvuev)) = do
    printf "%.3f \t%.3f \t%.3f\t%.3f \t%.3f \t%.3f \t%.3f \t%.3f \t%.3f \t%.3f \t%.3f \t%.3f\n" n mlsa (1.96*mlsv) n mmsa (1.96*mmsv) n gapsa (1.96*gapsv) n mvuea (1.96*mvuev)

main :: IO()
main = do
    args <- getArgs
    if (args !! 0) == "T"
        then
        let n = read (args !! 1) :: Int
            seed = read (args !! 2) :: Int
            runs = read (args !! 3) :: Int
        in do
            --putStrLn $ show $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns n runs seed
            showOutputStatistics $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns n runs seed
    else if (args !! 0) == "G"
        then
        let n = read (args !! 1) :: Int
            seed = read (args !! 2) :: Int
            runs = read (args !! 3) :: Int
        in do
            showGraphStats 500 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 500 runs seed
            showGraphStats 1000 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 1000 runs seed
            showGraphStats 1500 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 1500 runs seed
            showGraphStats 2000 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 2000 runs seed
            showGraphStats 2500 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 2500 runs seed
            showGraphStats 3000 $ makeStdDev $ lastFuckingWelfordVarianceStep $ foldl collectStatistics (0,(0,0),(0,0),(0,0),(0,0)) $ makeRuns 3000 runs seed
    else
        do putStrLn "Sorry, but that simulation isn't supported!"
