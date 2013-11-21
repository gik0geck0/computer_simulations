
import Data.Maybe
import Data.Sequence (unfoldl)
import Debug.Trace
-- import Control.Monad.State
import System.Environment
import System.Random.Lehmer.Base
import Text.Printf

data EventType = Red | Green | Yellow
    | LSpawnCar | LCarStop | LCarLeave
    | RSpawnCar | RCarStop | RCarLeave
    | SpawnPed | PedAtButton | PedPushButton | PedWalkEnd
    | CheckPool -- Meant to serve the one-minute timeout purpose
    deriving (Enum, Eq, Ord, Show)

data Event = Event {
    eventType :: EventType,
    time :: Double,         -- Time in s
    speed :: Maybe Double  -- Speed in ft/s for CarEvents and PedestrianEvents
} deriving (Show)

instance Eq Event where
    (Event _ t1 _) == (Event _ t2 _) = t1 == t2
instance Ord Event where
    (Event _ t1 _) `compare` (Event _ t2 _) = t1 `compare` t2

--              (BeginTime, Speed)
data PoolItem = PoolItem {
    poolEventType :: EventType,
    poolTime :: Double,
    poolSpeed :: Double
} deriving (Show)

type Pool = [PoolItem]
--              PastEvents, Current State, FutureEvents
--              PastEvents will only contain important past events; listed below are these important events
--                                                      (PersonID, Speed)
--                                                      This is the pool of folk waiting at the button
--                                                      TODO: Why do I need the ID? Will be set to 0 until I figure that out
--
--                 (past,    current,     future,  rgen,      pedpool,carpool, stats)
type SystemState = ([Event], Maybe Event, [Event], LehmerState, Pool, Pool, StatState)
type JustSystemState = ([Event], Event, [Event], LehmerState, Pool, Pool, StatState)

getPast :: SystemState -> [Event]
getPast (past,_,_,_,_,_,_) = past

getCurrent :: SystemState -> Maybe Event
getCurrent (_,current,_,_,_,_,_) = current

getFuture :: SystemState -> [Event]
getFuture (_,_,future,_,_,_,_) = future

getStats :: SystemState -> StatState
getStats (_,_,_,_,_,_,stats) = stats

--------
-- Stat Storing
--------

-- I need...
-- The number of pedestrians in the simulation
-- The number of autos in the simulation
-- The total duration of the simulation (how many minutes, greater than TIME, of simulated time there were)
-- The minimum, average, (sample) standard deviation, and maximum wait time for pedestrians.
-- The minimum, average, (sample) standard deviation, and maximum wait time for automobiles.
--
-- Additionally, your simulation will create a text file named acwait.dat.
--  The acwait.dat file will contain two columns of numerical data. The
--  first column will be for pedestrian wait time autocorrelation, the second
--  column is for the wait time autocorrelation of the autos. The file should
--  be written such that the row implicitely represents the lag value. Row 1
--  should have the r1 autocorrelation for pedestrians and autos in columns 1 and
--  two. Row 2 should have the r2 correlations, row 3 the r3 correlations, and so on and so forth.
--  Autocorrelations up to r20 should be reported in acwait.dat.

-- Simple includes (pt, min, max, avg, stdev, count)
type SimpleStat = (Double, Double, Double, Double, Double, Int)
-- Pop has number in simulation (ped, car)
type PopStat = (Int, Int)
--              Auto Correlation involves a (running autoCoVariance, lag size)  -- These NEED to be updated each time the lag-history changes
type AutoCoVarStat = Double -- One per lag
type LagHistory = [Double] -- Front of the list represents the most recent point (lag=0)

--               (CurrentStats for Lags, Current CoVar)
type HistoryModule = ([SimpleStat], [AutoCoVarStat])

--               (Ped Avg   , Car Avg   , Pop    , PedAutoCoVar , CarAutoCoVar )
type StatState = (SimpleStat, SimpleStat, PopStat, HistoryModule, HistoryModule)

initStatState :: StatState
initStatState = (
    (0, 999999, 0, 0, 0, 0),
    (0, 999999, 0, 0, 0, 0),
    (0, 0),
    ([(0, 999999, 0, 0, 0, 0)], [0]),
    ([(0, 999999, 0, 0, 0, 0)], [0]) )

--------
--Stat Functions
--------

addSystemPed :: StatState -> StatState
addSystemPed (a,b, (npeds, ncars), c, d) = (a,b, (npeds+1, ncars), c, d)

removeSystemPed :: StatState -> StatState
removeSystemPed (a,b, (npeds, ncars), c, d) = (a,b, (npeds-1, ncars), c, d)

addSystemCar :: StatState -> StatState
addSystemCar (a,b, (npeds, ncars), c, d) = (a,b, (npeds, ncars+1), c, d)

removeSystemCar :: StatState -> StatState
removeSystemCar (a,b, (npeds, ncars), c, d) = (a,b, (npeds, ncars-1), c, d)

addSimple :: Double -> SimpleStat -> SimpleStat
addSimple pt (oldpt, min, max, avg, var, n) =
    let newcount = n+1
        newavg = welfordAvg avg (n+1) pt
        newstdev = welfordVar var avg newcount pt
    in
    (
        pt,
        if pt < min then pt else min,
        if pt > max then pt else max,
        newavg,
        newstdev,
        newcount
    )

addStatSystemPedPoint :: Double -> StatState -> StatState
addStatSystemPedPoint pt (simpleped, simplecar, pop, pedhist, carhist) =
    let newsimpleped = addSimple pt simpleped
    in (
        newsimpleped,
        simplecar,
        pop,
        addToHist newsimpleped pedhist,
        carhist
    )

addStatSystemCarPoint :: Double -> StatState -> StatState
addStatSystemCarPoint pt (simpleped, simplecar, pop, pedhist, carhist) =
    let newsimplecar = addSimple pt simplecar
    in (
        simpleped,
        newsimplecar,
        pop,
        pedhist,
        addToHist newsimplecar carhist
    )

addToHist :: SimpleStat -> HistoryModule -> HistoryModule
addToHist newstat (stats, coVars) =
    let newCoVars = map (calculateCoVar newstat) $ zip stats coVars
        in if length stats < 20 then (newstat:stats, 0:newCoVars)
        else (newstat:(init stats),
            newCoVars
        )

calculateCoVar :: SimpleStat -> (SimpleStat, AutoCoVarStat) -> AutoCoVarStat
calculateCoVar (ptA,_,_,avgA,_,_) ((ptB,_,_,avgB,_,newcount), oldCoVar) = oldCoVar + (fromIntegral newcount-1/fromIntegral newcount) * (ptA-avgA) * (ptB-avgB)

--------
-- Welford Equations
--------
welfordAvg :: Double -> Int -> Double -> Double
welfordAvg lastAvg newCount nextVal = lastAvg + (nextVal - lastAvg) / fromIntegral newCount

welfordVar :: Double -> Double -> Int -> Double -> Double
welfordVar _ _ 0 nextVal = 0
welfordVar _ _ 1 nextVal = 0
welfordVar lastVar lastAvg newCount nextVal = lastVar + ((fromIntegral newCount-1)/fromIntegral newCount) * ((nextVal - lastAvg)**2)

--------
-- Maybe Hacks and Other Small things
--------

justStateToMaybe :: JustSystemState -> SystemState
justStateToMaybe (past, current, future, rgen, pedpool, carpool, stats) = (past, Just current, future, rgen, pedpool, carpool, stats)

maybeStateToJust :: SystemState -> JustSystemState
maybeStateToJust (past, current, future, rgen, pedpool, carpool, stats) = (past, (fromMaybe (Event CheckPool 0 Nothing) current), future, rgen, pedpool, carpool, stats)
-- the current event must be a maybe. WHEN current=Nothing, the simulation must be over. There are no more events

--------
-- Tuple Helpers
--------

-- Create a triplet by adding an element to the beginning
tupleInsert :: a -> (b,c) -> (a,b,c)
tupleInsert a (b,c) = (a,b,c)

-- Create a triplet by adding an element to the end
tupleAppend :: c -> (a,b) -> (a,b,c)
tupleAppend c (a,b) = (a,b,c)

-- Synthesize two pairs into a quad
dupleCombine :: (a,b) -> (c,d) -> (a,b,c,d)
dupleCombine (a,b) (c,d) = (a,b,c,d)

addPairToPair :: (c,d) -> (a,b) -> (a,b,c,d)
addPairToPair (c,d) (a,b) = (a,b,c,d)

pairAddTriplet :: (a,b) -> (c,d,e) -> (a,b,c,d,e)
pairAddTriplet (a,b) (c,d,e) = (a,b,c,d,e)

addTripletToPair :: (c,d,e) -> (a,b) -> (a,b,c,d,e)
addTripletToPair (c,d,e) (a,b) = (a,b,c,d,e)

addTripletToTriplet :: (d,e,f) -> (a,b,c) -> (a,b,c,d,e,f)
addTripletToTriplet (d,e,f) (a,b,c) = (a,b,c,d,e,f)

pairAddQuad :: (a,b) -> (c,d,e,f) -> (a,b,c,d,e,f)
pairAddQuad (a,b) (c,d,e,f) = (a,b,c,d,e,f)

pairAddQuintuple :: (a,b) -> (c,d,e,f,g) -> (a,b,c,d,e,f,g)
pairAddQuintuple (a,b) (c,d,e,f,g) = (a,b,c,d,e,f,g)

-- Create a quintuple from a quad by adding an item to the end
addToQuad :: e -> (a,b,c,d) -> (a,b,c,d,e)
addToQuad e (a,b,c,d) = (a,b,c,d,e)

addPairToQuad :: (e,f) -> (a,b,c,d) -> (a,b,c,d,e,f)
addPairToQuad (e,f) (a,b,c,d) = (a,b,c,d,e,f)

addTripletToQuad :: (e,f,g) -> (a,b,c,d) -> (a,b,c,d,e,f,g)
addTripletToQuad (e,f,g) (a,b,c,d) = (a,b,c,d,e,f,g)

startQuadWithPair :: (a,b) -> (c,d,e,f) -> (a,b,c,d,e,f)
startQuadWithPair (a,b) (c,d,e,f) = (a,b,c,d,e,f)

startQuintupleWithPair :: (a,b) -> (c,d,e,f,g) -> (a,b,c,d,e,f,g)
startQuintupleWithPair (a,b) (c,d,e,f,g) = (a,b,c,d,e,f,g)

--------
-- List Helpers
--------

maybeCons :: Maybe a -> [a] -> [a]
maybeCons a alist = if isNothing a then alist else (fromMaybe (head alist) a):alist

-- Insert the item into the list in it's sorted order
-- Note that it will be the LAST item in a sequence of equals
sortedInsertion :: (Ord a) => [a] -> a -> [a]
sortedInsertion list item = [a | a <- list, a <= item] ++ [item] ++ [b | b <- list, b > item]

fullInsertionSort :: (Ord a) => [a] -> [a]
fullInsertionSort [] = []
fullInsertionSort l = sortedInsertionAccum [] (head l) (tail l)

sortedInsertionAccum :: (Ord a) => [a] -> a -> [a] -> [a]
sortedInsertionAccum accum elem [] = sortedInsertion accum elem
sortedInsertionAccum accum elem left = sortedInsertionAccum (sortedInsertion accum elem) (head left) (tail left)

-- Place the event into the past-event list only if it will be needed later
-- TODO: Right now, ALL Events are being held in the history
smartInsert :: [Event] -> Event -> [Event]
-- smartInsert pEvents cEvent
--     | trace ("Smart-inserting " ++ show cEvent) False = undefined
smartInsert pEvents cEvent = cEvent:pEvents

-- Take the past events, current event and the list of future events, and produce the updated past events, next event, and the list of future events
nextState :: Double -> JustSystemState -> SystemState
nextState endTime = (stripOutOfTime endTime) . rotateEvents . processEvent

stripOutOfTime :: Double -> SystemState -> SystemState
stripOutOfTime endTime (past, current, [], rgen, pedpool, carpool, stats) = (past, current, [], rgen, pedpool, carpool, stats)
stripOutOfTime endTime (past, current, future, rgen, pedpool, carpool, stats) = (past, current, filter (\e-> not $ time e > endTime && eventType e `elem` [LSpawnCar, RSpawnCar, SpawnPed]) future, rgen, pedpool, carpool, stats)

getNextEvent :: [Event] -> (Event, [Event])
getNextEvent elist = (head elist, tail elist)

rotateEvents :: JustSystemState -> SystemState
-- rotateEvents (past, current, future, rgen, pedpool)
--    | trace ("Moving this event into the past: " ++ show current) False = undefined
--    | trace ("Future is: " ++ (show future)) False = undefined
--    | trace ("Advancing simulation time to " ++ (show $ time current)) False = undefined
rotateEvents (past, current, [], rgen, pedpool, carpool, stats) = (smartInsert past current, Nothing, [], rgen, pedpool, carpool, stats)
rotateEvents (past, current, future, rgen, pedpool, carpool, stats) = (smartInsert past current, Just (head future), tail future, rgen, pedpool, carpool, stats)

applyRandom :: [Event] -> EventType -> (Maybe Double, Double, LehmerState) -> ([Event], LehmerState)
-- applyRandom future etype (speed, timeval, rgen)
--    | trace ("Applying the random number " ++ show timeval) False = undefined
applyRandom future etype (speed, timeval, rgen) = (sortedInsertion future (Event etype timeval speed), rgen)

addCheckPool :: JustSystemState -> JustSystemState
addCheckPool (past, current, future, rgen, pedpool, carpool, stats) = (past, current, sortedInsertion future (Event CheckPool (time current + 60) Nothing), rgen, pedpool, carpool, stats)

-- Find the first occurance of a Green Light
getFirstEvent :: EventType -> [Event] -> Event
getFirstEvent etype elist = emptyHead (head elist) $ filter (\x -> eventType x == etype) elist

-- Version of head that won't error on empty
emptyHead :: a -> [a] -> a
emptyHead dflt [] = dflt
emptyHead _ a = head a
-- TODO: elist == []?

--------
-- Speed-transforms. Takes a randomly generated speed, and translates it into TIME. (intrinsicly event/state dependant, represented by the function name)
--------
pedSpeedTransformComing :: Double -> (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
-- pedSpeedTransformComing (speed, rgen)
--    | trace ("pedSpeedTransformComing: transforming the value " ++ show speed) False = undefined
pedSpeedTransformComing now (speed, rgen) = (Just speed, now + 1155.0 / speed, rgen)

carSpeedTransformComing :: Double -> (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
-- carSpeedTransformComing (speed, rgen)
--    | trace ("carSpeedTransformComing: transforming the value " ++ show speed) False = undefined
carSpeedTransformComing now (speed, rgen) = (Just speed, now + 1143.0 / (speed * 528.0 / 360.0), rgen)

-- This KINDA does the speedTransform. It takes a speed, and creates an Event for when the car will leave the simulation
carSpeedTransformGoing :: Double -> EventType -> (Maybe Double) -> Event
-- carSpeedTransformGoing etype speed
--    | trace ("carSpeedTransformGoing: transforming the value " ++ show speed) False = undefined
carSpeedTransformGoing now etype speed = Event etype (now + 1191.0 / ((fromMaybe 0 speed) * 528.0 / 360.0)) speed
-- TODO: Assumption that speed will be Just

addTimeToEvent :: Double -> Event -> Event
addTimeToEvent additional event = Event (eventType event) (additional + time event) (speed event)

addTimeToFst :: Double -> (Double, a) -> (Double, a)
addTimeToFst additional (fst, a) = (additional + fst, a)

carSpawnTransform :: (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
carSpawnTransform (timeval, rgen) = (Nothing, timeval, rgen)

--------
-- Calls to random
--------
randomPedSpeed :: LehmerState -> (Double, LehmerState)
randomPedSpeed = uniformRange 6 13 . stream 0

randomLCarSpeed :: LehmerState -> (Double, LehmerState)
randomLCarSpeed = uniformRange 25 35 . stream 1

randomRCarSpeed :: LehmerState -> (Double, LehmerState)
randomRCarSpeed = uniformRange 25 35 . stream 2

randomPedSpawn :: LehmerState -> (Double, LehmerState)
randomPedSpawn = makeExponential (60/4) . uniform . stream 3

randomLCarSpawn :: LehmerState -> (Double, LehmerState)
randomLCarSpawn = makeExponential (60/4) . uniform . stream 4

randomRCarSpawn :: LehmerState -> (Double, LehmerState)
randomRCarSpawn = makeExponential (60/4) . uniform . stream 5

randomButtonPressNow :: Double -> (Event, [Event], LehmerState) -> ([Event], LehmerState)
randomButtonPressNow probability (current, future, rgen)
    = let (u, newgen) = uniform $ stream 6 rgen
        -- probability to push the button "now".
        -- TODO: This breaks one of the axioms of NextEventSimulations: NEVER schedule another event for the same time as the current time
    in if u < probability then (sortedInsertion future (Event PedPushButton (time current) (speed current)), newgen)
    else (future, newgen)

--------
-- Random Helpers
--------

-- Take a lambda and a call from uniform (previously performed), and turn it into an exponential distribution
makeExponential :: Double -> (Double, LehmerState) -> (Double, LehmerState)
-- makeExponential lambda (u, s)
--    | trace ("Uniform=" ++ show u ++ "==> exponential=" ++ (show $ (-lambda * log (1-u)))) False = undefined
makeExponential lambda (u, s) = ( -lambda * log (1-u) , s)

-- Linker that adds a new pedestrian-spawn to the event list.
-- Note: This does not actually spawn a pedestrian. Merely an event for WHEN a pedestrian will spawn
addPedSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addPedSpawn now (elist, rgen) = applyRandom elist SpawnPed $ tupleInsert Nothing $ addTimeToFst now $ randomPedSpawn rgen -- randomCarSpawn rgen

addLCarSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addLCarSpawn now (elist, rgen) = applyRandom elist LSpawnCar $ tupleInsert Nothing $ addTimeToFst now $ randomLCarSpawn rgen -- randomCarSpawn rgen

addRCarSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addRCarSpawn now (elist, rgen) = applyRandom elist RSpawnCar $ tupleInsert Nothing $ addTimeToFst now $ randomRCarSpawn rgen -- randomCarSpawn rgen

-- Push the button now if u[0,1) < probability
maybePushButton :: Double -> (Event, [Event], LehmerState, Pool, Pool, StatState) -> ([Event], LehmerState, Pool, Pool, StatState)
maybePushButton probability (current, future, rgen, pedpool, carpool, stats) = addTripletToPair (pedpool, carpool, stats) $ randomButtonPressNow probability (current, future, rgen)

-- Have everyone in the pedestrian pool walk across the crosswalk
dumpPool :: Double -> (Double -> StatState -> StatState) -> [PoolItem] -> StatState -> [Event] -> ([Event], StatState)
dumpPool now statAccessor pool stats elist = foldl (poolFold now statAccessor) (elist, stats) pool

-- NOTE: This opportunity is being used to add pedestrian wait times to the statSystem
-- Any pedestrian that does not enter this pool will need to manually enter their wait time
poolFold :: Double -> (Double -> StatState -> StatState) -> ([Event], StatState) -> PoolItem -> ([Event], StatState)
poolFold now statAccessor accum poolitem = (sortedInsertion (fst accum) (Event (poolEventType poolitem) (now + 48.0/(poolSpeed poolitem)) (Just (poolSpeed poolitem))), statAccessor (now - poolTime poolitem) (snd accum))

--------
-- Event Processing and Simulation Components
--------

-- Takes the current event, and returns the future-list of Events (where the head is the next event)
processEvent :: JustSystemState -> JustSystemState
processEvent (past, current, future, rgen, pedpool, carpool, stats) =
    case eventType current of
                    -- TODO: When Red, I should probably be emptying the pool
                    -- TODO: I probably want to strip out any CheckPool events from the future, since a person in the past may now be moving on
        Red     -> let (newelist, newstats) = dumpPool (time current) (addStatSystemPedPoint) pedpool stats $ sortedInsertion future (Event Green (time current + 12) Nothing)
                   in (past, current, filter (\x-> eventType x /= CheckPool) newelist, rgen, [], carpool, newstats)
        Yellow  -> (past, current, sortedInsertion future (Event Red (time current + 8) Nothing), rgen, pedpool, carpool, stats)
        Green   -> let (newelist, newstats) = dumpPool (time current) (addStatSystemCarPoint) carpool stats future
                   in (past, current, newelist, rgen, pedpool, [], stats) -- "Let the cars go". Empty the capool, dumping leave-events into the eventQueue
        PedAtButton ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Yellow  -> (past, current, sortedInsertion future (Event PedWalkEnd (time $ head $ filter (\x -> eventType x == Red) future) (speed current)), rgen, pedpool, carpool, addStatSystemPedPoint ((time $ head $ filter (\x -> eventType x == Red) future)-time current) stats)
                            -- If The time of the next Green - now >= time to cross (If there's time to cross)
                    Red     -> if (time $ head $ filter (\x -> eventType x == Green) future) - time current >= (48.0 / fromMaybe 0 (speed current))                                     -- NOTE: This add-0 is important. This pedestrian has waited 0 seconds
                                    then (past, current, sortedInsertion future (Event PedWalkEnd ((time current) + 48.0 / fromMaybe 0 (speed current)) (speed current)), rgen, pedpool, carpool, addStatSystemPedPoint 0 stats)
                                    -- There was not enough time to cross. Wait around in the pool
                                    else addCheckPool (past, current, future, rgen, (PoolItem PedWalkEnd (time current) (fromMaybe 0 (speed current))):pedpool, carpool, stats)
                            -- Is he/she alone?
                    Green   ->  if length pedpool == 0
                                    -- There's a 2/3 chance that they'll push the button immediately
                                    then pairAddQuintuple (past, current) $ maybePushButton (2/3) (current, future, rgen, (PoolItem PedWalkEnd (time current) (fromMaybe 0 (speed current))):pedpool, carpool, stats)
                                else
                                    -- If others are around, probability to push button immediately is 1/n
                                    -- TODO: Does 1/n INCLUDE the new pedestrian? Or not. Currently, it assumes not. do 1/ n+1 if it does
                                    startQuintupleWithPair (past, current) $ maybePushButton (1.0/ fromIntegral (length pedpool)) (current, future, rgen, (PoolItem PedWalkEnd (time current) (fromMaybe 0 (speed current))):pedpool, carpool, stats)
                    _       -> (past, current, future, rgen, pedpool, carpool, stats) -- TODO: Should I add a trace here, to indicate that this is going on?
        -- TODO: Check time current, and prevent any spawning if it's greater than the proposed simulation end time
        SpawnPed -> addTripletToQuad (pedpool, carpool, addSystemPed stats) $ dupleCombine (past, current) $ addPedSpawn (time current) $ applyRandom future PedAtButton $ pedSpeedTransformComing (time current) $ randomPedSpeed rgen
        LSpawnCar -> addTripletToQuad (pedpool, carpool, addSystemCar stats) $ dupleCombine (past, current) $ addLCarSpawn (time current) $ applyRandom future LCarStop $ carSpeedTransformComing (time current) $ randomLCarSpeed rgen
        RSpawnCar -> addTripletToQuad (pedpool, carpool, addSystemCar stats) $ dupleCombine (past, current) $ addRCarSpawn (time current) $ applyRandom future RCarStop $ carSpeedTransformComing (time current) $ randomRCarSpeed rgen
        LCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint 0 stats) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint 0 stats)
                               else (past, current, future, rgen, pedpool, (PoolItem LCarLeave (time current) (fromMaybe 0 $ speed current)):carpool, stats)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                                   -- TODO: Make a car pool
                    Red     -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint ((time $ getFirstEvent Green future) - (time current)) stats)
                                -- Leave when the green comes up. We know when that is, so schedule a CarLeave
                    _       -> (past, current, future, rgen, pedpool, carpool, stats) -- Do.. nothing... I guess... TODO: trace?
        RCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint 0 stats) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint 0 stats)
                               else (past, current, future, rgen, pedpool, (PoolItem RCarLeave (time current) (fromMaybe 0 $ speed current)):carpool, stats)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                                   -- TODO: Make a car pool
                    Red     -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, carpool, addStatSystemCarPoint ((time $ getFirstEvent Green future) - (time current)) stats)
                    _       -> (past, current, future, rgen, pedpool, carpool, stats) -- Do.. nothing... I guess... TODO: trace?
        PedPushButton -> (past, current, sortedInsertion future (Event Yellow (max (time current + 1) (time $ getFirstEvent Green past)) Nothing), rgen, pedpool, carpool, stats)
        CheckPool -> if length pedpool > 0 then (past, current, sortedInsertion future (Event PedPushButton (time current) Nothing), rgen, pedpool, carpool, stats) else (past, current, future, rgen, pedpool, carpool, stats)
        PedWalkEnd -> (past, current, future, rgen, pedpool, carpool, stats) -- A Pedestrian is leaving. We will merely decrease the number of system-pedestrians
        LCarLeave -> (past, current, future, rgen, pedpool, carpool, stats)  -- A Car is leaving. We will merely decrease the number of system-cars
        RCarLeave -> (past, current, future, rgen, pedpool, carpool, stats)  -- A Car is leaving. We will merely decrease the number of system-cars
        -- Hopefully, this catch-all will never happen... hopefully...
        -- _ -> (past, current, future, rgen, pedpool)

--------
-- Iteration Functions and Helpers
--------
assert :: Bool -> a -> a
assert False x = error "Failed Assertion!"
assert _ x = x

startState :: Int -> JustSystemState
startState seed = ([], Event SpawnPed 0 Nothing, [Event LSpawnCar 0 Nothing, Event RSpawnCar 0 Nothing, Event Green 0 Nothing], lehmerInit seed, [], [], initStatState)

singleSpawnPed :: Int -> JustSystemState
singleSpawnPed seed = ([], Event SpawnPed 0 Nothing, [], lehmerInit seed, [], [], initStatState)

singleSpawnLCar :: Int -> JustSystemState
singleSpawnLCar seed = ([], Event LSpawnCar 0 Nothing, [], lehmerInit seed, [], [], initStatState)

singleSpawnRCar :: Int -> JustSystemState
singleSpawnRCar seed = ([], Event RSpawnCar 0 Nothing, [], lehmerInit seed, [], [], initStatState)

iterateUntilNoTomorrow :: Double -> SystemState -> SystemState
iterateUntilNoTomorrow endtime start@(past, current, future, rgen, pedpool, carpool, stats)
-- Only stop when there's no future, we have no current event to process, and there's nothing in the pools
    | (isNothing current) && length future == 0 && length pedpool == 0 && length carpool == 0 = (past, Nothing, future, rgen, pedpool, carpool, stats)
    | True                  = iterateUntilNoTomorrow endtime $ nextState endtime (maybeStateToJust start)

--------
-- Testing Functions
--------

-- check that there's an event type with a time less than that specified in the list. Return whether or not there is
checkEventContainment :: EventType -> Double -> [Event] -> Bool
checkEventContainment etype maxtime elist = (0 < (length $ filter (derFilter etype maxtime) elist))

derFilter :: EventType -> Double -> Event -> Bool
-- derFilter etype maxtime e
--    | trace ("Event " ++ show e ++" ?" ++ show ((time e <= maxtime) && ((eventType e) == etype))) False = undefined
derFilter etype maxtime e = (time e <= maxtime) && ((eventType e) == etype)

identityTrace :: Show a => a -> a
identityTrace a
    | trace("Trace: " ++ show a) True = a

-- Longest time it will take for a single pedestrian to reach the button is 192s
testSinglePed :: Int -> String
testSinglePed seed =
    let singlePed@(past, current, future, rgen, pedpool, carpool, stats) = nextState 193 $ singleSpawnPed seed
        in if length past /= 1 then "Did not process first event in testSinglePed"
            else if not (checkEventContainment PedAtButton 193 (maybeCons current future)) then "Expected a PedAtButton to be scheduled within 193 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSinglePed. The first pedestrian will arrive at the button at " ++ show (time (fromMaybe (head future) current))

-- Longest time it will take a car to reach the crosswalkstop is 31.17s
testSingleLCar :: Int -> String
testSingleLCar seed =
    let singleLCar@(past, current, future, rgen, pedpool, carpool, stats) = nextState 32 $ singleSpawnLCar seed
        in if length past /= 1 then "Did not process first event in testSingleRCar"
            else if not (checkEventContainment LCarStop 32.0 (maybeCons current future)) then "Expected an LCarStop to be scheduled within 32 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSingleLCar. The first car will arrive at the crosswalk at " ++ show (time (fromMaybe (head future) current))

-- Longest time it will take a car to reach the crosswalkstop is 31.17s
testSingleRCar :: Int -> String
testSingleRCar seed =
    let singleRCar@(past, current, future, rgen, pedpool, carpool, stats) = nextState 32 $ singleSpawnRCar seed
        in if length past /= 1 then "Did not process first event in testSingleRCar"
            else if not (checkEventContainment RCarStop 32.0 (maybeCons current future)) then "Expected an RCarStop to be scheduled within 32 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSingleRCar. The first car will arrive at the crosswalk at " ++ show (time (fromMaybe (head future) current))

validateDecreasing :: (Ord a) => [a] -> Bool
validateDecreasing [] = True
validateDecreasing (a:b:[]) = a >= b
validateDecreasing (a:b:rest) = (a >= b) && validateDecreasing (b:rest)

validateIncreasing :: (Ord a) => [a] -> Bool
validateIncreasing [] = True
validateIncreasing (a:b:[]) = a <= b
validateIncreasing (a:b:rest) = (a <= b) && validateIncreasing (b:rest)

-- Make sure the number of pedestrians and cars spawned equals the number that left
-- validateEnterLeave

testInsertionSort :: String
testInsertionSort = let sorted = fullInsertionSort [3,4,1,6,9,3,1,4]
                        in if sorted == [1,1,3,3,4,4,6,9]
                            then "True"
                        else ("Expected " ++ (show [1,1,3,3,4,4,6,9]) ++ " but got " ++ (show sorted))

writeAcWait :: HistoryModule -> HistoryModule -> IO()
writeAcWait (pedSimples, pedCoVars) (carSimples, carCoVars) = do
    writeFile "acwait.dat" $ unlines $ map formatCoVars $ zip pedCoVars carCoVars

formatCoVars :: (Double, Double) -> String
formatCoVars (pedCoVar, carCoVar) = (show pedCoVar) ++ "\t" ++ (show carCoVar)

main = do
    args <- getArgs
    if length args < 3 then putStrLn "Usage: ./sim (M|T) <time> <seed>"
    else
        if (==) 'T' $ head (args !! 0) then
            -- Run some sanity tests to make sure events are processed appropriately
            let seed = read $ args !! 1 :: Int
                in do
                    putStrLn "Testing Single Pedestrian Spawn"
                    -- putStrLn $ testSinglePed seed
                    -- putStrLn $ testSingleLCar seed
                    -- putStrLn $ testSingleRCar seed
                    putStrLn ("InsertionSort works? " ++ testInsertionSort)
        else
            -- Run the simulation
            let fintime = read $ args !! 1 :: Double
                seed = read $ args !! 2 :: Int
                in do
                    putStrLn ("Running the simulation with seed=" ++ (show seed) ++ ", ending at time=" ++ (show fintime))
                    -- Iterate until the sim is over, then show the final-state
                    let endstate = iterateUntilNoTomorrow fintime $ justStateToMaybe $ startState seed
                        past = getPast endstate
                        stats@(
                            pedwaits@(pedpt, pedmin, pedmax, pedavg, pedrawvar, pedcount),
                            carwaits@(carpt, carmin, carmax, caravg, carrawvar, carcount),
                            (peds, cars),
                            (pedCoVars),
                            (carCoVars)
                            ) = getStats endstate
                        in do
                            -- mapM_ putStrLn (map show past)
                            putStrLn ("Is the past decreasing? " ++ (show $ validateDecreasing past))
                            -- if (pedsremain > 0) then putStrLn ("There were remaining pedestrians. Remaining events: " ++ (show (maybeCons (getCurrent endstate) $ getFuture endstate)))
                            -- else putStrLn "No remaining pedestrians. Thats good"

                            -- if (carsremain > 0) then putStrLn ("There were remaining cars. Remaining events: " ++ (show (maybeCons (getCurrent endstate) $ getFuture endstate)))
                            -- else putStrLn "No remaining cars. Thats good"
                            -- Show the final statistics
                            putStrLn ("OUTPUT Duration " ++ (show (((time $ head past) - fintime)/60)))
                            putStrLn ("OUTPUT Total Pedestrians " ++ (show peds))
                            putStrLn ("OUTPUT Total Cars " ++ (show cars))
                            printf "OUTPUT Pedestrian min=%f max=%f avg=%f stdev=%f\n" pedmin pedmax pedavg (sqrt(pedrawvar/fromIntegral pedcount))
                            printf "OUTPUT Car min=%f max=%f avg=%f stdev=%f\n" carmin carmax caravg (sqrt(carrawvar/fromIntegral carcount))
                            writeAcWait pedCoVars carCoVars
