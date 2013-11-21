
import Data.Maybe
import Data.Sequence (unfoldl)
import Debug.Trace
-- import Control.Monad.State
import System.Environment
import System.Random.Lehmer.Base

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
    -- id :: Maybe Int         -- Unique Identified. For either a pedestrian or a car. (Car unimplemented/unused right now)
                            -- NOTE: Why did I need the ID?
} deriving (Show)
instance Eq Event where
    (Event _ t1 _) == (Event _ t2 _) = t1 == t2
instance Ord Event where
    (Event _ t1 _) `compare` (Event _ t2 _) = t1 `compare` t2

type PoolItem = (Int, Double)
type Pool = [PoolItem]
--              PastEvents, Current State, FutureEvents
--              PastEvents will only contain important past events; listed below are these important events
--                                                      (PersonID, Speed)
--                                                      This is the pool of folk waiting at the button
--                                                      TODO: Why do I need the ID? Will be set to 0 until I figure that out
--
--                                                      Aside: Do I need a pool for cars as well???
type SystemState = ([Event], Maybe Event, [Event], LehmerState, Pool, StatState)
type JustSystemState = ([Event], Event, [Event], LehmerState, Pool, StatState)

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

-- Simple includes (min, max, avg, stdev, count)
type SimpleStat = (Double, Double, Double, Double, Int)
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
    (999999, 0, 0, 0, 0),
    (999999, 0, 0, 0, 0),
    (0, 0),
    ([(999999, 0, 0, 0, 0)], [0]),
    ([(999999, 0, 0, 0, 0)], [0]) )

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
addSimple pt (min, max, avg, var, n) =
    let newcount = n+1
        newavg = welfordAvg avg (n+1) pt
        newstdev = welfordVar var avg newcount pt
    in
    (
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
    if length stats < 20 then (newstat:stats, (calculateCoVar (head coVars) newstat):coVars)
    else (newstat:(init stats),
        (calculateCoVar (head coVars) newstat):(init coVars)
    )

calculateCoVar :: AutoCoVarStat -> SimpleStat -> AutoCoVarStat
calculateCoVar oldCoVar (_,_, avg, var, newcount) = oldCoVar

--------
-- Welford Equations
--------
welfordAvg :: Double -> Int -> Double -> Double
welfordAvg lastAvg newCount nextVal = lastAvg + (nextVal - lastAvg) / fromIntegral newCount

welfordVar :: Double -> Double -> Int -> Double -> Double
welfordVar _ _ 0 nextVal = 0
welfordVar _ _ 1 nextVal = 0
welfordVar lastVar lastAvg newCount nextVal = lastVar + ((fromIntegral newCount-1)/fromIntegral newCount) * ((nextVal - lastAvg)**2)

getWelford :: SimpleStat -> Double
getWelford (_, _, avg, _, n) = avg/ fromIntegral n


--------
-- Maybe Hacks and Other Small things
--------

justStateToMaybe :: JustSystemState -> SystemState
justStateToMaybe (past, current, future, rgen, pedpool, stats) = (past, Just current, future, rgen, pedpool, stats)

maybeStateToJust :: SystemState -> JustSystemState
maybeStateToJust (past, current, future, rgen, pedpool, stats) = (past, (fromMaybe (Event CheckPool 0 Nothing) current), future, rgen, pedpool, stats)
-- the current event must be a maybe. WHEN current=Nothing, the simulation must be over. There are no more events

getPast :: SystemState -> [Event]
getPast (past,_,_,_,_,_) = past

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

pairAddQuad :: (a,b) -> (c,d,e,f) -> (a,b,c,d,e,f)
pairAddQuad (a,b) (c,d,e,f) = (a,b,c,d,e,f)

-- Create a quintuple from a quad by adding an item to the end
addToQuad :: e -> (a,b,c,d) -> (a,b,c,d,e)
addToQuad e (a,b,c,d) = (a,b,c,d,e)

addPairToQuad :: (e,f) -> (a,b,c,d) -> (a,b,c,d,e,f)
addPairToQuad (e,f) (a,b,c,d) = (a,b,c,d,e,f)

startQuadWithPair :: (a,b) -> (c,d,e,f) -> (a,b,c,d,e,f)
startQuadWithPair (a,b) (c,d,e,f) = (a,b,c,d,e,f)

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
stripOutOfTime endTime (past, current, [], rgen, pedpool, stats) = (past, current, [], rgen, pedpool, stats)
stripOutOfTime endTime (past, current, future, rgen, pedpool, stats) = (past, current, filter (\e-> not $ time e > endTime && eventType e `elem` [LSpawnCar, RSpawnCar, SpawnPed]) future, rgen, pedpool, stats)

getNextEvent :: [Event] -> (Event, [Event])
getNextEvent elist = (head elist, tail elist)

rotateEvents :: JustSystemState -> SystemState
-- rotateEvents (past, current, future, rgen, pedpool)
--    | trace ("Moving this event into the past: " ++ show current) False = undefined
--    | trace ("Future is: " ++ (show future)) False = undefined
--    | trace ("Advancing simulation time to " ++ (show $ time current)) False = undefined
rotateEvents (past, current, [], rgen, pedpool, stats) = (smartInsert past current, Nothing, [], rgen, pedpool, stats)
rotateEvents (past, current, future, rgen, pedpool, stats) = (smartInsert past current, Just (head future), tail future, rgen, pedpool, stats)

applyRandom :: [Event] -> EventType -> (Maybe Double, Double, LehmerState) -> ([Event], LehmerState)
-- applyRandom future etype (speed, timeval, rgen)
--    | trace ("Applying the random number " ++ show timeval) False = undefined
applyRandom future etype (speed, timeval, rgen) = (sortedInsertion future (Event etype timeval speed), rgen)

addCheckPool :: JustSystemState -> JustSystemState
addCheckPool (past, current, future, rgen, pedpool, stats) = (past, current, sortedInsertion future (Event CheckPool (time current + 60) Nothing), rgen, pedpool, stats)

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

carSpawnTransform :: (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
carSpawnTransform (timeval, rgen) = (Nothing, timeval, rgen)

--------
-- Calls to random
--------
randomPedSpeed :: LehmerState -> (Double, LehmerState)
randomPedSpeed = uniformRange 6 13 . stream 0

randomLCarSpeed :: LehmerState -> (Double, LehmerState)
randomLCarSpeed = uniformRange 25 35 . stream 2

randomRCarSpeed :: LehmerState -> (Double, LehmerState)
randomRCarSpeed = uniformRange 25 35 . stream 3

randomPedSpawn :: LehmerState -> (Double, LehmerState)
randomPedSpawn = makeExponential (60/4) . uniform . stream 1

randomLCarSpawn :: LehmerState -> (Double, LehmerState)
randomLCarSpawn = makeExponential (60/4) . uniform . stream 4

randomRCarSpawn :: LehmerState -> (Double, LehmerState)
randomRCarSpawn = makeExponential (60/4) . uniform . stream 5

randomButtonPressNow :: Double -> (Event, [Event], LehmerState) -> ([Event], LehmerState)
randomButtonPressNow probability (current, future, rgen)
    = let (u, newgen) = uniform rgen
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
--    | trace ("Turning uniform=" ++ show u ++ " into an exponential distribution") False = undefined
makeExponential lambda (u, s) = ( -lambda * log (1-u) , s)

-- Linker that adds a new pedestrian-spawn to the event list.
-- Note: This does not actually spawn a pedestrian. Merely an event for WHEN a pedestrian will spawn
addPedSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addPedSpawn now (elist, rgen) = applyRandom elist SpawnPed $ pedSpeedTransformComing now $ randomPedSpawn rgen -- randomCarSpawn rgen

addLCarSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addLCarSpawn now (elist, rgen) = applyRandom elist LSpawnCar $ carSpeedTransformComing now $ randomLCarSpawn rgen -- randomCarSpawn rgen

addRCarSpawn :: Double -> ([Event], LehmerState) -> ([Event], LehmerState)
addRCarSpawn now (elist, rgen) = applyRandom elist RSpawnCar $ carSpeedTransformComing now $ randomRCarSpawn rgen -- randomCarSpawn rgen

-- Push the button now if u[0,1) < probability
maybePushButton :: Double -> (Event, [Event], LehmerState, Pool, StatState) -> ([Event], LehmerState, Pool, StatState)
maybePushButton probability (current, future, rgen, pedpool, stats) = addPairToPair (pedpool, stats) $ randomButtonPressNow probability (current, future, rgen)

-- Have everyone in the pedestrian pool walk across the crosswalk
dumpPool :: Double -> [(Int, Double)] -> [Event] -> [Event]
dumpPool now pool elist = foldl (poolFold now) elist pool

poolFold :: Double -> [Event] -> (Int, Double) -> [Event]
poolFold now accum poolitem = sortedInsertion accum (Event PedWalkEnd (now + 48.0/snd poolitem) (Just (snd poolitem)))

--------
-- Event Processing and Simulation Components
--------

-- Takes the current event, and returns the future-list of Events (where the head is the next event)
processEvent :: JustSystemState -> JustSystemState
processEvent (past, current, future, rgen, pedpool, stats) =
    case eventType current of
                    -- TODO: When Red, I should probably be emptying the pool
                    -- TODO: I probably want to strip out any CheckPool events from the future, since a person in the past may now be moving on
        Red     -> (past, current, filter (\x-> eventType x /= CheckPool) $ dumpPool (time current) pedpool $ sortedInsertion future (Event Green (time current + 12) Nothing), rgen, [], stats)
        Yellow  -> (past, current, sortedInsertion future (Event Red (time current + 8) Nothing), rgen, pedpool, stats)
        Green   -> (past, current, future, rgen, pedpool, stats) -- "Let the cars go" -- That would entail emptying a carpool...?
        PedAtButton ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Yellow  -> (past, current, sortedInsertion future (Event PedWalkEnd (time $ head $ filter (\x -> eventType x == Red) future) (speed current)), rgen, pedpool, stats)
                            -- If The time of the next Green - now >= time to cross (If there's time to cross)
                    Red     -> if (time $ head $ filter (\x -> eventType x == Green) future) - time current >= (48.0 / fromMaybe 0 (speed current))
                                    then (past, current, sortedInsertion future (Event PedWalkEnd ((time current) + 48.0 / fromMaybe 0 (speed current)) (speed current)), rgen, pedpool, stats)
                                    -- There was not enough time to cross. Wait around in the pool
                                    else addCheckPool (past, current, future, rgen, (0, fromMaybe 0 (speed current)):pedpool, stats)
                            -- Is he/she alone?
                    Green   ->  if length pedpool == 0
                                    -- There's a 2/3 chance that they'll push the button immediately
                                    then pairAddQuad (past, current) $ maybePushButton (2/3) (current, future, rgen, (0, fromMaybe 0 $ speed current):pedpool, stats)
                                else
                                    -- If others are around, probability to push button immediately is 1/n
                                    -- TODO: Does 1/n INCLUDE the new pedestrian? Or not. Currently, it assumes not. do 1/ n+1 if it does
                                    startQuadWithPair (past, current) $ maybePushButton (1.0/ fromIntegral (length pedpool)) (current, future, rgen, (0, fromMaybe 0 $ speed current):pedpool, stats)
                    _       -> (past, current, future, rgen, pedpool, stats) -- TODO: Should I add a trace here, to indicate that this is going on?
        -- TODO: Check time current, and prevent any spawning if it's greater than the proposed simulation end time
        SpawnPed -> addPairToQuad (pedpool, stats) $ dupleCombine (past, current) $ addPedSpawn (time current) $ applyRandom future PedAtButton $ pedSpeedTransformComing (time current) $ randomPedSpeed rgen
        LSpawnCar -> addPairToQuad (pedpool, stats) $ dupleCombine (past, current) $ addLCarSpawn (time current) $ applyRandom future LCarStop $ carSpeedTransformComing (time current) $ randomLCarSpeed rgen
        RSpawnCar -> addPairToQuad (pedpool, stats) $ dupleCombine (past, current) $ addRCarSpawn (time current) $ applyRandom future RCarStop $ carSpeedTransformComing (time current) $ randomRCarSpeed rgen
        LCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, stats) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, stats)
                               else (past, current, sortedInsertion future $ Event LCarStop (time $ getFirstEvent Red future) (speed current), rgen, pedpool, stats)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                    Red     -> (past, current, sortedInsertion future $ addTimeToEvent ((time $ getFirstEvent Green future) - (time current)) $ carSpeedTransformGoing (time current) LCarLeave $ speed current, rgen, pedpool, stats)
                                -- Leave when the green comes up. We know when that is, so schedule a CarLeave
                    _       -> (past, current, future, rgen, pedpool, stats) -- Do.. nothing... I guess... TODO: trace?
        RCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, stats) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, stats)
                               else (past, current, sortedInsertion future $ Event RCarStop (time $ getFirstEvent Red future) (speed current), rgen, pedpool, stats)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                    Red     -> (past, current, sortedInsertion future $ addTimeToEvent ((time $ getFirstEvent Green future) - (time current)) $ carSpeedTransformGoing (time current) RCarLeave $ speed current, rgen, pedpool, stats)
                    _       -> (past, current, future, rgen, pedpool, stats) -- Do.. nothing... I guess... TODO: trace?
        PedPushButton -> (past, current, sortedInsertion future (Event Yellow (max (time current + 1) (time $ getFirstEvent Green past)) Nothing), rgen, pedpool, stats)
        CheckPool -> if length pedpool > 0 then (past, current, sortedInsertion future (Event PedPushButton (time current) Nothing), rgen, pedpool, stats) else (past, current, future, rgen, pedpool, stats)
        PedWalkEnd -> (past, current, future, rgen, pedpool, stats) -- A Pedestrian is leaving. We will do nothing in particular
        LCarLeave -> (past, current, future, rgen, pedpool, stats)  -- A Car is leaving. We will do nothing in particular
        RCarLeave -> (past, current, future, rgen, pedpool, stats)  -- A Car is leaving. We will do nothing in particular
        -- Hopefully, this catch-all will never happen... hopefully...
        -- _ -> (past, current, future, rgen, pedpool)

-- Gather up an event list (so that we can get statistics from it)
--simulationStepper :: Double -> JustSystemState -> Maybe (SystemState, Event)
--simulationStepper endTime s@(past, current, future, rgen, pedpool) =
--    if length future > 0
--        then Just ((nextState endTime s), current)
--    else Nothing

--    if event_type current == Red then
--        sortedInsertion future (Event Green (time current + 12))
--    else if event_type current ==

-- TODO: Should a Monad be used??
-- Monad stuffs. The StateMonad makes the folding quite nice
--type SystemStateMonad = State SystemState
--
--nextEventM :: SystemState -> SystemStateMonad SystemState
--nextEventM s = return $ nextState s

assert :: Bool -> a -> a
assert False x = error "Failed Assertion!"
assert _ x = x

startState :: Int -> JustSystemState
startState seed = ([], Event SpawnPed 0 Nothing, [Event LSpawnCar 0 Nothing, Event RSpawnCar 0 Nothing, Event Green 0 Nothing], lehmerInit seed, [], initStatState)

singleSpawnPed :: Int -> JustSystemState
singleSpawnPed seed = ([], Event SpawnPed 0 Nothing, [], lehmerInit seed, [], initStatState)

singleSpawnLCar :: Int -> JustSystemState
singleSpawnLCar seed = ([], Event LSpawnCar 0 Nothing, [], lehmerInit seed, [], initStatState)

singleSpawnRCar :: Int -> JustSystemState
singleSpawnRCar seed = ([], Event RSpawnCar 0 Nothing, [], lehmerInit seed, [], initStatState)

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
    let singlePed@(past, current, future, rgen, pedpool, stats) = nextState 193 $ singleSpawnPed seed
        in if length past /= 1 then "Did not process first event in testSinglePed"
            else if not (checkEventContainment PedAtButton 193 (maybeCons current future)) then "Expected a PedAtButton to be scheduled within 193 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSinglePed. The first pedestrian will arrive at the button at " ++ show (time (fromMaybe (head future) current))

-- Longest time it will take a car to reach the crosswalkstop is 31.17s
testSingleLCar :: Int -> String
testSingleLCar seed =
    let singleLCar@(past, current, future, rgen, pedpool, stats) = nextState 32 $ singleSpawnLCar seed
        in if length past /= 1 then "Did not process first event in testSingleRCar"
            else if not (checkEventContainment LCarStop 32.0 (maybeCons current future)) then "Expected an LCarStop to be scheduled within 32 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSingleLCar. The first car will arrive at the crosswalk at " ++ show (time (fromMaybe (head future) current))

-- Longest time it will take a car to reach the crosswalkstop is 31.17s
testSingleRCar :: Int -> String
testSingleRCar seed =
    let singleRCar@(past, current, future, rgen, pedpool, stats) = nextState 32 $ singleSpawnRCar seed
        in if length past /= 1 then "Did not process first event in testSingleRCar"
            else if not (checkEventContainment RCarStop 32.0 (maybeCons current future)) then "Expected an RCarStop to be scheduled within 32 seconds, but future was " ++ (show future)
            else if length future /= 1 then "The next pedestrian spawn was not created. Here's the future-list: " ++ show future
            else "No errors with testSingleRCar. The first car will arrive at the crosswalk at " ++ show (time (fromMaybe (head future) current))

iterateUntilNoTomorrow :: Double -> SystemState -> SystemState
iterateUntilNoTomorrow endtime start@(past, current, future, rgen, pedpool, stats)
-- Only stop when there's no future, and we have no current event to process
    | (isNothing current) && length future == 0 = (past, Nothing, future, rgen, pedpool, stats)
    | True                  = iterateUntilNoTomorrow endtime $ nextState endtime (maybeStateToJust start)

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
            let time = read $ args !! 1 :: Double
                seed = read $ args !! 2 :: Int
                in do
                    putStrLn ("Running the simulation with seed=" ++ (show seed) ++ ", ending at time=" ++ (show time))
                    -- Iterate until the sim is over, then show the final-state
                    let past = getPast $ iterateUntilNoTomorrow time $ justStateToMaybe $ startState seed
                        in do
                            mapM_ putStrLn (map show past)
                            putStrLn ("Past decreasing?" ++ (show $ validateDecreasing past))
