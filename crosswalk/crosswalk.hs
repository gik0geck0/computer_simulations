
import Data.Maybe
-- import Control.Monad.State
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
type SimState = ([Event], Event, [Event], LehmerState, Pool)


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

pairAddTriplet :: (a,b) -> (c,d,e) -> (a,b,c,d,e)
pairAddTriplet (a,b) (c,d,e) = (a,b,c,d,e)

-- Create a quintuple from a quad by adding an item to the end
addToQuad :: e -> (a,b,c,d) -> (a,b,c,d,e)
addToQuad e (a,b,c,d) = (a,b,c,d,e)

--------
-- List Helpers
--------

-- Insert the item into the list in it's sorted order
-- Note that it will be the LAST item in a sequence of equals
sortedInsertion :: (Ord a) => [a] -> a -> [a]
sortedInsertion list item = [a | a <- list, a <= item] ++ [item] ++ [b | b <- list, b > item]

-- Place the event into the past-event list only if it will be needed later
-- TODO: Right now, ALL Events are being held in the history
smartInsert :: [Event] -> Event -> [Event]
smartInsert pEvents cEvent = cEvent:pEvents

valFromState :: SimState -> Event
valFromState (_, v, _, _, _) = v

-- Take the past events, current event and the list of future events, and produce the updated past events, next event, and the list of future events
nextState :: SimState -> SimState
nextState = rotateEvents . processEvent

getNextEvent :: [Event] -> (Event, [Event])
getNextEvent elist = (head elist, tail elist)

rotateEvents :: SimState -> SimState
rotateEvents (past, current, future, rgen, pedpool) = (smartInsert past current, head future, tail future, rgen, pedpool)

applyRandom :: [Event] -> EventType -> (Maybe Double, Double, LehmerState) -> ([Event], LehmerState)
applyRandom future etype (speed, timeval, rgen) = (sortedInsertion future (Event etype timeval speed), rgen)

addCheckPool :: SimState -> SimState
addCheckPool (past, current, future, rgen, pedpool) = (past, current, sortedInsertion future (Event CheckPool (time current + 60) Nothing), rgen, pedpool)

-- Find the first occurance of a Green Light
getFirstEvent :: EventType -> [Event] -> Event
getFirstEvent etype elist = head $ filter (\x -> eventType x == etype) elist
-- TODO: elist == []?

--------
-- Speed-transforms. Takes a randomly generated speed, and translates it into TIME. (intrinsicly event/state dependant, represented by the function name)
--------
pedSpeedTransformComing :: (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
pedSpeedTransformComing (speed, rgen) = (Just speed, 1155.0 / speed, rgen)

carSpeedTransformComing :: (Double, LehmerState) -> (Maybe Double, Double, LehmerState)
carSpeedTransformComing (speed, rgen) = (Just speed, 1143.0 / speed * 528.0 / 360.0, rgen)

-- This KINDA does the speedTransform. It takes a speed, and creates an Event for when the car will leave the simulation
carSpeedTransformGoing :: EventType -> (Maybe Double) -> Event
carSpeedTransformGoing etype speed = Event etype (1191.0 / (fromMaybe 0 speed) * 528.0 / 360.0) speed
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

randomPedSpawn :: LehmerState -> (Double, LehmerState)
randomPedSpawn = makeExponential (4/60) . uniform . stream 1

randomLCarSpeed :: LehmerState -> (Double, LehmerState)
randomLCarSpeed = uniformRange 25 35 . stream 2

randomRCarSpeed :: LehmerState -> (Double, LehmerState)
randomRCarSpeed = uniformRange 25 35 . stream 3

randomLCarSpawn :: LehmerState -> (Double, LehmerState)
randomLCarSpawn = makeExponential (4/60) . uniform . stream 4

randomRCarSpawn :: LehmerState -> (Double, LehmerState)
randomRCarSpawn = makeExponential (4/60) . uniform . stream 5

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
makeExponential lambda (u, s) = ( lambda * log (1-u) , s)

-- Linker that adds a new pedestrian-spawn to the event list.
-- Note: This does not actually spawn a pedestrian. Merely an event for WHEN a pedestrian will spawn
addPedSpawn :: ([Event], LehmerState) -> ([Event], LehmerState)
addPedSpawn (elist, rgen) = applyRandom elist SpawnPed $ pedSpeedTransformComing $ randomPedSpawn rgen -- randomCarSpawn rgen

addLCarSpawn :: ([Event], LehmerState) -> ([Event], LehmerState)
addLCarSpawn (elist, rgen) = applyRandom elist LSpawnCar $ carSpeedTransformComing $ randomLCarSpawn rgen -- randomCarSpawn rgen

addRCarSpawn :: ([Event], LehmerState) -> ([Event], LehmerState)
addRCarSpawn (elist, rgen) = applyRandom elist RSpawnCar $ carSpeedTransformComing $ randomRCarSpawn rgen -- randomCarSpawn rgen

-- Push the button now if u[0,1) < probability
maybePushButton :: Double -> (Event, [Event], LehmerState, Pool) -> ([Event], LehmerState, Pool)
maybePushButton probability (current, future, rgen, pedpool) = tupleAppend pedpool $ randomButtonPressNow probability (current, future, rgen)

--------
-- Event Processing and Simulation Components
--------

-- Takes the current event, and returns the future-list of Events (where the head is the next event)
processEvent :: SimState -> SimState
processEvent (past, current, future, rgen, pedpool) =
    case eventType current of
                    -- TODO: When Red, I should probably be emptying the pool
                    -- TODO: I probably want to strip out any CheckPool events from the future, since a person in the past may now be moving on
        Red     -> (past, current, sortedInsertion future (Event Green (time current + 12) Nothing), rgen, pedpool)
        Yellow  -> (past, current, sortedInsertion future (Event Red (time current + 8) Nothing), rgen, pedpool)
        Green   -> (past, current, future, rgen, pedpool) -- "Let the cars go" -- That would entail emptying a carpool...?
        PedAtButton ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Yellow  -> (past, current, sortedInsertion future (Event PedWalkEnd (time $ head $ filter (\x -> eventType x == Red) future) (speed current)), rgen, pedpool)
                            -- If The time of the next Green - now >= time to cross (If there's time to cross)
                    Red     -> if (time $ head $ filter (\x -> eventType x == Green) future) - time current >= (48.0 / fromMaybe 0 (speed current))
                                    then (past, current, sortedInsertion future (Event PedWalkEnd (48.0 / fromMaybe 0 (speed current)) (speed current)), rgen, pedpool)
                                    -- There was not enough time to cross. Wait around in the pool
                                    else addCheckPool (past, current, future, rgen, (0, fromMaybe 0 (speed current)):pedpool)
                            -- Is he/she alone?
                    Green   ->  if length pedpool == 0
                                    -- There's a 2/3 chance that they'll push the button immediately
                                    then pairAddTriplet (past, current) $ maybePushButton (2/3) (current, future, rgen, (0, fromMaybe 0 $ speed current):pedpool)
                                else
                                    -- If others are around, probability to push button immediately is 1/n
                                    -- TODO: Does 1/n INCLUDE the new pedestrian? Or not. Currently, it assumes not. do 1/ n+1 if it does
                                    pairAddTriplet (past, current) $ maybePushButton (1.0/ fromIntegral (length pedpool)) (current, future, rgen, (0, fromMaybe 0 $ speed current):pedpool)
                    _       -> (past, current, future, rgen, pedpool) -- TODO: Should I add a trace here, to indicate that this is going on?
        SpawnPed -> addToQuad pedpool $ dupleCombine (past, current) $ addPedSpawn $ applyRandom future PedAtButton $ pedSpeedTransformComing $ randomPedSpeed rgen
        LSpawnCar -> addToQuad pedpool $ dupleCombine (past, current) $ addLCarSpawn $ applyRandom future LCarStop $ carSpeedTransformComing $ randomLCarSpeed rgen
        RSpawnCar -> addToQuad pedpool $ dupleCombine (past, current) $ addRCarSpawn $ applyRandom future RCarStop $ carSpeedTransformComing $ randomRCarSpeed rgen
        LCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing LCarLeave $ speed current, rgen, pedpool) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing LCarLeave $ speed current, rgen, pedpool)
                               else (past, current, sortedInsertion future $ Event LCarStop (time $ getFirstEvent Red future) (speed current), rgen, pedpool)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                    Red     -> (past, current, sortedInsertion future $ addTimeToEvent ((time $ getFirstEvent Green future) - (time current)) $ carSpeedTransformGoing LCarLeave $ speed current, rgen, pedpool)
                                -- Leave when the green comes up. We know when that is, so schedule a CarLeave
                    _       -> (past, current, future, rgen, pedpool) -- Do.. nothing... I guess... TODO: trace?
        RCarStop ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Green   -> (past, current, sortedInsertion future $ carSpeedTransformGoing RCarLeave $ speed current, rgen, pedpool) -- Schedule CarLeave
                    Yellow  -> if (time $ getFirstEvent Red future) - time current >= (fromMaybe 0 $ speed current) / 48
                                    then (past, current, sortedInsertion future $ carSpeedTransformGoing RCarLeave $ speed current, rgen, pedpool)
                               else (past, current, sortedInsertion future $ Event RCarStop (time $ getFirstEvent Red future) (speed current), rgen, pedpool)
                               -- else stop. TODO: carpool??? We don't know when the Green will hit
                                   -- TODO: Hack: reschedule the car to stop (again) when the RED happens. THEEEENNN it'll get scheduled after the Green
                    Red     -> (past, current, sortedInsertion future $ addTimeToEvent ((time $ getFirstEvent Green future) - (time current)) $ carSpeedTransformGoing RCarLeave $ speed current, rgen, pedpool)
                    _       -> (past, current, future, rgen, pedpool) -- Do.. nothing... I guess... TODO: trace?
        PedPushButton -> (past, current, sortedInsertion future (Event Yellow (max (time current + 1) (time $ getFirstEvent Green past)) Nothing), rgen, pedpool)
        CheckPool -> if length pedpool > 0 then (past, current, sortedInsertion future (Event PedPushButton (time current) Nothing), rgen, pedpool) else (past, current, future, rgen, pedpool)
        PedWalkEnd -> (past, current, future, rgen, pedpool) -- A Pedestrian is leaving. We will do nothing in particular
        LCarLeave -> (past, current, future, rgen, pedpool)  -- A Car is leaving. We will do nothing in particular
        RCarLeave -> (past, current, future, rgen, pedpool)  -- A Car is leaving. We will do nothing in particular
        -- Hopefully, this catch-all will never happen... hopefully...
        -- _ -> (past, current, future, rgen, pedpool)

-- Gather up an event list (so that we can get statistics from it)
simulationStepper :: SimState -> Maybe (SimState, Event)
simulationStepper s@(past, current, future, rgen, pedpool) =
    if length future > 0
        then Just ((nextState s), current)
    else Nothing

--    if event_type current == Red then
--        sortedInsertion future (Event Green (time current + 12))
--    else if event_type current ==

-- TODO: Should a Monad be used??
-- Monad stuffs. The StateMonad makes the folding quite nice
--type SimStateMonad = State SimState
--
--nextEventM :: SimState -> SimStateMonad SimState
--nextEventM s = return $ nextState s

startState :: SimState
startState = ([], Event SpawnPed 0 Nothing, [Event LSpawnCar 0 Nothing, Event RSpawnCar 0 Nothing], lehmerInit 1, [])

main = do
    putStrLn "Hello, world!"
    putStrLn $ show $ nextState startState
