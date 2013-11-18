
-- import Control.Monad.State
import System.Random

data EventType = Red | Green | Yellow | SpawnCar | CarStop | CarLeave | SpawnPed | PedAtButton | PedPushButton | PedWalkEnd deriving (Enum, Eq, Ord, Show)

data Event = Event {
    eventType :: EventType,
    time :: Int,
    speed :: Maybe Int  -- Speed in ft/s for CarEvents and PedestrianEvents
} deriving (Show)
instance Eq Event where
    (Event _ t1 _) == (Event _ t2 _) = t1 == t2
instance Ord Event where
    (Event _ t1 _) `compare` (Event _ t2 _) = t1 `compare` t2

sortedInsertion :: (Ord a) => [a] -> a -> [a]
sortedInsertion list item = [a | a <- list, a < item] ++ [item] ++ [b | b <- list, b >= item]

type SimValue = Int
--              PastEvents, Current State, FutureEvents
--              PastEvents will only contain important past events; listed below are these important events
type SimState = ([Event], Event, [Event], StdGen)

tupleInsert :: a -> (b,c) -> (a,b,c)
tupleInsert a (b,c) = (a,b,c)

dupleCombine :: (a,b) -> (c,d) -> (a,b,c,d)
dupleCombine (a,b) (c,d) = (a,b,c,d)

-- Place the event into the past-event list only if it will be needed later
smartInsert :: [Event] -> Event -> [Event]
smartInsert pEvents cEvent = cEvent:pEvents

valFromState :: SimState -> Event
valFromState (_, v, _, _) = v

-- Take the past events, current event and the list of future events, and produce the updated past events, next event, and the list of future events
nextState :: SimState -> SimState
nextState = rotateEvents . processEvent

getNextEvent :: [Event] -> (Event, [Event])
getNextEvent elist = (head elist, tail elist)

rotateEvents :: SimState -> SimState
rotateEvents (past, current, future, rgen) = (smartInsert past current, head future, tail future, rgen)

applyRandom :: [Event] -> EventType -> (Maybe Int, Int, StdGen) -> ([Event], StdGen)
applyRandom future etype (speed, timeval, rgen) = (sortedInsertion future (Event etype timeval speed), rgen)

pedSpeedTransform :: (Int, StdGen) -> (Maybe Int, Int, StdGen)
pedSpeedTransform (speed, rgen) = (Just speed, truncate (1155.0 / fromIntegral speed), rgen)

carSpeedTransform :: (Int, StdGen) -> (Maybe Int, Int, StdGen)
carSpeedTransform (speed, rgen) = (Just speed, truncate (1143.0 / ((fromIntegral speed) * 528.0 / 360.0)), rgen)

randomCarSpeed :: StdGen -> (Int, StdGen)
randomCarSpeed = randomR (25, 35)

randomPedSpeed :: StdGen -> (Int, StdGen)
randomPedSpeed = randomR (6,13)

-- Takes the current event, and returns the future-list of Events (where the head is the next event)
processEvent :: SimState -> SimState
processEvent (past, current, future, rgen) =
    case eventType current of
        Red     -> (past, current, sortedInsertion future (Event Green (time current + 12) Nothing), rgen)
        Yellow  -> (past, current, sortedInsertion future (Event Red (time current + 8) Nothing), rgen)
        Green   -> (past, current, future, rgen) -- "Let the cars go"
        PedAtButton ->
            let lightstat   = head $ filter (\x -> x == Green || x == Yellow || x == Red) $ map eventType past
                in case lightstat of
                    Yellow  -> (past, current, sortedInsertion future (Event PedWalkEnd (time $ head $ filter (\x -> eventType x == Red) future) (speed current)), rgen)
                    Red     -> if (time $ head $ filter (\x -> eventType x == Green) future) - time current >= truncate (48.0 / fromMaybe 0 (speed current))
                                    then (past, current, sortedInsertion future (Event PedWalkEnd truncate (48.0 / fromMaybe 0 (speed current)) (speed current)), rgen)
                                    else (past, current, future, rgen)
                    _       -> (past, current, future, rgen)
              -- if Y: walk at R
              -- if R: if G-now >= walktime, walk.
              --     else: --TODO
              -- if G:
              --     if alone, 2/3 check for press now
              --         if not press, press at now+60s
              --     if others, 1/n check for press now
        SpawnPed -> dupleCombine (past, current) $ applyRandom future PedAtButton $ pedSpeedTransform $ randomPedSpeed rgen
        SpawnCar -> dupleCombine (past, current) $ applyRandom future CarStop $ carSpeedTransform $ randomCarSpeed rgen
        _ -> (past, current, future, rgen)
--    if event_type current == Red then
--        sortedInsertion future (Event Green (time current + 12))
--    else if event_type current ==

-- TODO: Should a Monad be used??
-- Monad stuffs. The StateMonad makes the folding quite nice
--type SimStateMonad = State SimState
--
--nextEventM :: SimState -> SimStateMonad SimState
--nextEventM s = return $ nextState s
--
startState :: SimState
startState = ([], Event SpawnPed 0 Nothing, [Event SpawnCar 0 Nothing], mkStdGen 1)

main = do
    putStrLn "Hello, world!"
    putStrLn $ show $ nextState startState
