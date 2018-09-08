module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum, Bounded)

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ cs) = cs

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

turnRight :: Bearing -> Bearing
turnRight b
  | b == maxBound = minBound
  | otherwise = succ b

turnLeft :: Bearing -> Bearing
turnLeft b
  | b == minBound = maxBound
  | otherwise = pred b

turnRobotRight :: Robot -> Robot
turnRobotRight (Robot direction cs) = Robot (turnRight direction) cs

turnRobotLeft :: Robot -> Robot
turnRobotLeft (Robot direction cs) = Robot (turnLeft direction) cs

advanceRobot :: Robot -> Robot
advanceRobot (Robot North (x, y)) = Robot North (x, y + 1)
advanceRobot (Robot South (x, y)) = Robot South (x, y - 1)
advanceRobot (Robot East (x, y)) = Robot East (x + 1, y)
advanceRobot (Robot West (x, y)) = Robot West (x - 1, y)

command :: Char -> (Robot -> Robot)
command 'R' = turnRobotRight
command 'L' = turnRobotLeft
command 'A' = advanceRobot

simulate :: Robot -> String -> Robot
simulate robot instructions = foldl (flip ($)) robot commands
  where commands = map command instructions
