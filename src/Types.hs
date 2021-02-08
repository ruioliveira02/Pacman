{- |
    Module: DataTypes

    Module with all the datatypes definitions needed for the Pacman
-}


module Types where

import Data.List



defaultDelayTime =  250 -- 250 ms



-- | Represents the state of the Game at a given time
data State = State 
    {
     maze :: Maze -- ^ The maze of the game
 ,   playersState :: [Player] -- ^ The players in the game
 ,   level :: Int -- ^ The current level of the game
    } deriving Eq

type Maze = [Corridor] -- always horizontal
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)

-- | Represents a player. Each player can be uniquely described by a @ PacState @ or a @ GhoState @
-- depending if it is a pacman or a ghost
data Player  =  Pacman PacState -- ^ Player is a pacman, described by its PacState
              | Ghost GhoState -- ^ Player is a ghost, described by its GhoState
               deriving Eq

-- | Represents directions: Left / Right / Up / Down
data Orientation = L -- ^ Left
                 | R -- ^ Right
                 | U -- ^ Up
                 | D -- ^ Down 
                 | Null -- ^ Null
                 deriving (Show, Eq)

-- | A list of all 5 orientations, arbitrarily ordered
orientationsList :: [Orientation]
orientationsList = [U, D, L, R, Null]

-- | Represents the state of Pacman in a given point in time
--
-- == /Example/
-- @pac = PacState (0,(3,2),1,R,5,1) 0 Open Normal@
data PacState = PacState
    {   
        pacState :: PlayerState -- Generic PlayerState which describes the pacman
    ,   timeMega :: Double --  Remaining time Pacman has in Mega Mode
    ,   openClosed :: Mouth --  State of Pacman's mouth (Open or Closed)
    ,   pacmanMode :: PacMode --  Current PacMode (Dying / Mega / Normal)
    
    } deriving Eq

-- | Represents the state of a ghost in a given point in time
--
-- == /Example/
-- @ghost = GhoState (0,(3,2),1,R,5,1) Alive@
data GhoState = GhoState
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

-- | Coordinates in the maze, given as a @(y, x)@ pair
-- The maze can be as a two-dimensional grid, where the origin (0,0) is the top left corner. The x-axis is the horizontal axis and the y-axis the vertical axis.
-- The y coordinate increases as you go down, and the x coordinate increases as you go to the right
type Coords = (Int,Int)

-- | Represents the state of a generic player (i.e. Pacman or ghosts) in a given point in time
type PlayerState = (Int --  The player's unique id
                   ,Coords --  The (y,x) player's location
                   ,Double -- The player's speed
                   ,Orientation -- The player's orientation
                   ,Int --  The player's score
                   ,Int) --  Number of lives the player has left

-- | Represents the state of Pacman's mouth (open or closed)              
data Mouth = Open | Closed deriving (Show, Eq)

-- | Represents the state of Pacman (Dying Normal or Mega) 
data PacMode = Dying
             | Mega --  Pacman has eaten Food Big and can destroy ghosts
             | Normal
             deriving (Show, Eq)

-- | Represents the state of a ghost (dead or alive) 
data GhostMode = Dead --  Pacman can destroy ghost if it is dead
               | Alive --  Pacman can not touch ghost if it is alive
                 deriving (Show, Eq)

-- | Represents the different kinds of food
data FoodType = Big -- ^ Rarer, worth more points, Pacman enters Mega mode when it eats it
              | Little -- ^ More common, worth less, Pacman does not change mode when it eats it
               deriving Eq

data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

-- | Represents an action made by a player
--  Each action is a movement of a player in a certain direction
-- 
-- == /Warning/
-- An action with a certain orientation does not mean that the player will move in that direction,
-- only that it will be facing in that direction after the action
data Play = Move Int -- ^ The id of the player who performs the action
 Orientation  -- ^ The orientation of the action
    deriving Show
-- | Type used to compact the entire maze
type Instructions = [Instruction]

-- | Data type used to compact a corridor (horizontal)
data Instruction = Instruct [(Int, Piece)] -- ^ There is the same piece in n consecutive squares
               | Repeat Int -- ^ The same as the instruction with index i
               deriving (Show,Eq) -- Two instructions can be compared


-- | Visual representation of @State@
instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

-- | Visual representation of @PacState@
instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

-- | Visual representation of @Player@
instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

-- | Visual representation of @SGhoState@
instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

-- | Visual representation of @FoodType@
instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

-- | Visual Representation of Pieces
instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


-- | Adds the given color to the given string so it can be displayed in the console.
--
-- The supported colors are red, blue, green, purple and yellow
coloredString :: String -- ^ The given string
    -> Color -- ^ The given color
    -> String -- ^ The resulting string
coloredString x _ = x
{-
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 
-}


-- | Places the players on map so it can be printed to the console
--
placePlayersOnMap :: [Player] -- ^ The list of players
    -> Maze -- ^ The maze with no players
    -> Maze -- ^ The maze with the players
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )

-- | Converts the maze to a string
--
printMaze :: Maze -- ^ The given maze
    -> String -- ^ The resulting string
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

-- | Parses the @PlayerState@ to a string
--
-- == /Example/
--
-- >>> printPlayerStats (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- "ID:0 Points:5 Lives:1\n"
printPlayerStats :: Player -- ^ The given player
    -> String -- ^ The resulting string
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++ " Coords:" ++ show b ++ "\n"

-- | Returns the player's ID
--
-- == /Example/
--
-- >>> getPlayerID (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- 0
getPlayerID :: Player -- ^ The given player
    -> Int -- ^ The player's ID
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
-- | Returns the player's points
--
-- == /Example/
--
-- >>> getPlayerPoints (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- 5
getPlayerPoints :: Player -- ^ The given player
    -> Int -- ^ The player's points
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

-- | Changes the player's coordinates
--
-- == /Example/
--
-- >>> setPlayerCoords (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal)) (4,3)
-- Pacman (PacState (0, (4,3), 1, R, 5, 1) 0 Open Normal)
setPlayerCoords :: Player -- ^ The given player
    -> Coords -- ^ The new coordinates
    -> Player -- ^ The resulting player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


-- | Returns the orientation of the given piece
--
-- == /Example/
--
-- >>> getPieceOrientation (PacPlayer (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal)))
-- R
getPieceOrientation :: Piece -- ^ The given piece
    -> Orientation -- ^ The orientation of the piece
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

-- | Returrns the pacMode of the player (only defined for Pacman)
--
-- == /Example/
--
-- >>> getPieceOrientation (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- Normal
getPacmanMode :: Player -- ^ The given player
    -> PacMode -- ^ Its PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  


-- | Returns the @PlayerState@ of a player
--
-- == /Example/
--
-- >>> getPieceOrientation (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- (PacState (0, (1,1), 1, R, 5, 1)
getPlayerState :: Player -- ^ The given player
    -> PlayerState -- ^ The player's @PlayerState@
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a



-- | Returns the orientation of a player
--
-- == /Example/
--
-- >>> getPlayerOrientation (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- R
getPlayerOrientation :: Player -- ^ The given player
    -> Orientation -- ^ The player's orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t


-- | Replaces the element in the given coordinates with the given piece in a maze
--
replaceElemInMaze :: Coords -- ^ The coordinates of the Piece to replace 
    -> Piece -- ^ The piece to replace the old one with 
    -> Maze -- ^ The original maze
    -> Maze -- ^ The resulting maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


-- | Replaces the n-th element in a list with a given element
--
replaceNElem :: Int -- ^ The position of the element
    -> a -- ^ The element to replace the old one with
    -> [a] -- ^ The original list
    -> [a] -- ^ The resulting list
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- | Returns the coordinates of a given player
--
-- == /Example/
--
-- >>> getPlayerCoords (Pacman (PacState (0, (1,1), 1, R, 5, 1) 0 Open Normal))
-- (1,1)
getPlayerCoords :: Player -- ^ The given player
    -> Coords -- ^ The coordinates of the player
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y


{-

getPlayer :: Int -> [Player] -> Player
getPlayer id (p@(Pacman (PacState (pid,_,_,_,_,_) _ _ _)):t) | pid == id = p
                                                             | otherwise = getPlayer id t
getPlayer id (g@(Ghost (GhoState (pid,_,_,_,_,_) _ )):t) | pid == id = g
                                                         | otherwise = getPlayer id t
                                                         -}