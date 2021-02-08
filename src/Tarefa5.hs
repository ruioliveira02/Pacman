{-| 
    Module: Tarefa5

    Module responsible for the behaviour of the ghosts.
-}

module Tarefa5 where 

import Data.List as DL
import Data.Hashable

import Types
import PacmanUtils
import LazyBFS
import Tarefa2


-- | Given a list of players, this function returns wether some
--   of the ghosts within that list are at the specified coordinates.
--   
isGhostHere :: [Player] -- ^ the list of players (not necessarily ghosts)
    -> Coords -- ^ the specified coordinates
    -> Bool -- ^ wether any ghost from the list is at the coordinates
isGhostHere list coords = or ((map ((coords ==) . getPlayerCoords)) $ filter isGhost list)
                            where isGhost (Ghost _) = True
                                  isGhost _ = False

-- | Given the list of players, returns the type (1 or 2)
--   of the specified ghost (given by id)
--
--   Type 1 is direct chase, where the ghost will follow the shortest path towards pacman
--
--   Type 2 is advanced chase, where the ghost follows the shortest path to the position
--   4 pieces ahead of pacman, and only then enters direct chase
--
getGhostType :: [Player] -- ^ The list of players (ghosts or otherwise)
    -> Int -- ^ The id of the specified ghost
    -> Int -- ^ The type of the ghost (1 or 2)
getGhostType list id =
    case (findIndex (== id)) $ (map getPlayerID) $ (filter isGhost) list of
        Just x -> mod x 2 + 1
    where isGhost (Ghost _) = True
          isGhost _ = False


-- | Returns the Location of the pacman
--
-- It assumes there is only one pacman player, meaning it will return the first value it finds.
-- It also supposes the player exists in the first place, meaning it is not defined if there is no pacman.
--
-- == /Example/
--
-- >>> getPacmanLocation [(Pacman (PacState (0, (1,1), 2.0, U, 1,1) 2.0 Closed Mega)), (Ghost (GhoState (1,(2,1), 2.5, L,2,2) Dead))]
-- >>> (1,1)
getPacmanLocation :: [Player] -- ^ The list of players
    -> Coords -- ^ The pacman's coordinates
getPacmanLocation ((Pacman ps):t) = coords
                                  where (PacState (_,coords,_,_,_,_) _ _ _) = ps
getPacmanLocation (x:t) = getPacmanLocation t 



-- | Given some coordinates, all the distances between every two points, and a target, returns the distances 
--   from the neighbouring cells and the cell itself to the target that do not contain a ghost, i.e, given a set of coordinates (y, x), it returns
--   the distance from the squares (y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1) and (y, x), in that order,
--   as long as no ghost is contained in them.
--
-- == /Note/
--
--   If any of the neighbouring cells is a wall (meaning it is unreachable) or has a ghost,
--   that value is instead replaced by -1 in the final list.
--
getNeighbourDistances :: Coords -- ^ The given coordinates
    -> Memoization -- ^ All the distances
    -> Coords -- ^ The coordinates of the target
    -> [Player] -- ^ The list of players
    -> [Int] -- ^ The resulting list.
getNeighbourDistances c mem target players
    = (map (\x -> if (x >= maxValue) then -1 else x))
        $ map (\c -> if (c == (-1,-1)) then -1 else distance mem target c)
        $ map (\c -> if (isGhostHere players c) then (-1,-1) else c)
        $ map ((flip (pseudoMove s)) c) orientationsList
    where s = mazeSize mem


-- | Given the index of the first element, a filter and a list, this function
--   returns a list containing the indices of the elements that satisfy said filter
--   (ordered in ascending order)
filterIndices :: Int -- ^ The index of the first element
    -> (a -> Bool)  -- ^ The filtering condition
    -> [a] -- ^ The given list
    -> [Int] -- ^ The resulting list of indices
filterIndices _ f [] = []
filterIndices n f (x:xs) | f x = n : (filterIndices (n + 1) f xs)
                         | otherwise = filterIndices (n + 1) f xs

-- | Calculates the hash of a State by casting it
--   to a String and then calculating its hash (Data.Hashable)
hash' :: State -- ^ the state
    -> Int -- ^ the hash
hash' = hash . show'

-- | Casts a State to a String. Used by hash'
show' :: State -- ^ the state
    -> String -- ^ the resulting string
show' (State _ players _) = concat (map (\x -> show (getPlayerCoords x)  ++ show (getPlayerPoints x) ++ "|" ++ show (getLives x)) players)

-- | Given /n/, this function calculates a pseudo-random number from 0 to /n/ using a state
rand' :: Int -- ^ /n/
    -> State -- ^ the state
    -> Int -- ^ the pseudo-random number from 0 to /n/
rand' n state = hash' state `mod` n


-- | Given the coordinates of a ghost, and a map representing the distances between every two points
--   of the maze, returns which orientation should the ghost be in in order to escape the target most optimally, i.e, the furthest away from it
--   If two or more maximum distances are found, a pseudo-random one based on the hash of the state is returned.
--
-- == /Note/
--   This function is greedy. If caught in a dead end, the ghost will not try to escape
--   since that would cause a local decrease in the distance to the target.
--
getLongestDirection :: Coords  -- ^ The coordinates of the given ghost
    -> Memoization -- ^ The map representing the distances
    -> Coords -- ^ The target the ghost is trying to escape
    -> State -- ^ The state
    -> Orientation -- ^ The optimal orientation for the ghost, i.e, the direction which escapes the pacman most optimally
getLongestDirection c mem target st@(State _ players _)
    | max == -1 = Null
    | length indices == 1 = orientationsList !! (head indices)
    | otherwise = orientationsList !! (indices !! (rand' (length indices) st))
    where neightbours = getNeighbourDistances c mem target players
          max = maximum neightbours
          indices = filterIndices 0 (== max) neightbours
          

-- | Given the coordinates of a ghost, a target, and a map representing the distances between every two points
--   of the maze, returns which orientation should the ghost be in in order to get to the target in the shortest possible distance.
--   If two or more minimum distances are found, a pseudo-random one based on the hash of the state is returned.
--
getShortestDirection :: Coords -- ^ The coordinates of the given ghost
    -> Memoization -- ^ The map representing the distances
    -> Coords -- ^ The coordinates of the target
    -> State -- ^ The state
    -> Orientation -- ^ The optimal orientation for the ghost, i.e, the direction which leads to the pacman the quickest
getShortestDirection c mem target st@(State _ players _)
    | length filtered == 0 = Null
    | length filtered == 1 = orientationsList !! (head indices)
    | otherwise = orientationsList !! (indices !! (mod (hash' st) (length indices)))
    where neightbours = getNeighbourDistances c mem target players
          filtered = filter (/= (-1)) neightbours
          min = minimum $ filtered
          indices = filterIndices 0 (== min) neightbours


-- | For a certain player, returns the position he would land in
--   were he to take the specified number of steps
--   (assuming collisions with walls)
getAdvancedTarget :: Int -- ^ the specified number of moves
    -> Maze -- ^ the maze where the player moves
    -> Player -- ^ the player
    -> Coords -- ^ the coordinates where he lands
getAdvancedTarget 0 _ pacman = getPlayerCoords pacman
getAdvancedTarget n maze pacman
    | (isSquareClear nc maze) = (getAdvancedTarget (n - 1) maze (setPlayerCoordinates nc pacman))
    | otherwise = getPlayerCoords pacman
    where nc = pseudoMove (getSize maze) (getPlayerOrientation pacman) (getPlayerCoords pacman)


-- | Given a state, and the id of an 'Alive' ghost, returns the its move
--
-- == /Note/
-- It is not defined for 'Dead' ghosts
--
chaseMode :: State -- ^ The given state
    -> Int -- ^ The ghost's id
    -> Memoization -- ^ The 'Memoization' object
    -> Play -- ^ The ghost's move
chaseMode s@(State mz players _) id  mem
    | getGhostType players id == 1 = Move id (getShortestDirection coords mem pacCoords s) 
    | distance mem pacCoords coords <= 4 = Move id (getShortestDirection coords mem pacCoords s) 
    | otherwise = Move id (getShortestDirection coords mem (getAdvancedTarget 4 mz (getPacman players)) s)
    where (Ghost (GhoState (_, coords@(y, x),_,_,_,_) Alive)) = getPlayer players id
          pacCoords = getPacmanLocation players


-- | Given a state, and the id of a 'Dead' ghost, returns the its move
--
-- == /Note/
-- It is not defined for 'Alive' ghosts
--
scatterMode :: State -- ^ The given state
    -> Int -- ^ The ghost's id
    -> Memoization -- ^ The 'Memoization' object
    -> Play -- ^ The ghost's move
scatterMode s@(State mz players _) id mem
    = Move id (getLongestDirection coords mem pacCoords s)
      where (Ghost (GhoState (_, coords,_,_,_,_) Dead)) = getPlayer players id
            pacCoords = getPacmanLocation players


-- | Returns the state were the players to move in the specified directions
newst :: [Play] -- ^ The list of moves
    -> State -- ^ The original state
    -> State -- ^ The resulting state
newst plays (State mz players lvl) = State mz (zipWith zipper players (plays ++ (repeat (Move (-1) Null)))) lvl
        where zipper player@(Ghost _) (Move _ orient) = setPlayerCoords player (pseudoMove (getSize mz) orient (getPlayerCoords player))
              zipper player@(Pacman _) (Move _ orient) = let c = pseudoMove (getSize mz) orient (getPlayerCoords player)
                                                         in if (isSquareClear c mz)
                                                            then setPlayerCoords player c
                                                            else player

-- | For the given player, chooses the mode they are to move in and stores their move.
--   The moves of previous players are taken into account to decide the moves of the current player.
--
chooseMode :: State -- ^ The original state
    -> Memoization -- ^ The 'Memoization' object
    -> [Play] -- ^ The list of moves already calculated
    -> Player -- ^ The given player
    -> [Play] -- ^ The resulting list of moves
chooseMode st memo plays (Ghost (GhoState (id,_,_,_,_,_) Alive)) = plays ++ [chaseMode (newst plays st) id memo]
chooseMode st memo plays (Ghost (GhoState (id,_,_,_,_,_) Dead))  = plays ++ [scatterMode (newst plays st) id memo]
chooseMode _ _ plays pacman = plays ++ [(Move (-1) (getPlayerOrientation pacman))]


-- | Given a state, and a 'Memoization', returns the list of moves for each ghost,
--   in the order they appear in the players list
--
ghostPlay' :: State -- ^ The given state
    -> Memoization -- ^ The 'Memoization' object
    -> [Play] -- ^ The resulting list of moves
ghostPlay' st@(State mz players lvl) memo = filter (\(Move n _) -> n >= 0) (foldl (chooseMode st memo) [] players)


-- | Given a state, returns the list of moves for each ghost,
--   in the order they appear in the players list
ghostPlay :: State -- ^ The given state
    -> [Play] -- ^ The resulting list of moves
ghostPlay st = ghostPlay' st (getMemory (maze st))