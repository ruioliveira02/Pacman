{-| 
    Module: LazyBFS

    Module used for calculating the distances between squares of the maze. Used by "Tarefa5"
    to compute the moves the ghosts should make.

-}


module LazyBFS where

import Types
import PacmanUtils
import Tarefa2
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | The maximum value of an integer (is returned when there is no path from A to B)
maxValue :: Int
maxValue = 2147483647

-- | Stores the distance from every point to every other point
type Memoization = Map.Map Coords ([Set.Set Coords], Map.Map Coords (Maybe Int))

-- | Given a Memoization, extracts from it the distance between 2 points
distance :: Memoization -- ^ the given mamoization
    -> Coords -- ^ the first point
    -> Coords -- ^ the second point
    -> Int -- ^ the distance between them, or maxValue if there is no path from one to the other
distance mem c1 c2 | c1 > c2 = distance mem c2 c1
                   | otherwise = case snd (mem Map.! c1) Map.! c2 of
                        Just d -> d
                        Nothing -> maxValue --Nao existe um caminho de c1 para c2 ou c1 ou c2 é uma parede

-- | Given a Memoization, returns the size of the original maze
mazeSize :: Memoization -- ^ the given Mamoization object
    -> Coords -- ^ the size of the original maze (height, width)
mazeSize mem = (y + 1, x + 1)
                where ((y,x),_) = Map.findMax mem

-- | Given a maze, creates a Memoization object storing the distance between every 2 points
getMemory :: Maze -- ^ the given maze
    -> Memoization -- ^ the resulting Memoization object
getMemory mz = Map.mapWithKey (mapper mz) (Map.fromAscList [((a,b),([],Map.empty))
                                                            | a <- [0..(h-1)],
                                                              b <- [0..(w-1)]])
            where (h,w) = getSize mz

-------------------------INTERNAL FUNCIONS-------------------------

-- | Populates an empty entry with the intended data structure using the key
mapper :: Maze -- ^ the maze
    -> Coords -- ^ the key
    -> a -- ^ the original value (unused)
    -> ([Set.Set Coords], Map.Map Coords (Maybe Int)) -- ^ the resulting data structure
mapper mz c1 _ = (atdists, Map.mapWithKey mapper2 (coords mz c1))
                where atdists = atDists mz c1                          --find index of the set
                      mapper2 c2 _ = findIndex (Set.member c2) atdists --that contains wanted point


-- | Returns a Map containing every point bigger than the given point.
--   Used to ensure no 2 pairs of points in the final map are equivalent.
coords :: Maze -- ^ the original maze (used for size)
    -> Coords -- ^ the given point
    -> Map.Map Coords (Maybe Int) -- ^ The map of points
coords mz (a, b) = Map.fromAscList [((c,d), Nothing)
                    | c <- [a..(h - 1)],
                      d <- [(if (a == c) then b else 0)..(w - 1)]]
                    where (h, w) = getSize mz


-- | Given a maze and a source point, returns a list of Sets, where the /n/-th Set
--   contains all the points of the maze distanced /n/ from the source
atDists :: Maze -- ^ the given maze
    -> Coords -- ^ the source point
    -> [Set.Set Coords] -- ^ the list of sets
atDists mz c | not $ isSquareClear c mz = []
             | otherwise = a : b : (atDists' mz c a b)
                where a = Set.singleton c
                      b = Set.fromList $ filter (/= c) (getNeighbours mz c)


-- | Auxiliar function to atDists. Recursively creates the list of Sets from the 2 previous sets
atDists' :: Maze -- ^ the given maze
    -> Coords -- ^ the source point
    -> Set.Set Coords -- ^ the set before the previous
    -> Set.Set Coords -- ^ the previous set
    -> [Set.Set Coords] -- ^ the list of sets
atDists' mz coord a b | null c = []
                      | otherwise = c : (atDists' mz coord b c)
    where c = Set.unions $ Set.map (Set.fromList
                                    . (filter (\x -> Set.notMember x a   --checks if point hasn't
                                                  && Set.notMember x b)) --been included already
                                    . getNeighbours mz) b


-- | Given a maze and a source point, returns the adjacent points to the source
--   that are clear (for definition of "clear", check PacmanUtils.isSquareClear)
getNeighbours :: Maze -- ^ the given maze
    -> Coords -- ^ the source point
    -> [Coords] -- ^ the adjacent points
getNeighbours mz c = filter (flip isSquareClear mz)
                        $ map ((flip (pseudoMove (getSize mz))) c)
                        [U, D, L, R]
