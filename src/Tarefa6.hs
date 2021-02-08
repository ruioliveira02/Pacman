{-| 
    Module: Tarefa6

    Module responsible for the behaviour of the Pacman bot.
-}

module Tarefa6 where 

import Types hiding (Empty)
import PacmanUtils
import Tarefa2
import Tarefa5
import Tarefa4

import LazyBFS
import FileUtils

import Data.Hashable
import qualified Data.List as DL
import qualified Data.Map as Map

-- | Represents all the values already calculated by the program
type DPMap = Map.Map Int (Orientation, Int)

-- | The default search depth
--
defaultDepth :: Int
defaultDepth = 5

-- | Calculates the hash of a tuple (State, Int) by casting it
--   to a String and then calculating its hash (Data.Hashable)
hash'' :: State -- ^ the state
    -> Int -- ^ the step
    -> Int -- ^ the hash
hash'' state step = hash $ show'' state step

-- | Casts a tuple (State, Int) to a String. Used by hash''
show'' :: State -- ^ the state 
    -> Int -- ^ the step
    -> String -- ^ the resulting string
show'' (State _ players _) step = show step ++ concat (map (\x -> show (getPlayerCoords x)  ++ show (getPlayerPoints x) ++ "|" ++ show (getLives x)) players)

-- | Given /n/, this function calculates a pseudo-random number from 0 to /n/ using a step and a state
rand'' :: Int -- ^ /n/
    -> Int -- ^ the step
    -> State -- ^ the state
    -> Int -- ^ the pseudo-random number from 0 to /n/
rand'' n step state = hash'' state step `mod` n


-- | The evaluation of any given state of the game.
--
--   This is the target function the bot tries to maximize.
evaluate :: Memoization
      -> State -- ^ The given state
      -> Player -- ^ Pacman
      -> Int -- ^ The number of steps since the begining of the game
      -> Int -- ^ The evaluation of the state
evaluate mem (State mz pl _) pacman step            
    = 1000000 * points + 100000000 * (lives - dead) - 1000 * distance mem coords c + 100 * distanceToAllGhosts
      where (Pacman (PacState (_, coords, _, _, points, lives) _ _ mode)) = pacman
            dead | mode == Dying = 1
                 | otherwise = 0
            Just c = firstFood mz
            distanceToAllGhosts = sum [distance mem coords (getPlayerCoords x) | x <- pl]

-- | Given a list of @[(Orientation, Int)]@ returns the elements which have the biggest @Int@ component.
--
maxValue' :: [(Orientation, Int)] -- ^ The given list
      -> [(Orientation, Int)] -- ^ The elements with the largest @Int@ component
maxValue' list = filter (\x -> snd x == max) list
            where max = maximum (map snd list)


-- | Given a function (always used with 'testOrientation'), and a list of parameters, and one original
--   accumulator, returns the output of the function (as a list of results) as well as the accumulator
--   resulting from running the function through all said arguments
--
iterateOverList :: (a -> c -> (b,c)) -- ^ The given function
      -> c  -- ^ The original accumulator
      -> [a] -- ^ The list of arguments
      -> ([b],c) -- ^ The output of the functions: a list consisting of the first component of each function 
      -- call, as well as the accumulator returned by the final call.
iterateOverList f t [] = ([],t)
iterateOverList f tree (x:xs) = (val : a, b)
                              where (val, newTree) = (f x tree)
                                    (a,b)           = iterateOverList f newTree xs


-- | Given a certain state, runs through every possible move for Pacman (left, right, down and up), and returns
--   not only the optimal orientation of the move, as well as the evaluation of the optimal state that can be reached,
--   and the map used for dynamic programming purposes.
--
search :: Int -- ^ The given step
      -> State -- ^ The given state
      -> Memoization -- ^ The given Memoization table
      -> [Int] -- ^ The hashes of the previously searched states
      -> Int -- ^ The search depth
      -> DPMap -- ^ The map used for dynamic programming
      -> (Orientation, Int, DPMap) -- ^ The optimal orientation and evaluation, alongside the updated DP map
search step state _ _ _ tree | isGameOver state = (Null, maxValue - step, tree)
search step state memo prev 0 tree = (Null, eval, Map.insert (head prev) (Null,eval) tree)
                                where eval = evaluate memo state (getPacman $ playersState state) step
search step state@(State _ pl _) memo prev depth tree
    = case (Map.lookup (head prev) tree) of
        Nothing -> (o, i, Map.insert (hash'' state step) (o, i) newTree)
        Just (or, i') -> (or, i', tree)
    where pacman = getPacman pl
          (o, i) | length list' == 0 = (Null, -1000000000)
                 | otherwise = list' !! (rand'' (length list') step state)
          list' = maxValue' list
          (list, newTree) = iterateOverList aux tree
            $ filter (\(_, _, h') -> notElem h' prev)
            $ map (\(a, b) -> (a, b, hash'' b (step + 1)))
            $ map (\x -> (x, (passTime' step (updatePlayer (rotatePlayerTo x pacman) state) memo)))
                [U, D, L, R]
          aux (or', s, h) t = ((or', snd), thrd)
              where (_, snd, thrd) = search (step + 1) s memo (h : prev) (depth - 1) t


-- | Given a step and step, as well as 'Memoization', returns the
--   best move for pacman to play
--
bot' :: Int -- ^ The given step
      -> State -- ^ The given state
      -> Memoization -- ^ The given 'Memoization' object
      -> Maybe Play -- ^ The optimal move
bot' step state memo | orientation /= Null = Just (Move id orientation)
                    | otherwise = Nothing
               where (orientation,_,b) = (search step state memo [hash'' state step] defaultDepth Map.empty)
                     pacman@(Pacman (PacState (id,_,_,_,_,_) _ _ _)) = (getPacman (playersState state))



-- | Given a step and step,  returns the
--   best move for pacman to play
--
bot :: Int -- ^ The given step
      -> State -- ^ The given state
      -> Maybe Play -- ^ The optimal move
bot step state = bot' step state (getMemory (maze state))