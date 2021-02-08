{-| 
    Module: Tarefa4

    Module responsible for handling the passing of time in the game.
-}



module Tarefa4 where 

import Types
import PacmanUtils
import LazyBFS
import Tarefa2
import Tarefa5

-- | Returns whether a given number is an integer or not
--
-- == /Example/
-- >>> isInteger 2.0
-- >>> True
--
-- >>> isInteger 2.5
-- >>> False
isInteger :: Double -- ^ The given number
    -> Bool -- ^ Whether the given number is an integer or not
isInteger d = d == fromInteger (round d)

-- | Returns whether a given number is the inverse of any integer, i.e., if it is of the form
-- @1/d@, where d is an integer
--
-- == /Example/
-- >>> isInverse 0.25
-- >>> True
--
-- >>> isInverse 2
-- >>> False
isInverse :: Double -- ^ The given number
    -> Bool -- ^ Whether the given number is an inverse or not
isInverse d = isInteger (1 / d)


-- | Auxiliary function to @applyMoves@. Given a state, and a list of moves for the player, alongside the number
-- of times each move is to be executed, returns the state which results from applying each move exactly once,
-- alongside wih the remaining moves that still need applying.
--
-- In other words, given a set of moves, each one having to be done a certain amount of times, plays each move exactly
-- once, and returns the resulting state and the remaining set of moves.
--
-- It uses an accumulating parameter to store the remaining set of moves
--
-- == /Example/
--
-- >>> applyMovesAux (loadMaze "../maps/5.txt") [(2, Move 0 R), (1, Move 1 D)] []
-- >>> (####################
--      #. <........#..... #
--      #...##..#.#.#.....M#
--      #.......#...#...## #
--      #.#......#.#. ....M#
--      #....#.......#.#...#
--      #............#.#...#
--      ##o..          ....#
--      #.### ###  ### .#..#
--       .... #     M# .... 
--       #... ######## ##.. 
--      #...#          ....#
--      #..#...#..#........#
--      #.....##....#....#.#
--      #....o...........###
--      #...#....#.#.......#
--      #.##....#.......#..#
--      #...........#......#
--      ##....#............#
--      ####################
--      Level: 1
--      Players: 
--      ID:3 Points:0 Lives:1
--      ID:2 Points:0 Lives:1
--      ID:1 Points:0 Lives:1
--      ID:0 Points:0 Lives:1
--      ,[(0,Move 1 D),(1,Move 0 R)]) @

applyMovesAux :: State -- ^ The given state
    -> [(Int, Play)] -- ^ The set of moves to be played, and the number of times each one is to be played
    -> [(Int, Play)] -- ^ The accumulating parameter, corresponding to the remaining set of moves
    -> (State, [(Int, Play)]) -- ^ The @State@ resulting from applying each move once, and the remaining set of moves
applyMovesAux state [] list = (state, list)
applyMovesAux state ((0, move):t) lista = applyMovesAux state t lista
applyMovesAux state@(State mz pl lvl) ((x, move@(Move id orientation)):t) lista
    = applyMovesAux (play move (updatePlayer (rotatePlayerTo orientation (getPlayer pl id)) state)) t ((x - 1, move):lista)


-- | Given a state and a set of moves, as well as the number of times each one is to be played,
-- returns the state which results from applying all the moves.
--
-- == /Note/
-- The players move one square at a time, meaning that, should one player move twice, and another only once,
-- firstly both players move once, and only then does the first player move again.
--
-- == /Example/
--
-- >>> applyMoves (loadMaze "../maps/5.txt") [(2, Move 0 R), (1, Move 1 D)]
-- >>> ####################
--     #.  {.......#..... #
--     #...##..#.#.#.....M#
--     #.......#...#...## #
--     #.#......#.#. ....M#
--     #....#.......#.#...#
--     #............#.#...#
--     ##o..          ....#
--     #.### ###  ### .#..#
--      .... #     M# .... 
--      #... ######## ##.. 
--     #...#          ....#
--     #..#...#..#........#
--     #.....##....#....#.#
--     #....o...........###
--     #...#....#.#.......#
--     #.##....#.......#..#
--     #...........#......#
--     ##....#............#
--     ####################
--     Level: 1
--     Players: 
--     ID:3 Points:0 Lives:1
--     ID:2 Points:0 Lives:1
--     ID:1 Points:0 Lives:1
--     ID:0 Points:1 Lives:1

applyMoves :: State -- ^ The given state
    -> [(Int, Play)] -- ^ The set of moves, alongside the number of times each one is to be played
    -> State -- ^ The resulting state
applyMoves state moves = case (applyMovesAux state moves []) of
                                (newState, [])       -> newState
                                (newState, newMoves) -> applyMoves newState newMoves

-- | Given the current step and the player's speed, returns the number of times the player should move
--
-- == /Examples/
-- >>> numberOfMoves 1 1
-- >>> 1
-- >>> numberOfMoves 1 0.5
-- >>> 0
-- >>> numberOfMoves 1 2
-- >>> 2
-- >>> numberOfMoves 4 0.5
-- >>> 1
numberOfMoves :: Int -> Double -> Int
numberOfMoves step speed | isInteger speed = (round speed)
                         | isInverse speed = if (step `mod` (round (1 / speed)) == 0) then 1 else 0
                         | otherwise       = 1


-- | Given the current step and a player, returns its move ('Play', based on their orientation), 
--   as well as the number of times it is to be executed
--
getMove :: Int  -- ^ The given step
    -> Player -- ^ The given player
    -> (Int, Play) -- ^ The player's move and how often they should move
getMove step (Pacman (PacState (id,_,speed,or,_,_) _ _ _)) = ((numberOfMoves step speed), Move id or)
getMove step (Ghost (GhoState (id,_,speed,or,_,_) _))      = ( (numberOfMoves step speed), Move id or)


-- | Given the current state and step, returns the state which resulting from the passing of time.
--   This function moves the players in the direction they are facing.
--
passTimeAux :: Int -- ^ The current step
    -> State -- ^ The original state
    -> State -- ^ The resulting state
passTimeAux step st@(State maze players level) = applyMoves st (map (getMove step) players)



-- | Given the current state, step and a 'Memoization', returns the state which resulting from the passing of time
--   , as well as having in mind the ghosts movements.
passTime' :: Int -- ^ The current step
    -> State -- ^ The current state
    -> Memoization -- ^ The 'Memoization' of the state (cf. "Tarefa5")
     -> State -- ^ The resulting state
passTime' step state memo = passTimeAux step (rotateGhosts state (ghostPlay' state memo))


-- | Given the current state and step, returns the state which resulting from the passing of time
--   , as well as having in mind the ghosts movements.
passTime :: Int -- ^ The current step
    -> State -- ^ The current state
    -> State -- ^ The resulting state
passTime step state = passTime' step state (getMemory (maze state))


-- | Given a state and a list of moves, returns the state resulting from rotating all the ghosts
--   in the map.
--
-- == /Note/
--
--   This function assumes that the list of moves only refers to ghosts.
rotateGhosts :: State -- ^ The given state
    -> [Play] -- ^ The list of moves
    -> State -- ^ The resulting state
rotateGhosts st [] = st
rotateGhosts s@(State mz pls lvl) ((Move id or):ps) = rotateGhosts (updatePlayer (rotatePlayerTo or pl) s) ps
                                  where pl = getPlayer pls id