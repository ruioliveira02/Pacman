{- |
    Module: MazeMenu

    Module responsible for handling the UI of the Maze Menu and the corresponding inputs.
-}


module MazeMenu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG

import Types
import PacmanUtils
import GlossTypes

import Game

-- | Returns the Picture used to render the Maze Menu
drawMazeMenu :: Int -- ^ The maze pressed
    -> PictureSet -- ^ The PictureSet of the application
    -> Picture -- ^ The resulting picture
drawMazeMenu mz pics = Pictures [Translate 0.0 (-50.0) (snd ((mazes pics) !! mz)),
                            Translate 0.0 (375.0) (mazeTitle pics)]


-- | Given an event (key press) and a state, returns the state after the event is processed
--
reactEventMazeMenu :: Event -- ^ The given event
    -> GlossState -- ^ The current state
    -> GlossState -- ^ The resulting state
reactEventMazeMenu (EventKey (SpecialKey KeyLeft)    Down _ _) (GlossState sc bt man p map) 
    = (GlossState sc ((bt - 1 + l) `mod` l) man p map)
                                                where l = length (mazes p)
reactEventMazeMenu (EventKey (SpecialKey KeyRight)    Down _ _) (GlossState sc bt man p map) 
    = (GlossState sc ((bt + 1) `mod` l) man p map)
                                                where l = length (mazes p)
reactEventMazeMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ bt man p _) 
    = (GlossState PlayMenu 0 newMan p map)
        where newMan = loadManager map (level (state man)) (pid man) lives score
              map = "../maps/" ++ (fst ((mazes p) !! bt)) ++ ".txt"
              (Pacman (PacState(_,_,_,_,score,lives) _ _ _)) = getPacman (playersState (state man))
reactEventMazeMenu (EventKey (SpecialKey KeyEsc)    Down _ _) (GlossState _ _ man p map) 
    = (GlossState PlayMenu 0 man p map)
reactEventMazeMenu _ st = st