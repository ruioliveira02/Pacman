{- |
    Module: InstructionsMenu

    Module which draws the instructions menu, as well as handles user input 
    in said screen.

    It should be noted this menu is nothing more than just one image which 
    is being showed on screen.
-}


module InstructionsMenu where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG
import GlossTypes


-- | Given a set of pictures, draws the picture corresponding to the
--   instruction menu
drawInstructionsMenu :: PictureSet -- ^ The given set of pictures
    -> Picture -- ^ The picture corresponding to the instruction menu
drawInstructionsMenu pics = instructions pics






-- | Given an event (key press) and a state, returns the state after the event is processed.
--   In this case, any key press makes the game return to the main menu.
--
reactEventInst:: Event -- ^ The given event
      -> GlossState -- ^ The given state
      -> GlossState -- ^ The resulting state
reactEventInst (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ bt man pics map)
    = (GlossState MainMenu 0 man pics map)
reactEventInst _ st
    = st