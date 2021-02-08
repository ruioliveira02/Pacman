{- |
    Module: PlayMenu

    Module responsible for handling the UI of the Play Menu and the corresponding inputs.
-}


module PlayMenu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG

import Types
import PacmanUtils
import GlossTypes

-- | Returns the Picture used to render the Play Menu
drawPlayMenu :: PictureSet -- ^ The given 'PictureSet'
    -> Int -- ^ The button pressed
    -> Picture -- ^ The resulting picture
drawPlayMenu pics bt = Pictures [pacmanButton pics (bt == 0), ghostButton pics (bt == 1),
                                     backButton pics (bt == 2), title pics]


-- | the title of the page
title :: PictureSet -- ^ The given 'PictureSet'
    -> Picture -- ^ The resulting 'Picture'
title pics = Scale 0.9 0.9 (Translate 0.0 425.0 ((playMenu pics) !! 0))

-- | The top button (the one to choose to play as pacman)
pacmanButton :: PictureSet -- ^ The given 'PictureSet'
    -> Bool -- ^ Whether or not the button is selected
    -> Picture -- ^ The picture corresponding to the button
pacmanButton pics False = Translate (-500.0) (-15.0) ((playMenu pics) !! 1)
pacmanButton pics True  = Translate (-500.0) (-15.0) ((playMenu pics) !! 2)


-- | The middle button (the one to choose to play as the ghosts)
ghostButton :: PictureSet -- ^ The given 'PictureSet'
    -> Bool -- ^ Whether or not the button is selected
    -> Picture -- ^ The picture corresponding to the button
ghostButton pics False = Translate (500.0) (-15.0) ((playMenu pics) !! 3)
ghostButton pics True  = Translate (500.0) (-15.0) ((playMenu pics) !! 4)


-- | The lowest button (to go back in the menus)
backButton :: PictureSet -- ^ The given 'PictureSet'
    -> Bool -- ^ Whether or not the button is selected
    -> Picture -- ^ The picture corresponding to the button
backButton pics False = Translate (0.0) (-430.0) ((playMenu pics) !! 5)
backButton pics True = Translate (0.0) (-430.0) ((playMenu pics) !! 6)



choosePacman :: Manager -- ^ The given manager
    -> Manager -- ^ The resulting manager
choosePacman (Manager st memo pid step bef delt del p) = (Manager st memo newPid step bef delt del p)
                                                    where newPid = getPlayerID $ getPacman (playersState st)


chooseGhost :: Manager -- ^ The given manager
    -> Manager -- ^ The resulting manager
chooseGhost (Manager st memo pid step bef delt del p) = (Manager st memo newPid step bef delt del p)
                                                    where newPid = getFirstGhost (playersState st)
                                                          getFirstGhost :: [Player] -> Int
                                                          getFirstGhost [] = -1
                                                          getFirstGhost ((Ghost (GhoState (id,_,_,_,_,_) _)):_) = id
                                                          getFirstGhost (h:t) = getFirstGhost t



-- | Given an event (key press) and a state, returns the state after the event is processed
--
reactEventPlayMenu :: Event -- ^ The given event
    -> GlossState -- ^ The current state
    -> GlossState -- ^ The resulting state
reactEventPlayMenu (EventKey (SpecialKey KeyLeft)    Down _ _) (GlossState sc bt man pics map) 
    = (GlossState sc ((bt + 2) `mod` 3) man pics map)
reactEventPlayMenu (EventKey (SpecialKey KeyRight)    Down _ _) (GlossState sc bt man pics map) 
    = (GlossState sc ((bt + 1) `mod` 3) man pics map)
reactEventPlayMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ 0 man pics map) 
    = (GlossState Game 0 (choosePacman man) pics map)
reactEventPlayMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ 1 man pics map) 
    = (GlossState Game 0 (chooseGhost man) pics map)
reactEventPlayMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ 2 man pics map) 
    = (GlossState MainMenu 0 man pics map)
reactEventPlayMenu _ st = st