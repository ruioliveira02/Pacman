{- |
    Module: MainMenu

    Module responsible for handling the UI of the Main Menu and the corresponding inputs.
-}


module MainMenu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG

import GlossTypes

-- | Returns the Picture used to render the Main Menu
drawMainMenu :: Int -- ^ The button pressed
    -> PictureSet -- ^ The given set of pictures
    -> Picture -- ^ The resulting picture
drawMainMenu bt pics = Pictures [playButton pics (bt == 0), instructionsButton pics (bt == 1),
                                title pics, finalPic pics]


-- | Returns the Picture used to render the title of the menu
title :: PictureSet -- ^ The given set of pictures
    -> Picture -- ^ The picture corresponding to the title
title pics = Translate 0.0 350.0 (Scale 1.2 1.2 ((mainMenu pics) !! 0))


-- | Returns the Picture used to render the bottom image with the Pacman and the ghosts
finalPic :: PictureSet -- ^ The given set of pictures
    -> Picture -- ^ The picture corresponding to the title
finalPic pics = Translate 0.0 (-350.0) (Scale 0.7 0.7 ((mainMenu pics) !! 5))

-- | The top button (the one to choose to play)
playButton :: PictureSet -- ^ The given set of pictures
    -> Bool -- ^ Whether or not the button is selected
    -> Picture -- ^ The picture corresponding to the button
playButton pics False = Translate 0.0 (100.0) ((mainMenu pics) !! 1)
playButton pics True  = Translate 0.0 (100.0) ((mainMenu pics) !! 2)


-- | The lower button (the one to read the instructions)
instructionsButton :: PictureSet -- ^ The given set of pictures
    -> Bool -- ^ Whether or not the button is selected
    -> Picture -- ^ The picture corresponding to the button
instructionsButton pics False = Translate 0.0 (-100.0) ((mainMenu pics) !! 3)
instructionsButton pics True  = Translate 0.0 (-100.0) ((mainMenu pics) !! 4)


-- | Given an event (key press) and a state, returns the state after the event is processed
--
reactEventMainMenu :: Event -- ^ The given event
    -> GlossState -- ^ The current state
    -> GlossState -- ^ The resulting state
reactEventMainMenu (EventKey (SpecialKey KeyUp)    Down _ _) (GlossState sc bt man p map)   
    = (GlossState sc ((bt + 1) `mod` 2) man p map)
reactEventMainMenu (EventKey (SpecialKey KeyDown)    Down _ _) (GlossState sc bt man p map) 
    = (GlossState sc ((bt - 1) `mod` 2) man p map)
reactEventMainMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ 0 man p map)  
    = (GlossState MazeMenu 0 man p map)
reactEventMainMenu (EventKey (SpecialKey KeyEnter)    Down _ _) (GlossState _ 1 man p map)  
    = (GlossState InstructionsMenu 0 man p map)
reactEventMainMenu _ st                                                                     
    = st