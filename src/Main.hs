{- |
    Module: Main

    Main module for the Gloss implementation of the game. It handles the window creation,
    redirects the drawing and reaction to events to the correct modules, and loads the 
    'PictureSet' used throughout the program's lifespan.


    == Usage of pictures

    Pictures were used, many times in place of text, due to the limitations of gloss when
    it comes to text rendering, namely custom fonts.
-}

module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG
import Graphics.Gloss.Juicy

import Types
import PacmanUtils
import Tarefa2
import Tarefa4 hiding (rotateGhosts)
import Tarefa5
import Tarefa6

import System.IO.Unsafe


import Data.Maybe

import FileUtils
import LazyBFS

import GlossTypes

import Game
import MainMenu
import PlayMenu
import MazeMenu
import InstructionsMenu

import System.Directory
import System.FilePath.Posix 
import Data.List as DL

-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------
--COMPILE WITH: ghc -hidir ../bin -odir ../bin -o ../bin/Pacman Main.hs--
-----------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------


-- | The default background color
--
background :: GG.Color
background = makeColor 0.05 0.05 0.05 1.0

-- | The default initial state for gloss rendering
--
initialState :: PictureSet -- ^ The given set of pictures
    -> GlossState -- ^ The resulting initial state
initialState pics = (GlossState MainMenu 0 defaultManager pics "")

-- | Given the set of pictures and the current state, returns the picture to be rendered on screen
--
drawState :: PictureSet -- ^ The given set of pictures
  -> GlossState -- ^ The current state
  -> Picture -- ^ The picture to be rendered on screen
drawState pics (GlossState (LevelUp _) _ _ _ _)        = drawLevelUp pics
drawState pics (GlossState Game bt estado _ _)         = drawGame pics estado
drawState pics (GlossState MainMenu bt _ _ _)          = drawMainMenu bt pics
drawState pics (GlossState InstructionsMenu bt _ _ _)  = drawInstructionsMenu pics
drawState pics (GlossState PlayMenu bt _ _ _)          = drawPlayMenu pics bt
drawState pics (GlossState MazeMenu bt _ _ _)          = drawMazeMenu bt pics



-- | Given an event (key press) and a state, returns the state after the event is processed
--
reactEvent :: Event -- ^ The given event
    -> GlossState -- ^ The current state
    -> GlossState -- ^ The resulting state
reactEvent key st@(GlossState Game _ _ _ _)             = reactEventGame key st
reactEvent key st@(GlossState MainMenu _ _ _ _)         = reactEventMainMenu key st
reactEvent key st@(GlossState InstructionsMenu _ _ _ _) = reactEventInst key st
reactEvent key st@(GlossState PlayMenu _ _ _ _)         = reactEventPlayMenu key st
reactEvent key st@(GlossState MazeMenu _ _ _ _)         = reactEventMazeMenu key st
reactEvent _ st                                         = st

-- | Given the time elapsed since the last frame was d Int -- ^ Pacman's lives
reactTime :: Float -- ^ The time elapsed since the last frame
    -> GlossState -- ^ The previous state
    -> GlossState -- ^ The current state
reactTime n gs@(GlossState Game _ (Manager _ _ _ _ _ _ _ False) _ _) = reactTimeGame n gs  
reactTime n (GlossState (LevelUp t) bt man pics map)
                    | t - n > 0 = (GlossState (LevelUp (t - n)) bt man pics map)
                    | otherwise = (GlossState Game bt (loadManager map (level (state man) + 1) 
                                    (pid man) lives score) pics map)
                    where (Pacman (PacState (_,_,_,_,score,lives) _ _ _)) 
                                = getPacman (playersState $ state man)
                          
reactTime _ gs = gs -- time does not pass if game is paused


-- | Default frame rate (60)
--
fr :: Int
fr = 60

-- | Default display
--
dm :: Display
dm = InWindow "Pacman Grupo 32" (windowWidth, windowHeight) (0, 0)


-- | Returns the pictures to be used for rendering of the alive ghosts.
--   
--   They are divided by color: first are the red ones, then the pink ones,
--   and finally the green ones.
--
loadGhostAlivePictures :: IO ([Picture], [Picture], [Picture]) -- ^ ([Red], [Pink], [Green])
loadGhostAlivePictures = do
    let list = ["../img/ghostAlive" ++ x ++ y ++ ".png" | x <- ["Red", "Pink", "Green"]
            ,y <- ["R","D","L","U"]]

    let res = DL.map (\x -> fromJust $ unsafePerformIO $ loadJuicy x) list

    let (first, t) = splitAt 4 res
    let (second, third) = splitAt 4 t

    return (first, second, third)


-- | Returns the pictures to be used for rendering of the dead ghosts.
-- 
--   They are divided by colors : first the dark blue ones, then the light blue ones.
--
loadGhostDeadPictures :: IO ([Picture], [Picture]) -- ^ ([Dark, Light])
loadGhostDeadPictures = do
    let list = ["../img/ghostDead" ++ x ++ y ++ ".png" | x <- ["", "Player"], y <- ["R","D","L","U"]]

    let res = DL.map (\x -> fromJust $ unsafePerformIO $ loadJuicy x) list

    return (splitAt 4 res)


-- | Loads the pictures to be displayed when choosing the given maze
loadMazePicture :: FilePath -- ^ The file containing the maze
    -> IO (Picture) -- ^ The resulting picture
loadMazePicture path = do
    pic         <- loadJuicy path
    (Just base) <- loadJuicy "../img/mazePics/noPreview.png"
    return (case pic of
                Nothing -> base
                Just p  -> p)

-- | Auxiliary function to 'loadMazePicture'
loadMazePicturesAux :: [FilePath] -- ^ The list of files containing mazes
    -> IO ([Picture]) -- ^ The resulting pictures
loadMazePicturesAux [] = return []
loadMazePicturesAux (h:t) = do
    pic       <- loadMazePicture ("../img/mazePics/"++ h ++ ".png")
    remaining <- loadMazePicturesAux t
    return (pic : remaining)


-- | Returns the pictures displayed when choosing a maze
loadMazePictures :: IO ([(String,Picture)])
loadMazePictures = do
    files' <- getDirectoryContents "../maps"
    
    let files = DL.sort ("Random" : (DL.map (takeBaseName) (filter (\x -> (isSuffixOf ".txt" x)) files')))

    pics <- loadMazePicturesAux files

    return ((zip files pics))

-- | Returns the set of pictures to be used throughout the execution of the program
--   (player images, for example).
--
loadPictureSet :: IO PictureSet
loadPictureSet = do
    Just wall      <- loadJuicy "../img/wall.png"
    Just fdlittle  <- loadJuicy "../img/foodLittle.png"
    Just fdbig     <- loadJuicy "../img/foodBig.png"
    Just pacOpen   <- loadJuicy "../img/pacOpen.png"
    Just pacClosed <- loadJuicy "../img/pacClosed.png"

    Just character      <- loadJuicy "../img/character.png"
    Just pacman         <- loadJuicy "../img/pacman.png"
    Just pacmanSelected <- loadJuicy "../img/pacmanSelected.png"
    Just ghost          <- loadJuicy "../img/ghost.png"
    Just ghostSelected  <- loadJuicy "../img/ghostSelected.png"
    Just back           <- loadJuicy "../img/back.png"
    Just backSelected   <- loadJuicy "../img/backSelected.png"

    Just mapa           <- loadJuicy "../img/mapa.png"
    Just gameOver       <- loadJuicy "../img/gameOver.png"

    ghostAlive     <- loadGhostAlivePictures
    ghostDead      <- loadGhostDeadPictures
    mazePics       <- loadMazePictures

    Just pacTitle     <- loadJuicy "../img/title.png"
    Just playBt       <- loadJuicy "../img/play.png"
    Just playSelected <- loadJuicy "../img/playSelected.png"
    Just inst         <- loadJuicy "../img/instructions.png"
    Just instSelected <- loadJuicy "../img/instructionsSelected.png"
    Just lowerPic     <- loadJuicy "../img/mainImage.png"

    Just paused       <- loadJuicy "../img/paused.png"
    Just levelUp      <- loadJuicy "../img/levelUp.png"
    Just instructions <- loadJuicy "../img/instructionsMenu.png"

    let pics = PicSet wall fdlittle fdbig pacOpen pacClosed ghostAlive ghostDead mazePics 
                    [character,pacman,pacmanSelected,ghost,ghostSelected, back, backSelected] 
                    mapa gameOver [pacTitle, playBt, playSelected, inst, instSelected, lowerPic]
                    paused levelUp instructions
    return pics


-- | Starting point of the application
--
main :: IO ()
main = do
    pics <- loadPictureSet

    GG.play dm background fr (initialState pics) (drawState pics) reactEvent reactTime
    return ()