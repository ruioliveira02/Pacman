{- |
    Module: GlossTypes

    Module which contains all the type definitions which are used exclusively
    for rendering purposes inside Gloss.

    == Note

    It is assumed that Gloss is running on a 1920x1080 screen
-}


module GlossTypes where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG

import Types
import LazyBFS
import PacmanUtils
import FileUtils

import Tarefa1
import Tarefa2
import System.IO.Unsafe
import Data.Time.Clock.POSIX

-- | The current screen the application is on
data Screen = MainMenu -- ^ The main menu (first screen)
            | PlayMenu -- ^ The menu to select character
            | MazeMenu -- ^ The menu to choose the maze
            | InstructionsMenu -- ^ The menu with the instructions
            | Game -- ^ The actual game
            | LevelUp Float -- ^ The screen indicating the Pacman passed the level, alongside
                            --   the time this screen has to be shown (in ms)



-- | The state of the application, i.e., all the information required to render to the screen
--,for example, the current screen (menu, gameplay, .etc), the state of the game, .etc
--
data GlossState = GlossState
    {
        screen         :: Screen, -- ^ The current screen of the application
        selectedButton :: Int, -- ^ The index of the selected button
        manager        :: Manager, -- ^ The game manager
        picSet         :: PictureSet, -- ^ The set of pictures used for rendering
        map            :: String -- ^ The name of the loaded maze
    }

-- | The set of pictures the program needs to display everything. At the moment, only player images 
-- are loaded.
data PictureSet = PicSet
    {
        wall         :: Picture, -- ^ Picture of a wall
        fdlittle     :: Picture, -- ^ Picture of a little food
        fdbig        :: Picture, -- ^ Picture of a big food
        pacOpen      :: Picture, -- ^ Picture of pacman with the mouth open
        pacClosed    :: Picture, -- ^ Picture of pacman with the mouth closed
        ghostAlive   :: ([Picture], [Picture], [Picture]), -- ^ Pictures of the alive ghosts (red, pink, green)
        ghostDead    :: ([Picture],[Picture]), -- ^ Pictures of the dead ghosts (dark blue, light blue)
        mazes        :: [(String, Picture)], -- ^ The name of the mazes, and the respective picture to be shown
        playMenu     :: [Picture], -- ^ Pictures for the 'PlayMenu'
        --[Character,Pacman (Not selected (NS)/Selected(S)),Ghost (NS/S),BackButton (NS/S)]
        mazeTitle    :: Picture, -- ^ 'MAPA'
        gameOver     :: Picture, -- ^ Picture for showing the game is over
        mainMenu     :: [Picture], -- ^ Pictures of the main menu
        -- [Title, Play, Play Selected, Inst., Inst. Selected, Lower Pic]
        pausedPic    :: Picture, -- ^ Picture for showing the game is paused
        levelUp      :: Picture, -- ^ Picture used in the level up screen
        instructions :: Picture -- ^ Picture corresponding to the instruction screen
    }

-- | Represents the state of the game, including player positions, the player controlled by the user,
--   whether the game is paused or not, .etc
data Manager = Manager 
    {    
        state  :: State -- ^ The state of the game
    ,   memory :: Memoization -- ^ The memoization object
    ,   pid    :: Int -- ^ The id of the controlled player
    ,   step   :: Int -- ^ The current step
    ,   before :: Integer -- ^ Time of the last step
    ,   delta  :: Integer -- ^ time elapsed since last step
    ,   delay  :: Float -- ^ Delay between steps
    ,   paused :: Bool -- ^ Whether the game is paused or not
    } 


-- | Default width of the window (1920px)
--
windowWidth :: Int
windowWidth = 1920

-- | Default height of the window (1080px)
windowHeight :: Int
windowHeight = 1080


-- | Auxiliary function which converts a double to a float
--
-- == /Note regarding precision lost/
--
-- Despite @Double@ supporting twice as many decimal places as @Float@,
-- for the purposes this function is used for, that loss is irrelevant.
--
doubleToFloat :: Double -- ^ The given number (as a @Double@)
    -> Float -- ^ The given number (as a @Float@)
doubleToFloat d = read (show d) :: Float



-- | Loads the manager for the current level
--
loadManager :: String -- ^ The file with the maze
    -> Int -- ^ The level
    -> Int -- ^ The player's id
    -> Int -- ^ Pacman's lives
    -> Int -- ^ Pacman's score
    -> Manager -- ^ The resulting manager
loadManager str lvl pid lives score = ( Manager state memo pid 0 0 0 (fromInteger defaultDelayTime) False)
            where state | str == "../maps/Random.txt" = spawnGhosts lvl 1 (State (generateMaze 20 20 seed) pl lvl)
                        | otherwise                   = spawnGhosts lvl 1 (State (maze (loadMaze str)) pl lvl)
                  memo  = getMemory (maze state)
                  seed  = unsafePerformIO $ (fmap ( round . (* 1000) ) getPOSIXTime)
                  pl    = [Pacman (PacState (0,(getPacmanSpawnCoords state),1.0,R,score,lives) 0.0 Open Normal)]


-- | The default Manager for the game
defaultManager :: Manager
defaultManager = (loadManager "../maps/Random.txt" 1 0 3 0)


-- | Given a number of ghosts, the id of the first one and a 'State', returns the state
--   which results from spawning in the given number of ghosts (up to a maximum of 4)
spawnGhosts :: Int -- ^ The number of ghosts to spawn
    -> Int -- ^ Id of the first ghost
    -> State -- ^ The given state
    -> State -- ^ The resulting state
spawnGhosts 0 _ st = st
spawnGhosts n id st | n > 4 = spawnGhosts 4 id st
spawnGhosts n id st@(State mz pl lvl) = spawnGhosts (n - 1) (id+1) (State mz newPl lvl)
                                     where newPl = pl ++ [Ghost (GhoState (id, getGhostSpawnCoords st, 1.0,U,0,0) Alive)]