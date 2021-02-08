{- |
    Module: Game

    Module responsible for handling the events (keyPresses, passing of time) and the drawing of everything
    related to the gameplay (the maze, the players, score, lives, .etc)
-}


module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as GG

import Types
import GlossTypes

import PacmanUtils

import Tarefa2
import Tarefa4
import Tarefa5
import Tarefa6 hiding (Empty)

import FileUtils
import LazyBFS

import Data.List


-- | Given the set of pictures, returns the pictures corresponding to the pause screen
--
drawPauseScreen :: PictureSet -- ^ The given set of pictures
      -> [Picture] -- ^ The pictures corresponding to the pause screen
drawPauseScreen pics = [(Translate 0.0 (-450.0)) (pausedPic pics)]

-- | Given the set of pictures, returns the pictures corresponding to the death screen
--
drawLostScreen :: PictureSet -- ^ The given set of pictures
      -> [Picture] -- ^ The pictures corresponding to the death screen
drawLostScreen pics = [gameOver pics]

-- | Default size of maze square (30px x 30px)
--
squareSize :: Float
squareSize = 30.0


-- | Given an orientation, returns its unique ID. The ID corresponds, not only to the position
--   in the list of pictures regarding the imagery for the ghosts, but also to the number of times
--   one has to rotate the image of the player facing to the right by 90 degrees in order for it to be
--   in the correct orientation.
--
orientationID :: Orientation -- ^ The given orientation
      -> Int -- ^ The orientation's id
orientationID R = 0
orientationID D = 1
orientationID L = 2
orientationID U = 3
orientationID Null = 0

-- | Given a picture corresponding to a player, and its orientation, returns the picture
--   resulting from rotating the original to the correct position.
--
rotatePicture :: Picture -- ^ The given picture (of a player pointing to the *right*)
      -> Orientation -- ^ The player's orientation
      -> Picture -- ^ The resulting picture
rotatePicture pic o = Rotate (90.0 * fromIntegral(orientationID o)) pic





-- | Given a set of pictures, a maze, the coordinates to place the current picture, and the coordinates of the
--   top left corner of the maze, returns the set of pictures corresponding to the rendering of the maze
--
drawPieces :: PictureSet -- ^ The given set of pictures
      -> Player -- ^ The controlled player
      -> Maze -- ^ The given maze
      -> (Float, Float) -- ^ The coordinates of the top left corner of the maze
      -> (Float, Float) -- ^ The coordinates to render the current picture on
      -> [Picture] -- ^ The resulting set of pictures
drawPieces _ _ [] _ _ = []
drawPieces pics player ([]:t) (xOriginal, yOriginal) (x,y) 
                  = drawPieces pics player t (xOriginal, yOriginal) (xOriginal, y - 30)
drawPieces pics player ((Wall:cs):ms) (xOriginal, yOriginal) (x,y) 
                  = (Translate (x+15) (y-15) (wall pics)) : remaining
                  where remaining = drawPieces pics player (cs:ms) (xOriginal, yOriginal) (x + 30, y)
drawPieces pics player ((Empty:cs):ms) (xOriginal, yOriginal) (x,y) 
                  = drawPieces pics player  (cs:ms) (xOriginal, yOriginal) (x + 30, y)
drawPieces pics player ((((Food Little)):cs):ms) (xOriginal, yOriginal) (x,y)
                  = (Translate (x+15) (y-15) (fdlittle pics)) : remaining
                  where remaining = drawPieces pics player (cs:ms) (xOriginal, yOriginal) (x + 30, y)
drawPieces pics player ((((Food Big)):cs):ms) (xOriginal, yOriginal) (x,y)
                  = (Translate (x+15) (y-15) (fdbig pics)) : remaining
                  where remaining = drawPieces pics player (cs:ms) (xOriginal, yOriginal) (x + 30, y)
drawPieces pics player ((((PacPlayer (Pacman pac))):cs):ms) (xOriginal, yOriginal) (x,y)
                  | openClosed pac == Open = pacOpenPic : remaining
                  | otherwise              = pacClosedPic : remaining
                  where remaining      = drawPieces pics player (cs:ms) (xOriginal, yOriginal) (x + 30, y)
                        pacOpenPic     =  Translate (x+15) (y-15) (rotatePicture (pacOpen pics) (getPlayerOrientation (Pacman pac)))
                        pacClosedPic   =  Translate (x+15) (y-15) (rotatePicture (pacClosed pics) (getPlayerOrientation (Pacman pac)))

drawPieces pics player ((((PacPlayer gh@(Ghost gho))):cs):ms) (xOriginal, yOriginal) (x,y) 
                  | (ghostMode gho == Alive) && gid == pid 
                        = (Translate (x+15) (y-15) (g !! id)) : remaining -- green
                  | (ghostMode gho == Alive) && gid `mod` 2 == 0       
                        = (Translate (x+15) (y-15) (r !! id)) : remaining -- red
                  | (ghostMode gho == Alive)
                        = (Translate (x+15) (y-15) (p !! id)) : remaining -- pink
                  | gid == pid         
                         = (Translate (x+15) (y-15) (lb !! id)) : remaining -- light blue
                  | otherwise         
                         = (Translate (x+15) (y-15) (db !! id)) : remaining -- dark blue
                  where remaining = drawPieces pics player (cs:ms) (xOriginal, yOriginal) (x + 30, y)
                        (r,p,g)   = ghostAlive pics
                        (db,lb)   = ghostDead pics
                        pid       = getPlayerID player
                        gid       = getPlayerID gh
                        id        = orientationID $ getPlayerOrientation (Ghost gho)


-- | Given a number @x@, a Picture, and a point, returns the list of pictures resulting from repeating the
--   given picture @x@ times in the horizontal axis.
--
--   It is an auxiliary function to @drawPacLives@
repeatPicture :: Int -- ^ The number of times to repeat the picture
      -> Picture -- ^ The given picture
      -> (Float, Float) -- ^ The coordinates to place the first picture in
      -> [Picture] -- ^ The resulting set of pictures
repeatPicture 0 _ _ = []
repeatPicture n pic (x,y) = (Translate x y pic) : repeatPicture (n - 1) pic (x + squareSize + 5, y)


-- | Given the set of images and a player (pacman), returns the picture corresponding to the number of lives
--   remaining.
--
drawPacLives :: PictureSet -- ^ The given set of pictures
      -> Player -- ^ The given player
      -> Picture -- ^ The resulting picture, containing the pictures corresponding to the lives of the pacman
drawPacLives pics (Pacman (PacState (_,_,_,_,_,lives) _ _ _)) = Pictures (repeatPicture lives (pacOpen pics) (800,400))


-- | Given a pacman, returns the picture corresponding to the mega mode of the pacman.
--
--   This corresponds to a vertical bar on the top of the window which becomes shorter
--   and shorter until the pacman runs out of mega mode.
--
drawMega :: Player -- ^ The given player 
      -> Picture -- ^ The picture of the mega mode
drawMega (Pacman (PacState _ mega _ _)) = Color ghostBlue (Polygon [(x1, 375), (x2, 375), (x2, 350), (x1,350)])
                                        where mega'     = max 0.0 ((doubleToFloat mega) / 1000)
                                              x1        = (-(fromIntegral windowWidth)) / 2.0
                                              x2        = (x1 + (mega'  * (fromIntegral (windowWidth)) / 10.0))
                                              ghostBlue = GG.makeColor 0.17 0.21 0.57 1.0


-- | Given the current level, returns the picture corresponding to said level
--
drawLevel :: Int -- ^ The given level
      -> Picture -- ^ The picture corresponding to the given level
--
drawLevel lvl = Translate (-800) 400 (Scale 0.4 0.4 (Color white ((Text ("Nivel : " ++ show lvl)))))

-- | Given the current score, returns the picture corresponding to said score
--
drawScore :: Player -- ^ The given score
      -> Picture -- ^ The picture corresponding to the given score
drawScore (Pacman (PacState (_,_,_,_,score,_) _ _ _)) = Translate (-100) 400 (Scale 0.4 0.4 (Color white ((Text ("Pontos : " ++ show score)))))


-- | Given a set of pictures, and the current game state, returns the picture corresponding to the
--   rendering of the game state
--
drawGame :: PictureSet -- ^ The given set of pictures
      -> Manager -- ^ The current game state
      -> Picture -- ^ The resulting picture
drawGame pics man | getPacmanMode pacman == Dying = Pictures (board ++ (drawLostScreen pics))
                  | paused man                    = Pictures (board ++ (drawPauseScreen pics))
                  | otherwise                     = Pictures board
                      where pacLives = drawPacLives pics pacman
                            player   = getPlayer (playersState $ state man) (pid man)
                            pieces = drawPieces pics player (placePlayersOnMap (playersState (state man)) (maze (state man))) (x,y) (x,y)
                            mega = drawMega pacman
                            score = drawScore pacman
                            lvl = drawLevel (level (state man))
                            (y',x') = getSize (maze (state man))
                            (x,y) = (-1 * fromIntegral x' * 30 / 2, fromIntegral y' * 30 / 2)
                            pacman = getPacman (playersState (state man))
                            board = (pieces ++ [pacLives, mega, lvl,score])



-- | Given the set of pictures, draws the picture corresponding to the level up screen
--
drawLevelUp :: PictureSet -- ^ The given set of pictures
      -> Picture -- ^ The picture for the level up display
drawLevelUp pics = (levelUp pics)


-- | Given a list of players, and the id of the ghost being controlled, returns the id
--   of the next ghost.
--
nextGhost :: [Player] -- ^ The given list of players
      -> Int -- ^ The id of the currently controlled ghost
      -> Int -- ^ The id of the next ghost
nextGhost pl pid | isPacman (getPlayer pl pid) = pid
nextGhost pl pid = getPlayerID (ghosts !! ((id + 1) `mod` size))
                  where ghosts  = filter (\x -> not (isPacman x)) pl
                        size    = length ghosts
                        Just id = elemIndex (getPlayer pl pid) ghosts
                        --isPacman (Pacman _) = True
                        --isPacman (Ghost _)  = False



-- | Given an event (key press) and a state, returns the state after the event is processed
--
reactEventGame :: Event -- ^ The given event
      -> GlossState -- ^ The given state
      -> GlossState -- ^ The resulting state
reactEventGame (EventKey (SpecialKey KeyUp)    Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del False) pics map)
            = (GlossState Game bt (Manager newState m pid stp bf delt del False) pics map)
            where newState = updatePlayer (rotatePlayerTo U (getPlayer (playersState st) pid)) st
reactEventGame (EventKey (SpecialKey KeyDown)  Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del False) pics map)
            = (GlossState Game bt (Manager newState m pid stp bf delt del False) pics map)
            where newState = updatePlayer (rotatePlayerTo D (getPlayer (playersState st) pid)) st
reactEventGame (EventKey (SpecialKey KeyLeft)  Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del False) pics map) 
            = (GlossState Game bt (Manager newState m pid stp bf delt del False) pics map)
            where newState = updatePlayer (rotatePlayerTo L (getPlayer (playersState st) pid)) st
reactEventGame (EventKey (SpecialKey KeyRight) Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del False) pics map)
            = (GlossState Game bt (Manager newState m pid stp bf delt del False) pics map)
            where newState = updatePlayer (rotatePlayerTo R (getPlayer (playersState st) pid)) st
reactEventGame (EventKey (SpecialKey KeySpace) Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del False) pics map)
            = (GlossState Game bt (Manager st m newPid stp bf delt del False) pics map)
            where newPid = nextGhost (playersState st) pid
reactEventGame (EventKey (Char 'p') Down _ _) (GlossState _ bt (Manager st m pid stp bf delt del paused) pics map) 
            = (GlossState Game bt (Manager st m pid stp bf delt del (not paused)) pics map)
reactEventGame (EventKey (Char 'q') Down _ _) (GlossState _ bt man pics map)
            = (GlossState MainMenu 0 defaultManager pics map)
reactEventGame (EventKey (SpecialKey KeyEnter) Down _ _) (GlossState _ bt man@(Manager st m pid stp bf delt del False) pics map)
            | (isGameOver st) || (getPacmanMode (getPacman (playersState st)) == Dying) 
                  = (GlossState MainMenu 0 defaultManager pics "")
reactEventGame _ st = st



-- | Given the time elapsed since the last frame was displayed, and the previous state, returns the state
--   after considering the passing of time
reactTimeGame :: Float -- ^ The time elapsed since the last frame
    -> GlossState -- ^ The previous state
    -> GlossState -- ^ The current state
reactTimeGame n (GlossState Game bt (Manager st memo pid stp bf delt del False) pics map)
                                                      | del < defaultDelayTime' 
                                                            = GlossState Game bt 
                                                                  (Manager st memo pid stp bf delt (del + n) False) pics map
                                                      | isGameOver finalState   
                                                            = GlossState (LevelUp 5) bt 
                                                                  (Manager st memo pid 0 bf delt del False) pics map
                                                      | otherwise               
                                                            = GlossState Game bt
                                                                   (resetTimer (Manager finalState memo pid (stp + 1) 
                                                                   bf delt del False)) pics map
                                                        where 
                                                              defaultDelayTime' = (fromInteger defaultDelayTime) / 1000
                                                              finalState        = (passTimeAux stp newState)
                                                              player            = getPlayer (playersState st) pid
                                                              pac               = getPacman (playersState st)
                                                              orientation       = getPlayerOrientation player  
                                                              Just (Move _ or)  = bot' stp st memo     
                                                              newState'         = rotateGhosts st (ghostPlay' st memo)                                          
                                                              newState | isPacman player = newState'
                                                                       | otherwise   = updatePlayer (rotatePlayerTo or pac)  (updatePlayer (rotatePlayerTo orientation player) newState')

-- | Resets the @delta@ and @before@ to zero and the current time, respectively
-- 
resetTimer :: Manager -- ^ The given manager
    -> Manager -- ^ The resulting manager
resetTimer (Manager st m pid stp bf delt del p) = (Manager st m pid stp bf delt 0 p)
