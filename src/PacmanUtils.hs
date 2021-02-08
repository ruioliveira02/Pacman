{- | 
    Module : PacmanUtils

    Module containing generic auxiliary functions required by multiple modules.

-}


module PacmanUtils where

import Types


-- | Variable used to test functions
{- |
@testMaze = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Empty, Food Big, Food Little, Food Little, Food Little, Empty, Food Little, Empty],
    [Empty, Food Little, Food Little, Empty, Food Little, Wall, Food Little, Empty],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
       ]@
-}
testMaze :: Maze
testMaze = [
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall],
    [Empty, Food Big, Food Little, Food Little, Food Little, Empty, Food Little, Empty],
    [Empty, Food Little, Food Little, Empty, Food Little, Wall, Food Little, Empty],
    [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
       ]


-- | Variable used to test functions
--
-- @js = [jogador1, jogador2]@
js :: [Player]
js = [jogador1,jogador2]

-- | Variable used to test functions
--
-- @jogador1= (Pacman (PacState (0,(2,3),1,R,5,1) 0 Open Normal))@
jogador1 :: Player
jogador1= (Pacman (PacState (0,(2,3),1,R,5,1) 0 Open Normal))

-- | Variable used to test functions
--
-- @jogador2 = (Ghost (GhoState (1,(1,5),1,L,10,0) Alive))@
jogador2 :: Player
jogador2 = (Ghost (GhoState (1,(1,5),1,L,10,0) Alive))


---------------------------------------------------------------
----------------------     MAZE UTILS     ---------------------
---------------------------------------------------------------


-- | Returns the (y, x) size of the given maze,
--   where y is vertical and x is horizontal
--
-- == /Example/
--
-- >>> getSize testMaze
-- (8, 4)

getSize :: Maze -- ^ the given maze
    -> Coords -- ^ the (y, x) size of the maze
getSize mz = (length (filter notEmpty mz), length (head mz))
            where notEmpty [] = False
                  notEmpty _ = True


-- | Returns the Piece at the specified coordinates.
--   For out of bounds coordinates an error is thrown
--
--   This is a recursive function which first iterates to the correct corridor (y coordinate),
--   and, from there, locates the correct piece on that corridor (x coordinate)
--
--   Note that (y, x) corresponds to the distance vertically and horizontally,
--   respectively, to the wanted square (origin is (0,0))
--
-- == /Examples/
--
-- >>> getPieceFrom (1,1) testMaze
-- Food Big
--
-- >>> getPieceFrom (3,3) testMaze
-- Wall
getPieceFrom :: Maze -- ^ The coordinates of the square
    -> Coords -- ^ The given maze
    -> Piece -- ^ The piece found at the given coordinates
getPieceFrom m (y, x) = (m !! y) !! x


-- | Checks if square of given coordinates is clear, i.e, if it is not a wall.
--
--   This function uses the function getPieceFrom to get the piece and then compare it with a Wall
--
--   Note that (y, x) corresponds to the distance vertically and horizontally,
--   respectively, to the wanted square (origin is (0,0))
--
-- == /Examples/
--
-- >>> isSquareClear (1,1) testMaze
-- True
--
-- >>> isSquareClear (3,3) testMaze
-- False
isSquareClear :: Coords -- ^ The coordinates of the square
    -> Maze -- ^ The given maze
    -> Bool -- ^ Whether the square is clear or not
isSquareClear c m = getPieceFrom m c  /= Wall



-- | Replaces the x-th value  of a corridor with the given piece.
--   For out of bounds coordinates an error is thrown.
--   Used as an auxiliary function to replaceSquareWith
--
-- == /Example/
--
-- >>> replaceSquareWith1D [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall] 4 (Food Little)
-- [#,#,#,#,.,#,#,#]
replaceSquareWith1D :: Corridor -- ^ the corridor to change
  -> Int -- ^ index of the value to replace
  -> Piece -- ^ piece to substitute the original one with
  -> Corridor -- ^ the resulting corridor
replaceSquareWith1D (x:xs) 0 piece = (piece:xs)
replaceSquareWith1D (x:xs) n piece = x : replaceSquareWith1D xs (n - 1) piece
replaceSquareWith1D [] _ _ = error "Coordinates out of bounds"


-- | Replaces the value (y, x) of the maze with the given piece.
--   For out of bounds coordinates an error is thrown
--
--   This is a recursive function which first iterates to the correct corridor (y coordinate),
--   and, from there, locates the correct piece on that corridor (x coordinate)
--
--   Note that (y, x) corresponds to the distance vertically and horizontally,
--   respectively, to the wanted square (origin is (0,0))
--
-- == /Example/
--
-- >>> replaceSquareWith testMaze (2,2) Wall
-- [[#,#,#,#,#,#,#,#],[ ,o,.,.,.,.,., ],[ ,.,#,.,.,#,., ],[#,#,#,#,#,#,#,#]]

replaceSquareWith :: Coords -- ^ coordinates to change
    -> Piece -- ^ piece to substitute original one with
    -> Maze -- ^ the maze to change
    -> Maze -- ^ the resulting maze
replaceSquareWith (0,0) piece ((p:ps):t) = (piece : ps) : t
replaceSquareWith (0, x) piece (h:t) = replaceSquareWith1D h x piece : t
replaceSquareWith (y,x) piece (h:t) = h : replaceSquareWith (y - 1, x) piece t
replaceSquareWith (_, _) _ [] = error "Coordinates out of bounds"


-- | Given a maze, returns the coordinates of the first food, big or small,
--   or Nothing if the isn't any
--
--   The food is searched up to down, left to right (similar to reading)
firstFood :: Maze -- ^ The given maze
    -> Maybe Coords -- ^ The coordinates of the first food ('Nothing' if there is no food)
firstFood maze = firstFood' maze (0, 0)
    where firstFood' :: Maze -> Coords -> Maybe Coords
          firstFood' (((Food _) : xs) : ys) (y, x) = Just (y, x)
          firstFood' ((_ : xs) : ys) (y, x) = firstFood' (xs : ys) (y, x + 1)
          firstFood' ([] : ys) (y, x) = firstFood' ys (y + 1, 0)
          firstFood' [] _ = Nothing



-- | Given a state, returns whether or not the game is over by the food remaining, i.e, if
--   there is no food on the maze, then the game is over.
--
isGameOver :: State -- ^ The given state
    -> Bool -- ^ Whether there is food on the maze or not
isGameOver st = firstFood (maze st) == Nothing

---------------------------------------------------------------
---------------------     PLAYER UTILS     --------------------
---------------------------------------------------------------


-- | Checks if the given player is a pacman
--
-- == /Examples/
--
-- >>> isPacman (Pacman (PacState (1,(1,1),1.0,U,1,1) 0.0 Open Normal))
-- >>> True
--
-- >>> isPacman (Ghost (GhoState (1,(1,1),1.0,U,1,1) Alive))
-- >>> False
--
isPacman :: Player -- ^ The given player
    -> Bool -- ^ Whether or not the given player is a pacman
isPacman (Pacman _) = True
isPacman (Ghost _)  = False


-- | Checks if a player has a certain orientation
--
-- == /Examples/
--
-- >>> hasOrientation jogador1 R
-- True
--
-- >>>  hasOrientation jogador2 D
-- False
hasOrientation :: Player -- ^ The given player
    -> Orientation -- ^ The orientation we want to compare the player's with
    -> Bool -- ^ Whether the orientation is the same or not
hasOrientation (Pacman pac) orientation = o == orientation
                                where (_, _, _, o, _, _) = pacState pac
hasOrientation (Ghost gho) orientation = o == orientation
                                where (_, _, _, o, _, _) = ghostState gho


-- | Rotates the given player to the specified orientation
--
-- == /Example/
-- 
-- >>> rotatePlayerTo U jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,U,5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
rotatePlayerTo :: Orientation -- ^ The new orientation
    -> Player -- ^ The given player
    -> Player -- ^ The resulting player
rotatePlayerTo o (Pacman (PacState (id, c, v, _, s, l) tm oc pm)) = Pacman (PacState (id, c, v, o, s, l) tm oc pm)
rotatePlayerTo o (Ghost (GhoState (id, c, v, _, s, l) gm)) = Ghost (GhoState (id, c, v, o, s, l) gm)


-- | Sets the coordinates of the player
--
-- == /Example/
-- 
-- >>> setPlayerCoordinates (1,1) jogador1
-- Pacman (PacState {pacState = (0,(1,1),1.0,R,5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
setPlayerCoordinates :: Coords -- ^ The new coordinates
    -> Player -- ^ The given player
    -> Player -- ^ The resulting player
setPlayerCoordinates c (Pacman (PacState (id, _, v, o, s, l) tm oc pm)) = Pacman (PacState (id, c, v, o, s, l) tm oc pm)
setPlayerCoordinates c (Ghost (GhoState (id, _, v, o, s, l) gm)) = Ghost (GhoState (id, c, v, o, s, l) gm)


-- | Changes the score of the given player by the specified amount.
--
-- == /Examples/
-- 
-- >>> updatePlayerScore 20 jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,25,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
--
-- >>> updatePlayerScore (-10) jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,-5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
updatePlayerScore :: Int -- ^ the score change. Positive to increase the score, negative to decrease the score
    -> Player -- ^ the given player
    -> Player -- ^ the resulting player (i.e. the player with updated score)
updatePlayerScore ds (Pacman (PacState (id, c, v, o, s, l) tm oc pm)) = Pacman (PacState (id, c, v, o, s + ds, l) tm oc pm)
--updatePlayerScore ds (Ghost (GhoState (id, c, v, o, s, l) gm)) = Ghost (GhoState (id, c, v, o, s + ds, l) gm)


-- | Updates given player's pacmode to the specified mode. For ghosts an error is thrown
--
-- == /Example/
-- 
-- >>> setPacmanMode Mega jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Mega})
setPacmanMode :: PacMode -- ^ The new PacMode
    -> Player -- ^ The original player
    -> Player -- ^ The resulting player (i.e. the original player with the new PacMode)
setPacmanMode pm (Pacman (PacState (id, c, v, o, s, l) tm oc _)) = Pacman (PacState (id, c, v, o, s, l) tm oc pm)
setPacmanMode _ _ = error "Ghosts don't have a pacmode"


-- | Returns the given player's timeMega. For ghosts an error is thrown
--
-- == /Example/
-- 
-- >>> getTimeMega jogador1
-- 0
getTimeMega :: Player -- ^ The given player
    -> Double -- ^ The player's time mega
getTimeMega (Pacman (PacState _ tm _ _)) = tm
getTimeMega _ = error "Ghosts don't have timeMega"


-- | Sets the given player's timeMega. For ghosts an error is thrown
--
-- == /Example/
-- 
-- >>> setTimeMega 1.0 jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,5,1), timeMega = 1.0, openClosed = Open, pacmanMode = Normal})
setTimeMega :: Double -- ^ The given value
    -> Player -- ^ The original player
    -> Player -- ^ The resulting player
setTimeMega tm (Pacman (PacState ps _ oc pm)) = (Pacman (PacState ps tm oc pm))
setTimeMega _ _ = error "Ghosts don't have timeMega"


-- | Returns the value to which timeMega should be updated during the next call of play on that player.
--   This value is calculated by subtracting from the current value the time elapsed since the last call of play (on average).
--   This function is (unsurprisingly) an auxiliary function to play.
nextTimeMega :: Player -- ^ The given player
    -> Double -- ^ The value to which timeMega should be updated 
nextTimeMega p = max 0 ((getTimeMega p) - (fromInteger defaultDelayTime / (getPlayerSpeed p)))


-- | Returns the given player's speed
--
-- == /Example/
-- 
-- >>> getPlayerSpeed jogador1
-- 1
getPlayerSpeed :: Player -- ^ The given player
    -> Double -- ^ The player's speed
getPlayerSpeed (Pacman (PacState (_, _, v, _, _, _) _ _ _)) = v
getPlayerSpeed (Ghost (GhoState (_, _, v, _, _, _) _)) = v


-- | Sets the player's speed
--
-- == /Example/
-- 
-- >>> setPlayerSpeed 2.27 jogador1
-- Pacman (PacState {pacState = (0,(3,2),2.27,R,5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
setPlayerSpeed :: Double -- ^ The new speed
    -> Player -- ^ The player to update the speed
    -> Player -- ^ The resulting player
setPlayerSpeed v (Pacman (PacState (id, c, _, o, s, l) tm oc pm)) = Pacman (PacState (id, c, v, o, s, l) tm oc pm)
setPlayerSpeed v (Ghost (GhoState (id, c, _, o, s, l) gm)) = Ghost (GhoState (id, c, v, o, s, l) gm)


-- | Returns the number of lives of the given player
--
-- == /Example/
-- 
-- >>> getLives jogador1
-- 1
getLives :: Player -- ^ The given player
    -> Int -- ^ The number of lives of the player
getLives (Pacman (PacState (id, c, v, o, s, l) tm oc pm)) = l
getLives (Ghost (GhoState (id, c, v, o, s, l) gm)) = l

-- | Steals one life from the specified player (i.e. life = life - 1)
--
-- == /Example/
-- 
-- >>> stealLife jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,5,0), timeMega = 0.0, openClosed = Open, pacmanMode = Normal})
stealLife :: Player -- ^ The given player
    -> Player -- ^ The given player with one less life
stealLife (Pacman (PacState (id, c, v, o, s, l) tm oc pm)) = Pacman (PacState (id, c, v, o, s, l - 1) tm oc pm)
--stealLife (Ghost (GhoState (id, c, v, o, s, l) gm)) = Ghost (GhoState (id, c, v, o, s, l - 1) gm)


-- | Toggles the mouth state for pacman (i.e. if it is closed, sets it to open and vice-versa).
--   For ghosts an error is thrown
--
-- == /Example/
-- 
-- >>> toggleMouth jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,5,1), timeMega = 0.0, openClosed = Closed, pacmanMode = Normal})
toggleMouth :: Player -- ^ The given player
    -> Player -- ^ The resulting player
toggleMouth (Pacman (PacState ps tm Open pm)) = Pacman (PacState ps tm Closed pm)
toggleMouth (Pacman (PacState ps tm Closed pm)) = Pacman (PacState ps tm Open pm)
toggleMouth ghost = error "Ghosts don't have a mouth"


-- | Sets the player's mouth to the specified state. For ghosts an error is thrown
--
-- == /Example/
-- 
-- >>> setMouth Closed jogador1
-- Pacman (PacState {pacState = (0,(3,2),1.0,R,5,1), timeMega = 0.0, openClosed = Closed, pacmanMode = Normal})
setMouth :: Mouth -- ^ The specified mouth state
    -> Player -- ^ The original player
    -> Player -- ^ The resulting player
setMouth ms (Pacman (PacState ps tm _ pm)) = (Pacman (PacState ps tm ms pm))
setMouth _ ghost = error "Ghosts don't have a mouth"





---------------------------------------------------------------
----------------------     STATE UTILS     --------------------
---------------------------------------------------------------


-- | Returns the player whose id is equal to x.
--   If no such player exists, an error is thrown
--
-- == /Examples/
--
-- >>> getPlayer js 0
-- Pacman (PacState {pacState = (1,(6,15),1.0,L,10,0), timeMega = 0.0, openClosed = Closed, pacmanMode = Normal})
--
-- >>> getPlayer js 2
-- *** Exception: Player not found

getPlayer :: [Player] -- ^ List of players to search
    -> Int -- ^ Id of wanted player
    -> Player -- ^ The wanted player
getPlayer [] id = error "Player not found"
getPlayer (p : ps) id | getPlayerID p == id = p
                      | otherwise = getPlayer ps id


-- | Retrieves the first pacman player from the given list of players.
--   If no pacman is found, an error is thrown
getPacman :: [Player] -- ^ The list of players
    -> Player -- ^ The first pacman
getPacman ((Pacman pac):t) = (Pacman pac)
getPacman (x:t) = getPacman t
getPacman [] = error "Pacman not found"


-- | Tests whether or not two players are the same by comparing their type and id
-- 
-- == /Examples/
--
-- >>> areTheSame jogador1 jogador1
-- True
--
-- >>> areTheSame jogador1 jogador2
-- False
areTheSame :: Player -- ^ The first player
    -> Player -- ^ The second player
    -> Bool -- ^ Whether they are equal (i.e. have the same id and type)
areTheSame (Pacman (PacState (id1, _, _, _, _, _) _ _ _)) (Pacman (PacState (id2, _, _, _, _, _) _ _ _)) = id1 == id2
areTheSame (Ghost (GhoState (id1, _, _, _, _, _) _)) (Ghost (GhoState (id2, _, _, _, _, _) _)) = id1 == id2
areTheSame _ _ = False


-- | Updates the list by replacing the equal player in the list with the specified player.
--   If no such player is found an error is thrown
--
--   It is an auxiliar function used by @updatePlayer@. Comparisons are done via the @areTheSame@ function
--
-- == /Example/
--
-- >>> updatePlayerList (Pacman (PacState (0,(3,2),1,U,5,1) 0 Open Normal)) js 
-- [Pacman (PacState {pacState = (0,(3,2),1.0,U,5,1), timeMega = 0.0, openClosed = Open, pacmanMode = Normal}),Ghost (GhoState {ghostState = (1,(6,1),1.0,L,10,0), ghostMode = Alive})]
updatePlayerList :: Player -- ^ The new player
    -> [Player] -- ^ The initial list
    -> [Player] -- ^ The resulting list
updatePlayerList _ [] = error "Player not found"
updatePlayerList np (p : ps) | areTheSame p np  = (np : ps)
                             | otherwise        = p : (updatePlayerList np ps)


-- | Updates the state by replacing the equal player in the state with the specified player.
--   The replacement is done in the player list (by direct replacement)
--
-- == /Example/
--
-- >>> updatePlayer (Pacman (PacState (0,(3,2),1,U,5,1) 0 Open Normal)) state
-- State {maze = [[#,#,#,#,#,#,#,#],[ ,*,.,.,.,M,., ],[ ,.,.,u,.,#,., ],[#,#,#,#,#,#,#,#]], playersState = [Pacman (PacState {pacState = (0,(3,2),1.0,U,5,1),
-- timeMega = 0.0, openClosed = Open, pacmanMode = Normal}),Ghost (GhoState {ghostState = (1,(6,1),1.0,L,10,0), ghostMode = Alive})], level = 1}
updatePlayer :: Player -- ^ The new player
    -> State -- ^ The original state
    -> State -- ^ The resulting state
updatePlayer player (State m ps lvl) = State m (updatePlayerList player ps) lvl
