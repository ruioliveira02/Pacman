{-| 
    Module: Tarefa2

    Module responsible for implementing the game logic.
-}



module Tarefa2 where

import Types
import PacmanUtils


-- | The speed of ghost in Dead Mode (0.5)
--
-- == /Example/
--
-- >>> halfSpeed
-- 0.5
halfSpeed :: Double
halfSpeed = 0.5


-- | The speed of ghost in Alive Mode (1.0)
--
-- == /Example/
--
-- >>> regularSpeed
-- 1.0
regularSpeed :: Double
regularSpeed = 1.0


-- | The places to check to spawn the ghosts, in relative position.
--   (0,0) corresponds to the center of the ghost house, rounding to the left
ghostSpawnChecks :: [Coords]
ghostSpawnChecks = [(0,0), (0,1), (0,-1), (0,2), (0,-2), (0,3), (0,-3),
                    (1,0), (1,1), (1,-1), (1,2), (1,-2), (1,3), (1,-3)]


-- | Returns the spawning position of the ghosts.
--   This corresponds to the inside of the ghost house, from the center to the borders
--   when the previous position is already taken, rounding to the left
--   in case the width of the house is even
--
--   If there is no space available to spawn pacman, an error is thrown
--
getGhostSpawnCoords :: State -- ^ The given state
    -> Coords -- ^ The coordinates of the next spawn position for a ghost
getGhostSpawnCoords (State m pl lvl) = case aux of
                                        (h : t) -> h
                                        [] -> error "There's no space to spawn the ghost"
                                       where mz = placePlayersOnMap pl m
                                             (h, w) = getSize m
                                             (y, x) = (div (h - 1) 2, div (w - 1) 2)
                                             aux = (filter (\c -> getPieceFrom mz c == Empty))
                                                    $ map (\(a,b) -> (a + y, b + x)) ghostSpawnChecks


-- | Returns pacman's spawning position.    
--   This corresponds to the position directly below the ghost house,
--   rounding to the left in case the width of the house is even
--
--   If there is no space available to spawn pacman, an error is thrown
--
getPacmanSpawnCoords :: State -- ^ The given state
    -> Coords -- ^ The coordinates of pacman's spawn position
getPacmanSpawnCoords (State m pl lvl) | isSquareClear (y, x) m = (y, x)
                                      | isSquareClear (y + 1, x) m = (y + 1, x)
                                      | otherwise = error ("Pacman's spawn " ++ (show m) ++ " is obstructed")
                                     where (h, w) = getSize m
                                           (y, x) = ((div (h - 1) 2) + 2, div (w - 1) 2)


-- | Changes the state of ghosts to Dead and reduces their speed to half
--
--  This function is called whenever pacman eats a food big
--
-- == /Example/
--
-- >>> scareGhosts state
-- State {maze = [[#,#,#,#,#,#,#,#],[ ,*,.,.,.,M,W, ],[ ,.,.,<,.,#,., ],[#,#,#,#,#,#,#,#]], playersState = [Pacman (PacState {pacState = (0,(3,2),1.0,R,5,1),
-- timeMega = 0.0, openClosed = Open, pacmanMode = Normal}),Ghost (GhoState {ghostState = (1,(6,1),0.5,L,10,0), ghostMode = Dead})], level = 1}
scareGhosts :: State -- ^ The original state
    -> State -- ^ The resulting state
scareGhosts (State m [] lv) = State m [] lv
scareGhosts (State m ((Ghost (GhoState gs gm)) : ps) lv) = State m2 (ghost : ps2) lv2
                                            where (State m2 ps2 lv2) = scareGhosts (State m ps lv)
                                                  ghost = setPlayerSpeed halfSpeed (Ghost (GhoState gs Dead))
scareGhosts (State m (pacman : ps) lv) = State m2 (pacman : ps2) lv2
                                        where (State m2 ps2 lv2) = scareGhosts (State m ps lv)


-- | Resets the game state.
--   
--   More specifically, it removes the players from their locations and places them at their spawning positions.
--   It also revives dead ghosts and set pacman's time mega to 0.
--
--   This function is called when pacman loses a life and the game is reset.
respawnPlayers :: State -- ^ The original game state
    -> State -- ^ The updated game state, with the players at their spawning positions
respawnPlayers (State m [] lv) = State m [] lv
respawnPlayers (State m ((Ghost (GhoState plst _)) : ps) lvl)
    = (State m2 ((setPlayerSpeed regularSpeed $ setPlayerCoordinates (getGhostSpawnCoords ns)
                                              $ rotatePlayerTo R (Ghost (GhoState plst Alive))) : pl2) lvl2)
    where ns@(State m2 pl2 lvl2) = respawnPlayers (State m ps lvl)

respawnPlayers (State m (pacman : ps) lvl)
    = (State m2 ((setPlayerCoordinates (getPacmanSpawnCoords ns) (setTimeMega 0 pacman)) : pl2) lvl2)
    where ns@(State m2 pl2 lvl2) = respawnPlayers (State m ps lvl)



-- | When combined with areAdjacent, defines the geometry of the board
--   (currently the coordinates are mod n). Returns the coordinates of
--   where a player lands should he move in the specified
--   direction (Move does not have to be legal)
--
-- == /Examples/
--
-- >>> pseudoMove (2,3) L
-- (2,2)
--
-- >>> pseudoMove (2,3) D
-- (3,3)
pseudoMove :: Coords -- ^ The (y, x) size of the maze (used for the tunnel)
    -> Orientation -- ^ The direction to move in
    -> Coords -- ^ The initial position of the player
    -> Coords -- ^ The coordinates of the square the player lands in
pseudoMove (h, w) U (y, x) = (if y == 0 then 0 else y - 1, x)
pseudoMove (h, w) D (y, x) = (if y == h - 1 then (h-1) else y + 1, x)
pseudoMove (h, w) L (y, x) = (y, if x == 0 then w - 1 else x - 1)
pseudoMove (h, w) R (y, x) = (y, if x == w - 1 then 0 else x + 1)
pseudoMove _ Null c = c


-- | When combined with pseudoMove, defines the geometry of the board
--   (currently the coordinates are mod n). Returns whether
--   two given cells of the maze are adjacent
--
-- == /Examples/
--
-- >>> areAdjacent testMaze (2,3) (3,3)
-- True
--
-- >>> areAdjacent testMaze (2,2) (3,3)
-- False
areAdjacent :: Maze -> Coords -> Coords -> Bool
areAdjacent mz (y1, x1) (y2, x2) | dx == 0 = (dy == 1) || (dy == h - 1)
                                 | dy == 0 = (dx == 1) || (dx == w - 1)
                                 | otherwise = False
                                    where dx = abs (x2 - x1)
                                          dy = abs (y2 - y1)
                                          (h, w) = getSize mz
--areAdjacent mz a b = or $ map ((== b) . ((flip (pseudoMove (getSize mz))) a)) [U, D, L, R]
--esta definicao era demasiado lenta. RIP.


-- | This function moves pacman to the coords, sets his new position as Empty
--   and calculates the resulting effects on the state:
--
--   * If he walks into Empty, nothing happens
--
--   * If he walks into a Food Little, he gains 1 point
--
--   * If he walks into a Food Big, his mode becomes Mega, he gains 5 points and all ghosts
--     become scared (their state becomes Dead and their speed is reduced to half)
--
--   If called by a non-pacman player, an error may be thrown
move :: Int -- ^ The id of the player who will move
    -> Coords -- ^ The new coordinates
    -> State -- ^ The current state
    -> State -- ^ The resulting state
move id c s@(State m ps l)
    | getPacmanMode player == Dying = s
    | not $ areAdjacent m (getPlayerCoords player) c = s
    | otherwise = case getPieceFrom m c of
        Empty -> updatePlayer (setPlayerCoordinates c player) (State m ps l)
        Food Little -> updatePlayer ((updatePlayerScore 1) $ setPlayerCoordinates c player)
                                    (State (replaceSquareWith c Empty m) ps l)
        Food Big -> scareGhosts $ updatePlayer ((setPacmanMode Mega) $ (setTimeMega 10000 {- 10s -})
                                                                     $ (updatePlayerScore 5)
                                                                     $ setPlayerCoordinates c player)
                                               (State (replaceSquareWith c Empty m) ps l)
    where player = getPlayer ps id


-- | Computes the interaction player by player.
--   This function is an auxiliar function to interation.
--   If for some reason it is called by a non-Pacman player, an error may be thrown
--
interactPacman :: [Player] -- ^ the list of players to interact with the main player
    -> State -- ^ the original state
    -> State -- ^ the resulting state
interactPacman [] s = s
interactPacman (p : ps) s@(State m pl lvl)
    | areTheSame pacman p = interactPacman ps s
    | y1 /= y2 || x1 /= x2 = interactPacman ps s
    | otherwise = case p of
                Ghost (GhoState _ Alive) -> if (getLives pacman == 0)
                                                then updatePlayer (setPacmanMode Dying pacman) s
                                                else respawnPlayers (updatePlayer (stealLife pacman) s)
                Ghost (GhoState gs Dead) -> (updatePlayer ((setPlayerCoordinates (getGhostSpawnCoords s))
                                                           $ (setPlayerSpeed regularSpeed)
                                                           $ (Ghost (GhoState gs Alive))))
                                            $ updatePlayer ((updatePlayerScore 10) pacman)
                                                            $ (interactPacman ps) s
                _ -> interactPacman ps s
    where pacman = getPacman pl
          (y1, x1) = getPlayerCoords pacman
          (y2, x2) = getPlayerCoords p


-- | Calculates the result from the player interactions.
--   These interactions happen when two players are at the same position:
--
--   * A dead ghost is killed by pacman, which sends him back to the ghost house alive and
--     causes pacman to gain 10 points
--
--   * An alive ghost kills pacman, which resets the level and causes pacman to lose 1 life
--
--   * If pacman had 0 lives when he interacts with an alive ghost, instead his state changes to Dying
--     and all of the ghosts are removed from the game
--
--   Aditionally, if pacman runs out of timeMega, his state goes back to Normal and the ghosts' state to Alive.
--   If called by a ghost, an error may be thrown
interaction :: State -- ^ the original state
    -> State -- ^ the resulting state
interaction s@(State m pl lvl)
    | getPacmanMode player == Dying = s
    | otherwise = interactPacman list (State m list lvl)
      where player = getPacman pl
            list = if (getTimeMega player <= 0) then map mapAux pl else pl
            mapAux (Pacman (PacState a b c Mega)) = Pacman (PacState a b c Normal)
            mapAux (Ghost (GhoState ps Dead)) = setPlayerSpeed regularSpeed (Ghost (GhoState ps Alive))
            mapAux p = p


-- | Updates the game state according to the specified play:
--
--   * If the player (assumed to be a pacman) is Dying, nothing happens
--
--   * If the player tries to move in a diferent direction than his orientation, instead of
--     moving he is rotated to the direction he tried to move towards
--
--   * If the player should move (there is no Wall in front of him),
--     this function calls move and then interaction
--
-- == /Example/
--
-- >>> play (Move 0 L) state 
-- State {maze = [[#,#,#,#,#,#,#,#],[ ,*,.,.,.,M,., ],[ ,.,.,>,.,#,., ],[#,#,#,#,#,#,#,#]], playersState = [Pacman (PacState {pacState = (0,(3,2),1.0,L,5,1),
-- timeMega = 0.0, openClosed = Open, pacmanMode = Normal}),Ghost (GhoState {ghostState = (1,(6,1),1.0,L,10,0), ghostMode = Alive})], level = 1}
play :: Play -- ^ The specified play
    -> State -- ^ The original game state
    -> State -- ^ The resulting game state
play mv@(Move id orientation) s@(State m ps l) 
    = case player of
        (Pacman _) -> if (getPacmanMode player == Dying)
                    then s
                 else if (not $ hasOrientation player orientation)
                    then interaction $ updatePlayer (toggleMouth
                                                     $ (rotatePlayerTo orientation)
                                                     $ setTimeMega (nextTimeMega player) player) s
                 else if (isSquareClear nc m)
                    then interaction $ (move id nc)
                         $ updatePlayer (toggleMouth $ (setTimeMega (nextTimeMega player)) player) s
                 else
                    interaction $ updatePlayer (toggleMouth $ setTimeMega (nextTimeMega player) player) s

        (Ghost _)  -> if (orientation == Null)
                    then s
                 else if (not $ hasOrientation player orientation)
                    then updatePlayer (rotatePlayerTo orientation player) s
                 else if (isSquareClear nc m)
                    then interaction $ updatePlayer (setPlayerCoordinates nc player) s
                 else s

       where player = getPlayer ps id
             nc = pseudoMove (getSize m) orientation (getPlayerCoords player)