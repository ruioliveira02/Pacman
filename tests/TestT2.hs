module TestT2 where

import System.IO.Unsafe

import FileUtils
import Types
import Tarefa2

{-

>>> ghci TestT2 -i../src

-}


-- First element is the input state, the second the move, the third is the expected value
testsT2 :: [(String,Play,String)]
testsT2 = [("mazes/Tarefa2/eat_food_big_L_input.txt",Move 0 L,"mazes/Tarefa2/eat_food_big_L_output.txt"),
           ("mazes/Tarefa2/eat_food_big_R_input.txt",Move 0 R,"mazes/Tarefa2/eat_food_big_R_output.txt"),

           ("mazes/Tarefa2/eat_food_little_U_input.txt",Move 0 U,"mazes/Tarefa2/eat_food_little_U_output.txt"),
           ("mazes/Tarefa2/eat_food_little_D_input.txt",Move 0 D,"mazes/Tarefa2/eat_food_little_D_output .txt"),
           
           ("mazes/Tarefa2/cross_tunnel_1_input.txt",Move 0 L,"mazes/Tarefa2/cross_tunnel_1_output.txt"),
           ("mazes/Tarefa2/cross_tunnel_2_input.txt",Move 0 R,"mazes/Tarefa2/cross_tunnel_2_output.txt"),

           ("mazes/Tarefa2/different_orientation_D_L_input.txt",Move 0 L,"mazes/Tarefa2/different_orientation_D_L_output.txt"),
           ("mazes/Tarefa2/different_orientation_L_R_input.txt",Move 0 R,"mazes/Tarefa2/different_orientation_L_R_output.txt"),           
           ("mazes/Tarefa2/different_orientation_R_U_input.txt",Move 0 U,"mazes/Tarefa2/different_orientation_R_U_output.txt"),
           ("mazes/Tarefa2/different_orientation_U_L_input.txt",Move 0 L,"mazes/Tarefa2/different_orientation_U_L_output.txt"),
           
           ("mazes/Tarefa2/wall_collision_D_input.txt",Move 0 D,"mazes/Tarefa2/wall_collision_D_output.txt"),
           ("mazes/Tarefa2/wall_collision_R_input.txt",Move 0 R,"mazes/Tarefa2/wall_collision_R_output.txt"),

           ("mazes/Tarefa2/eat_ghost_alive_1_input.txt",Move 0 R,"mazes/Tarefa2/eat_ghost_alive_1_output.txt"),
           ("mazes/Tarefa2/eat_ghost_dead_1_input.txt",Move 0 R,"mazes/Tarefa2/eat_ghost_dead_1_output.txt"),

           ("mazes/Tarefa2/eat_ghost_dead_and_food_little_input.txt",Move 0 R,"mazes/Tarefa2/eat_ghost_dead_and_food_little_output.txt"),
           ("mazes/Tarefa2/eat_ghost_alive_and_food_big_input.txt",Move 0 R,"mazes/Tarefa2/eat_ghost_alive_and_food_big_output.txt"),

           ("mazes/Tarefa2/cross_tunnel_and_eat_ghost_input.txt",Move 0 L,"mazes/Tarefa2/cross_tunnel_and_eat_ghost_output.txt")
            ]


runT2Tests :: IO (Int, Int)
runT2Tests = do
    let (p,f) = unsafePerformIO $ runT2TestsAux testsT2
    putStrLn ((show p) ++ " passed, " ++ (show f) ++ " failed")
    return (p,f)




{-

        Auxiliary functions to runT2Tests   |

-}



runT2TestsAux :: [(String,Play,String)] -> IO (Int, Int)
runT2TestsAux [] = do
    return (0,0)
runT2TestsAux ((str1,pl,str2):t) = do
    putStrLn ("Running test " ++ str1)
    putStrLn ""
    let stateIn  = convertToTest2 $ lines $ unsafePerformIO (readFile str1)
    let stateOut = convertToTest2 $ lines $ unsafePerformIO (readFile str2)
    let ans = play pl stateIn
    putStrLn $ show stateIn
    putStrLn ""
    putStrLn $ show pl
    putStrLn ""

    putStrLn "Expected"
    putStrLn $ show stateOut

    putStrLn "Result"
    putStrLn $ show ans

    putStrLn ""
    putStrLn ""
    
    let (p,f) = unsafePerformIO $ runT2TestsAux t 

    let res = (if (ans == stateOut) then "Test PASSED" else ("Test FAILED"))

    putStrLn res
    putStrLn "------------------------------------------------------------------------"

    return (if (ans == stateOut) then (1 + p, f) else (p, f + 1))



convertToTest2 :: [String] -> State
convertToTest2 lstr = convertToState lstr




convertToState :: [String] -> State
convertToState lstr = State maze players level
                    where (maze, _)    = convertLinesToPiece a1 0 [] 
                          players = getPlayers b1
                          level   = getLevel c1
                          (a1,x1) = break (== "--end-maze--") lstr
                          (b1,b2) = break (== "--end-players--") (tail x1)
                          c1 = tail b2



getLevel :: [String] -> Int
getLevel (x:xs) = read x :: Int


getPlayers :: [String] -> [Player]
getPlayers [] = []
getPlayers lstr = p : getPlayers p1
                  where (p,p1) = getCurrentPlayer lstr


getCurrentPlayer :: [String] -> (Player, [String])
getCurrentPlayer (id:x:y:speed:orientation:points:lives:t) = (p,t1)
                                    where (p,t1) = getPlayer' (((read id :: Int),(read x :: Int, read y :: Int), read speed :: Double , convertOrientation orientation, read points :: Int, read lives :: Int)) t


getPlayer' :: PlayerState -> [String] -> (Player, [String])
getPlayer' ps (x:xs) = if x == "Pacman" then getPacman ps xs else getGhost ps xs


getPacman :: PlayerState -> [String] -> (Player, [String])
getPacman ps (timeMega:closed:mode:t) = (Pacman (PacState ps (read timeMega :: Double) (convertMouth closed) (convertMode mode)), t)


getGhost :: PlayerState -> [String] -> (Player, [String])
getGhost ps (x:t) = (Ghost (GhoState ps (if x == "Alive" then Alive else Dead)), t)

convertMouth :: String -> Mouth
convertMouth "Open" = Open
convertMouth "Closed" = Closed


convertMode :: String -> PacMode
convertMode "Dying" = Dying
convertMode "Mega" = Mega
convertMode "Normal" = Normal

convertToPlay :: [String] -> Play
convertToPlay (x:xs) = Move id orientation
                      where id = (read a1 :: Int)
                            orientation = convertOrientation (tail b1)
                            (a1,b1) = break (== ' ') x




convertOrientation :: String -> Orientation
convertOrientation orientation = case orientation of 
                                    "L" -> L
                                    "R" -> R
                                    "U" -> U
                                    "D" -> D