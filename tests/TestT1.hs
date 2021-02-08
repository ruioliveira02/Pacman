module TestT1 where


import Tarefa1
import Types
import System.IO.Unsafe

{-

>>> ghci TestT1 -i../src

-}


testsT1 :: [(Int, Int, Int)]
testsT1 = [(15,10,123), (30,30,45345), (21, 21, 423423), (42, 31, 2533254) ,(200,200,234234), (100,100, 12331)]





runT1Tests :: IO (Int, Int)
runT1Tests = do
    let (p,f) = unsafePerformIO $ runT1TestsAux testsT1
    putStrLn ((show p) ++ " passed, " ++ (show f) ++ " failed")
    return (p,f)




{-


        |  Auxiliary Functions for runT1Tests   |


-}


runT1TestsAux :: [(Int, Int, Int)] -> IO (Int, Int)
runT1TestsAux [] = do
    return (0,0)
runT1TestsAux ((w,h,seed):t) = do
    putStrLn ("Width: " ++ (show w))
    putStrLn ("Height: " ++ (show h))
    putStrLn ("Seed: " ++ (show seed))

    let maze = generateMaze w h seed

    putStrLn $ show (State maze [] 0)

    let valid = if (isValidMaze (w,h) maze) then "Test PASSED" else "Test FAILED"

    putStrLn ""
    putStrLn valid

    putStrLn ""
    putStrLn ""
    let (p,f) = unsafePerformIO $ runT1TestsAux t

    return (if (isValidMaze (w,h) maze) then (p + 1, f) else (p, f + 1))


isValidMaze :: (Int, Int) -> Maze -> Bool
isValidMaze (x,y) maze = areDimentionsCorrect (x, y) maze && areWallsCorrect (x, y) maze 0 && isGhostHouseCorrect (x, y) maze



areDimentionsCorrect :: (Int, Int) -> Maze -> Bool
areDimentionsCorrect (x, 0) [] = True
areDimentionsCorrect (x,0) _ = False
areDimentionsCorrect (x,y) (m:ms) = (length m == x) && areDimentionsCorrect (x, y - 1) ms



areWallsCorrect :: (Int, Int) -> Maze -> Int -> Bool
areWallsCorrect _ [] _  = True
areWallsCorrect (w,h) (m:ms) i | i == 0 || i == h - 1                           = and (map (== Wall) m) && rem
                               | i == div h 2 || (even h && i == div (h - 1) 2) = (head m /= Wall && last m /= Wall) && rem
                               | otherwise                                      = (head m == Wall && last m == Wall) && rem
                               where rem = areWallsCorrect (w,h) ms (i + 1)



isGhostHouseCorrect :: (Int, Int) -> Maze -> Bool
isGhostHouseCorrect (w,h) maze = isMazeEqual ( subMaze (div (w - getHouseLength w - 2) 2, (div (h - 5) 2)) (getHouseLength w + 2, 5)  maze) (if even w then ghostHouseEven else ghostHouseOdd)



subMaze :: (Int, Int) -> (Int, Int) -> Maze -> Maze
subMaze (_, 0) (_, 0) _ = []
subMaze (x, 0) (w, h) (m:ms) = (take w (drop x m)) : subMaze (x, 0) (w, h - 1) ms
subMaze (x, y) (w,h) (m:ms) = subMaze (x, y - 1) (w,h) ms


isMazeEqual :: Maze -> Maze -> Bool
isMazeEqual [] [] = True
isMazeEqual (x:xs) (y:ys) = and (zipWith (==) x y) && isMazeEqual xs ys
isMazeEqual _ _ = False --different sizes




ghostHouseEven :: Maze
ghostHouseEven = [
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall ,Empty],
    [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
    [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ,Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]


ghostHouseOdd :: Maze
ghostHouseOdd = [
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall ,Empty],
    [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty],
    [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall ,Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]