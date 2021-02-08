module TestT3 where

import System.IO.Unsafe

import Types
import FileUtils
import Tarefa3

{-

>>> ghci TestT3 -i../src

-}



-- Generic Maze
testsT3 = ["mazes/Tarefa3/1.txt",
           "mazes/Tarefa3/2.txt",
           "mazes/Tarefa3/3.txt",
           "mazes/Tarefa3/4.txt",
           "mazes/Tarefa3/5.txt",
           "mazes/Tarefa3/6.txt",
           "mazes/Tarefa3/7.txt"]


runT3TestsAux :: [String] -> IO (Int, Int)
runT3TestsAux [] = do   
    return (0,0)
runT3TestsAux (x:xs) = do
    putStrLn ("Running testCase from file " ++ x)
    content <- readFile x
    let (maze, instExpected) = convertTarefa3Test (lines content)
    putStrLn $ show (State maze [] 0)
    putStrLn ""
    let res = compactMaze maze
    putStrLn "Expected"
    putStrLn $ show $ instExpected
    putStrLn "Result"
    putStrLn $ show res
    putStrLn ""
    putStrLn ""
    putStrLn ""

    let out = (if instExpected == res then "Test PASSED" else "Test FAILED")
    putStrLn out

    let (p,f) = unsafePerformIO $ runT3TestsAux xs
    return (if instExpected == res then (p + 1, f) else (p, f + 1))

runT3Tests :: IO (Int, Int)
runT3Tests = do
    let (p,f) = unsafePerformIO $ runT3TestsAux testsT3

    putStrLn ((show p) ++ " passed, " ++ (show f) ++ " failed")

    return (p,f)
    




convertTarefa3Test :: [String] -> (Maze, Instructions)
convertTarefa3Test lstr = let (mz, inst) = break (== "--ans--") lstr
                          in (fst (convertLinesToPiece mz 0 []), convertToInstructions (tail inst))





convertToInstructions :: [String] -> Instructions
convertToInstructions lstr = map (convertLineToInstruction) lstr


convertLineToInstruction :: String -> Instruction
convertLineToInstruction str | take 8 str == "Instruct" = Instruct (convertInstruct (drop 9 str))
                             | otherwise                = Repeat (read (drop 6 str) :: Int)




removeAll :: [Char] -> String -> String -> [String]
removeAll _ "" "" = []
removeAll _ acc "" = [acc]
removeAll list acc (x:xs) | elem x list = if acc == "" then removeAll list "" xs else acc : removeAll list "" xs
                          | otherwise   = removeAll list (acc ++ [x]) xs


convertInstruct :: String -> [(Int, Piece)]
convertInstruct str = convertInstructAux (removeAll ['[', ']', ',', '(', ')'] "" str)



convertInstructAux :: [String] -> [(Int, Piece)]
convertInstructAux [] = []
convertInstructAux (x:y:t) = (i, p) : convertInstructAux t
                           where i = (read x :: Int)
                                 p = fst $ charToPiece (head y) 0 0 []