module TestT4 where


import TestT2


import FileUtils
import Types
import Tarefa4
import System.IO.Unsafe


{-

>>> ghci TestT4 -i../src

-}

testsT4 = [("mazes/Tarefa4/test_input.txt", "mazes/Tarefa4/test_output.txt"),
           ("mazes/Tarefa4/test_input2.txt", "mazes/Tarefa4/test_output2.txt"),
           ("mazes/Tarefa4/cross_tunnel_input.txt", "mazes/Tarefa4/cross_tunnel_output.txt"),
           ("mazes/Tarefa4/eat_ghost_alive_input.txt", "mazes/Tarefa4/eat_ghost_alive_output.txt"),
           ("mazes/Tarefa4/ghost_dead_odd_input.txt", "mazes/Tarefa4/ghost_dead_odd_output.txt"),
           ("mazes/Tarefa4/ghost_dead_even_input.txt", "mazes/Tarefa4/ghost_dead_even_output.txt")]


runT4Tests :: IO (Int, Int)
runT4Tests = do
    putStrLn "Running Tarefa 4 Tests"
    putStrLn "-------------------------------"
    res <- runT4TestsAux testsT4

    putStrLn ((show (fst res)) ++ " tests passed, " ++ (show (snd res)) ++ " failed.")

    putStrLn "" 
    return res


runT4TestsAux :: [(String, String)] -> IO (Int, Int)
runT4TestsAux [] = return (0,0)
runT4TestsAux (h:t)   = do
                        ac    <- runT4Test h
                        (p,f) <- runT4TestsAux t

                        let res = (if ac then (p+1,f) else (p, f+1))

                        return res

runT4Test :: (String, String) -> IO Bool
runT4Test (input, output) = do
                    ((step, original), (step2, expected)) <- loadT4Test (input, output)

                    let result = passTimeAux step original

                    putStrLn ("Running Test " ++ input)

                    putStrLn "Given:"
                    putStrLn ""
                    putStrLn ("Step: " ++ show step)
                    putStrLn $ show original
                    putStrLn ""

                    putStrLn "Expected:"
                    putStrLn $ show expected

                    putStrLn ""

                    putStrLn "Result:"
                    putStrLn $ show result

                    putStrLn ""

                    
                    let ac =  (result == expected)
                    let str = (if ac then "Test PASSED" else "Test FAILED")

                    putStrLn str
                    putStrLn ""

                    return ac

loadT4Test :: (String, String) -> IO ((Int,State), (Int, State))
loadT4Test (input, output) = do
                    contentFirst  <- readFile input
                    contentSecond <- readFile output

                    let original = convertToT4Test (lines contentFirst)
                        expected = convertToT4Test (lines contentSecond)

                    return (original, expected)



                    
convertToT4Test :: [String] -> (Int, State)
convertToT4Test (h:t) = (read h :: Int, convertToState t)