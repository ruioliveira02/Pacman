module Tests where

import System.IO.Unsafe

import TestT1
import TestT2
import TestT3
import TestT4

testAll :: IO ()
testAll = do
    (p1, f1) <- runT1Tests
    (p2, f2) <- runT2Tests
    (p3, f3) <- runT3Tests
    (p4, f4) <- runT4Tests
    
    putStrLn ((show (p1 + p2 + p3 + p4)) ++ " passed, " ++ (show (f1 + f2 + f3 + f4)) ++ " failed")