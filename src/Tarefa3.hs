{- |
    Module: Tarefa3

    Module which compacts a maze into a set of 'Instruction'.

    This module has no application on the remaining project.
-}



module Tarefa3 where


import Types

-- | Partitions a corridor based on a given piece. It returns a tuple, representing the number of consecutive elements in the beggining of the list equal to 
-- the given piece, and the remaining list, i.e., the corridor starting at the first element different than the given piece. 
-- It works in the same way as the span function, only that the first element it returns is an Int corresponding to the length of the list instead of the
--actual list
--
-- == /Examples/
--
-- >>> partitionCorridor Wall [Wall, Wall, Empty, Food Little, Food Little]
-- >>> (2,[ ,.,.])
--
-- >>> partitionCorridor Empty [Empty, Food Little, Wall, Empty]
-- >>> (1,[.,#, ])
partitionCorridor :: Piece -- ^ The piece to compare the others against
    -> Corridor -- ^ The given corridor
    -> (Int, Corridor) -- ^ A tuple consisting of the number of consecutives elements in corridor equal to the given piece, and the remaining corridor
partitionCorridor _ [] = (0, [])
partitionCorridor piece (c:cs)
                            | c == piece = (a + 1, b)
                            | otherwise = (0, (c:cs))
                            where (a, b) = partitionCorridor piece cs
                            

-- | Compacts a corridor to a list of '(Int, Piece)', but not into an 'Instruction'
--
-- == /Examples/
--
-- >>> compactCorridor [Wall, Wall, Empty, Food Little, Food Little]
-- >>> [(2,#),(1, ),(2,.)]
--
-- >>> compactCorridor [Empty, Empty, Food Little, Wall, Empty]
-- >>> [(2, ),(1,.),(1,#),(1, )]
compactCorridor :: Corridor -- ^ The uncompressed corridor
    -> [(Int, Piece)] -- ^ The compressed corridor, as a list of (Int, Piece) and not an Instruction
compactCorridor [] = []
compactCorridor (p:ps) = (equal, p) : compactCorridor diff
                        where (equal, diff) = partitionCorridor p (p:ps)




-- | Replaces all identical instructions with a Repeat instruction
--
-- == /Examples/
--
-- >>> removeEqualInstructions [Instruct [(1,Wall), (2, Empty)], Instruct [(3,Empty)], Instruct [(3,Empty)]]
-- >>> [Instruct [(1,#),(2, )],Instruct [(3, )],Repeat 1]
--
-- >>> removeEqualInstructions [Instruct [(2,Wall), (2, Empty)], Instruct [(3,Empty)], Instruct [(4,Empty)], Instruct [(2,Wall), (2, Empty)], Instruct [(2,Wall), (2,Empty)]]
-- >>> [Instruct [(2,#),(2, )],Instruct [(3, )],Instruct [(4, )],Repeat 0,Repeat 0]

removeEqualInstructions :: Instructions -- ^ The original set of instructions
    -> Instructions -- ^ The compressed set of instructions
removeEqualInstructions inst = removeEqualInstructionsAux inst 0



-- | Auxiliary function to 'removeEqualInstructions'.
--   This function uses an accumulator to know what is the index of the head of the list,
--   in order to feed it to the 'removeInstructionsEqualTo', as it requires the index value
--
-- == /Example/
--
-- >>> removeEqualInstructionsAux [Instruct [(1,Wall), (2, Empty)], Instruct [(3,Empty)], Instruct [(3,Empty)]] 0
-- >>> [Instruct [(1,#),(2, )],Instruct [(3, )],Repeat 1]

removeEqualInstructionsAux :: Instructions -- ^ The original set of instructions
    -> Int -- ^ The index of the current head of the list
    -> Instructions -- ^ The new compressed set of instructions
removeEqualInstructionsAux [] _ = []
removeEqualInstructionsAux (x:xs) n = x : removeEqualInstructionsAux (removeInstructionsEqualTo (x,n) xs) (n + 1)





-- | Replaces all 'Instruction' equal to the given 'Instruction' with a 'Repeat' 'Instruction'
--
-- == /Example/
--
-- >>> removeInstructionsEqualTo (Instruct [(3, Empty)], 0) [Instruct [(1,Wall), (2, Empty)], Instruct [(3,Empty)], Instruct [(3,Empty)]]
-- >>> [Instruct [(1,#),(2, )],Repeat 0,Repeat 0]

removeInstructionsEqualTo :: (Instruction, Int) -- ^ The instruction to compare the others against, and its respective index
    -> Instructions -- ^ The original set of instructions
    -> Instructions -- ^ The new set of instructions with all instances of the given instruction replaced with 'Repeat i'
removeInstructionsEqualTo _ [] = []
removeInstructionsEqualTo (Repeat _, _) list = list -- We are not counting Repeat instructions
removeInstructionsEqualTo (inst,index) (x:xs)
                                    | x == inst = (Repeat index) : remaining
                                    | otherwise = x : remaining
                                    where remaining = removeInstructionsEqualTo (inst,index) xs



-- | Compacts a maze into a set of instructions
--
-- == /Example/
--
-- >>> compactMaze testMaze -- consider testMaze the same as defined in Tarefa2.hs
-- >>> [Instruct [(8,#)],Instruct [(1, ),(1,*),(3,.),(1,M),(1,.),(1, )],Instruct [(1, ),(2,.),(1,<),(1,.),(1,#),(1,.),(1, )],Repeat 0]

compactMaze :: Maze -- ^ The given maze
    -> Instructions -- ^ The maze in instruction form
compactMaze maze = removeEqualInstructions (map (\x -> (Instruct (compactCorridor x))) maze)