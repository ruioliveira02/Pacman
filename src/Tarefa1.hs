{- |
    Module: Tarefa1

    Module which generates valid mazes as per the task statement.

    A maze is considered valid if and only if it is at least of size 15x10,
    has a tunnel connecting the two ends of it in the middle row (or middle rows, if of
    even height), and has the ghost house placed in the middle of it.

-}


module Tarefa1 where

import System.Random
import Types

-- | Generates a list of n pseudo-randoms integers based on the given seed
--
-- == /Examples/
--
-- >>> generateRandoms 5 123
-- [43,5,7,30,50]
--
-- >>> generateRandoms 7 4124354
-- [89,72,69,7,20,33,49]

generateRandoms :: Int -- ^ The length of the wanted list
    -> Int -- ^ The given seed
    -> [Int] -- ^ The resulting list of numbers
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (0,99) gen -- takes the first n elements from an infinite series of random numbers between 0-99


-- | Given a list of pseudo-random integers, returns a 'Corridor' 
--  based on the probabilities of each type of Piece
-- 
-- == /Examples/
--
-- >>> convertIntListToCorridor [3,45,98,1,0,4]
-- [o,.,#,.,.,.]
--
-- >>> convertIntListToCorridor [56,9,12,9,75,90,34]
-- [.,.,.,.,#,#,.]
convertIntListToCorridor :: [Int] -- ^ List of pseudo-random ints
    -> Corridor -- ^ Corresponding corridor
convertIntListToCorridor [] = []
convertIntListToCorridor (x:xs)
                            | x == 3   = Food Big : remaining
                            | x < 70   = Food Little : remaining
                            | x <= 99   = Wall : remaining
                            where remaining = convertIntListToCorridor xs


-- | Returns a corridor of length len consisting of only 'Wall'
--
-- == /Examples/
--
-- >>> generateWall 5
-- [#,#,#,#,#]
generateWall :: Int -- ^ The length of the desired corridor
    -> Corridor -- ^ Resulting corridor
generateWall 0 = []
generateWall len = Wall : generateWall (len - 1)


-- | Returns the length of the ghost house including walls (8 if length is even, 9 otherwise)
-- 
-- == /Examples/
--
-- >>> getHouseLength 20
-- >>> 8
--
-- >>> getHouseLength 31
-- >>> 9
getHouseLength :: Int -- ^ The width of the maze
    -> Int -- ^ The width of the ghostHouse
getHouseLength n | mod n 2 == 0 = 8
                 | otherwise = 9



-- | Fixes the corridors directly above and below the house (see restrictions)
--
-- == /Example/
--
-- >>> generateArroundHouse 20 (generateWall 20)
-- >>> [#,#,#,#,#, , , , , , , , , , ,#,#,#,#,#]
generateArroundHouse :: Int -- ^ The width of the maze
    ->  Corridor -- ^ The original corridor
    -> Corridor -- ^ The new corridor
generateArroundHouse n corridor = let aux = (div (n - 4 - getHouseLength n) 2) + 1
                              in (take aux corridor) ++ (take (getHouseLength n + 2)
                                     (repeat Empty)) ++ (drop (n - aux) corridor)

-- | Fixes the 'Corridor' that passes through the ghost house
--
--   It also makes sure that the tunnel is clear, i.e, that it is possible to reach the ghost
--   house from the tunnel. It does this by replacing all walls from the generated maze with Empty
--
-- == /Example/
--
-- >>> generateInsideHouse 20 (generateWall 20)
-- >>> [ , , , , , ,#, , , , , , ,#, , , , , , ]
generateInsideHouse :: Int -- ^ The width of the maze
    -> Corridor -- ^ The original corridor
    -> Corridor -- ^ The new corridor
generateInsideHouse n corridor = let aux = (div (n - 4 - getHouseLength n) 2)
                             in [Empty] ++ (map (\x -> if x == Wall then Food Little else x) (take aux (tail corridor)))
                                                  ++ [Empty] ++ [Wall]
                                                  ++ (take (getHouseLength n - 2) (repeat Empty))
                                                  ++ [Wall] ++ [Empty] ++ 
                                                 (map (\x -> if x == Wall then Food Little else x) 
                                                      (drop (n - 1 - aux) (init corridor))) ++ [Empty]


-- | Generates the 'Corridor' containing the door to the ghost house
--
-- == /Example/
--
-- >>> generateUpperHouse 20 (generateWall 20)
-- >>> [#,#,#,#,#, ,#,#,#, , ,#,#,#, ,#,#,#,#,#]
generateUpperHouse :: Int -- ^ The width of the maze
    -> Corridor -- ^ The original corridor
    -> Corridor -- ^ The new corridor
generateUpperHouse n corridor = let aux = (div (n - 4 - getHouseLength n) 2) + 1
                             in (take aux corridor) ++ [Empty] ++ 
                                    (take 3 (repeat Wall)) ++ 
                                    (take (getHouseLength n - 6) (repeat Empty)) 
                                    ++ (take 3 (repeat Wall)) ++ [Empty] ++ (drop (n - aux) corridor)

-- | Generates the 'Corridor' containing the bottom of the ghost house.
--
-- Requires both width and height of the maze
-- 
-- == /Example/
--
-- >>> generateBottomHouse 20 10 (generateWall 20)
-- >>> [ ,#,#,#,#, ,#,#,#,#,#,#,#,#, ,#,#,#,#, ]
generateBottomHouse :: Int -- ^ The width of the maze
    -> Int -- ^ The height of the maze
    -> Corridor -- ^ The original corridor
    -> Corridor -- ^ The new corridor
generateBottomHouse n m corridor = [peca] ++ (take aux (tail corridor)) ++ [Empty] 
                                        ++ (take (getHouseLength n) (repeat Wall))
                                        ++ [Empty] ++ (drop (n - 1 - aux) (init corridor)) ++ [peca]
                                    where peca = if (mod m 2 == 0) then Empty else Wall
                                          aux = (div (n - 4 - getHouseLength n) 2)


-- | Generates a 'Maze' consisting of only walls
--
-- == /Example/
--
-- >>> generateStartingMaze 5 2
-- >>> [[#,#,#,#,#], [#,#,#,#,#]]
generateStartingMaze :: Int -- ^ The width of the maze
    -> Int -- ^ The height of the maze
    -> Maze -- ^ The resulting maze
generateStartingMaze _ 0  = []
generateStartingMaze n m = generateWall n : generateStartingMaze n (m - 1)

-- | Given a list, returns the list obtained by replacing the x-th element (base 0) with the given element
--
-- == /Example/
--
-- >>> changeVal1D [1,2,3] 1 0
-- >>> [1,0,3]
changeVal1D :: [a] -- ^ The original list
    -> Int -- ^ Position of the element to change
    -> a -- ^ The new element
    -> [a] -- ^ The resulting list
changeVal1D (x:xs) 0 a = a : xs
changeVal1D (x:xs) n a = x : changeVal1D xs (n - 1) a

-- | Given a list of lists, returns the list obtained by replacing the element at position (x,y),
--   i.e, the x-th element of the y-th list (indices base 0), with the given element
--
-- == /Example/
--
-- >>> changeVal [[1,2], [3,4]] (1,1) 0
-- >>> [[1,2], [3,0]]
changeVal :: [[a]] -- ^ The original list of lists
    -> (Int, Int) -- ^ The coordinates (x,y) (base 0) of the value to replace
    -> a -- ^ The new value
    -> [[a]] -- ^ The resulting list
changeVal (y:t) (x, 0) piece = changeVal1D y x piece : t
changeVal (w:t) (x, y) piece = w : changeVal t (x, y - 1) piece




-- | Given a list of lists, returns the element at position (x,y),
--   i.e, the x-th element of the y-th list (indices base 0)
--
-- == /Example/
--
-- >>> get [[1,2], [3,4]] (1,1)
-- 4
get :: [[a]] -- ^ The list of lists
    -> (Int, Int) -- ^ The coordinates (x,y) (base 0) of the wanted element
    -> a -- ^ The wanted element
get ((x:xs):t) (0,0) = x
get ((y:xs):t) (x, 0) = get (xs:t) (x - 1, 0)
get (w:t) (x, y) = get t (x, y - 1)
get  [] (x, y) = error ((show x) ++ " " ++ (show y))


-- | Returns the first element of a triple
--
-- == /Example/
--
-- >>> first (1,2,3)
-- >>> 1
first :: (a,b,c) -- ^ The given triple
    -> a -- ^ The first element
first (a,b,c) = a



-- | Given a 'Maze', the coordinates of the current square, the size of the maze, the list of visited squares and the list of pieces to choose from, returns the piece to place on that square
--   (if square out of bounds or already visited, returns 'Nothing', otherwise, 'Just Piece'), as well as the pieces with have not been used, so they can be selected in the future
--
-- == /Examples/
--
-- >>> used = take 20 (repeat (take 20 (repeat False)))
-- >>> selectSquare (generateStartingMaze 20 20) (5,5) (20,20) used [Wall, Empty, Wall, Food Little]
-- >>> (Just #,[ ,#,.])
selectSquare :: Maze -- ^ The given maze
      -> (Int, Int) -- ^ The coordinates of the given square
      -> (Int, Int) -- ^ The size of the maze (width, height)
      -> [[Bool]] -- ^ The list of visited squares
      -> [Piece] -- ^ The list of pieces to choose from
      -> (Maybe Piece, [Piece]) -- ^ The resulting piece and leftover pieces
selectSquare maze (x, y) (n,m) used (p:ps)
                            | get used (x,y)                               = (Nothing, p:ps)
                            | x <= 0 || x >= n - 1 || y <= 0 || y >= m - 1 = (Nothing, p:ps)
                            | otherwise                                    = (Just p, ps)




-- | Given a 4-uple of potential pieces, returns a valid 4-uple, i.e., returns a 4-uple in where at least one of the component is either a Food or Empty.
--
-- This makes sure that the DFS will continue until all squares have been visited and that it will not stop because it decided to surround itself in walls. 
--  
-- ==/Examples/
--
{- |

>>> truncate' (Nothing, Nothing, Nothing, Nothing) = (Nothing, Nothing, Nothing, Nothing)
>>> truncate' (Just Wall, Just Wall, Just Wall, Nothing) = (Just (Food Little), Just (Food Little), Just (Food Little), Nothing)
>>> truncate' (Just Wall, Just Wall, Just Wall, Just Wall) = (Just (Food Little), Just (Food Little), Just (Food Little), Just (Food Little))
>>> truncate' (Just Empty, Just Empty, Just Empty, Just (Food Little)) = (Just Empty, Just Empty, Just Empty, Just (Food Little))

-}
truncate' :: (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece) -- ^ The original set of pieces
      -> (Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece) -- ^ The new set of pieces, containing at least one Food Little
truncate' (a,b,c,d) | length (filter (\x -> x == Nothing || x == Just Wall) l) == 4
                         = toTuple $ map mapper l
                    | otherwise = (a,b,c,d)
                         where l = [a,b,c,d]
                               mapper (Just Wall) = Just (Food Little)
                               mapper x = x
                               toTuple [a2,b2,c2,d2] = (a2,b2,c2,d2)



-- | Given a maze, a position in said maze, a list of visited squares and a list of pieces to choose from, returns the pieces to replace all neighbours of the current square with
--
--   The choice for each component is done by the  'selectSquare' function
--
-- == /Examples/
--
-- >>> used = take 20 (repeat (take 20 (repeat False)))
-- >>> selectMove (generateStartingMaze 20 20) (3,3) (20,20) used [Wall, Empty, Wall, Food Little]
-- >>> ((Just .,Just #,Just  ,Just #),[])
selectMove :: Maze -- ^ The given maze
      -> (Int, Int) -- ^ The coordinates (x,y) of the square
      -> (Int, Int) -- ^ The size of the maze (width, height)
      -> [[Bool]] -- ^ The list of visited squares
      -> [Piece] -- ^ The list of pieces to choose from
      -> ((Maybe Piece, Maybe Piece, Maybe Piece, Maybe Piece),[Piece]) -- ^ The resulting moves (if not possible to move to neighbour, 'Nothing', otherwise, 'Just Piece') and leftover pieces
selectMove maze (x,y) (n,m) used pieces = (truncate' (a,b,c,d), a1)
                                                where (a,a1) = selectSquare maze (x, y - 1) (n,m) used b1
                                                      (b,b1) = selectSquare maze (x, y + 1) (n,m) used c1
                                                      (c,c1) = selectSquare maze (x - 1, y ) (n,m) used d1
                                                      (d,d1) = selectSquare maze (x + 1, y) (n,m) used pieces



-- | Given a starting maze consisting of only walls, a list of pieces and a piece, constructs a maze where every square is reachable from any other square (excluding walls, for obvious reasons)
--
--   As the name implies, this function transverses the maze depth-first. When it reaches a square, if it has not been visited before and is within bounds, replace the wall with the value of the piece argument
--   and mark it as visited. It then send the list of pieces to the 'selectMove' function, which will select the pieces to place on the other neighbouring squares which haven't been visited yet.
--
--   After this, it visits each of the neighbouring squares and replaces their Wall with the selected piece
--
--
-- == /Example/
--
-- >>> maze = generateStartingMaze 20 20
-- >>> used = take 20 (repeat (take 20 (repeat False)))
-- >>> pieces = convertIntListToCorridor (generateRandoms 600 123)
-- >>> dfs maze (1,1) (20,20) used pieces (Food Little)
-- >>> ([[#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#],
--       [#,.,.,.,#,.,.,.,.,#,#,#,.,.,.,.,.,#,.,#],
--       [#,.,.,.,.,.,.,#,.,.,.,.,.,#,.,.,.,.,.,#],
--       [#,.,.,#,.,.,.,.,.,.,.,.,.,.,.,#,#,.,.,#],
--       [#,#,.,.,.,#,#,.,.,#,.,.,.,.,.,.,.,.,.,#],
--       [#,.,#,#,.,.,.,.,.,.,.,.,.,.,.,.,.,#,.,#],
--       [#,.,.,.,.,.,#,.,.,.,.,#,.,.,#,.,.,.,.,#],
--       [#,.,.,.,.,.,#,#,.,.,.,.,#,.,.,#,#,.,.,#],
--       [#,.,.,.,.,.,.,.,#,.,.,.,#,#,.,.,.,.,.,#],
--       [#,.,#,.,#,.,.,.,.,.,.,.,.,.,.,.,.,.,.,#],
--       [#,.,.,#,#,#,.,#,.,#,.,.,.,.,.,.,.,.,.,#],
--       [#,#,.,#,#,.,.,#,.,.,.,.,.,.,.,.,#,o,.,#],
--       [#,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,#],
--       [#,.,#,.,.,.,.,.,.,.,#,.,.,.,.,#,.,.,.,#],
--       [#,.,.,.,.,.,.,#,#,.,.,.,.,.,.,.,.,.,.,#],
--       [#,.,.,.,.,.,.,#,#,.,.,#,#,.,.,.,.,.,.,#],
--       [#,.,#,.,.,.,.,.,.,.,.,.,.,#,.,#,.,.,.,#],
--       [#,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,.,#],
--       [#,.,.,#,#,.,.,#,.,.,.,.,.,.,.,#,.,.,.,#],
--       [#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#]],
--      [[False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False],
--       [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]],
--      [.,#,.,#,.,.,.,.,.,.,#,.,.,.,.,#,#,.,.,#,.,.,.,.,.,#,.,.,.,.,.,#,.,.,.,.,.,.,#,#,#,.,#,.,.,#,#,.,#,#,.,.,.,.,#,#,.,.,.,#,.,#,.,.,.,#,.,#,#,.,#,.,#,.,.,.,.,.,.,#,.,#,.,.,#,#,.,#,.,.,.,.,o,#,.,.,.,#])


dfs :: Maze -- ^ The starting maze (at first only containing walls)
      -> (Int, Int) -- ^ The coordinates (x,y) of the current square
      -> (Int,Int)  -- ^ The width and height of the maze
      -> [[Bool]] -- ^ The list of which squares have been visited (at first all values are False)
      -> [Piece] -- ^ The list of pieces to choose from
      -> Piece -- ^ The piece to replace current square with
      -> (Maze, [[Bool]], [Piece]) -- ^ The resulting maze, the list of used squares and the leftover pieces for future choices
dfs maze (x,y) (n,m) used (p:ps) piece             
                            | x <= 0 || x >= n - 1 || y <= 0 || y >= m - 1 = (maze, used, p:ps)
                            | get used (x,y)  = (maze, used, p:ps)
                            | piece == Wall       = ((changeVal maze (x,y) piece), w, p:ps)
                            | otherwise = case x4 of
                                        Just x44 -> dfs a1 (x + 1, y) (n,m) a2 a3 x44
                                        Nothing  -> (a1, a2, a3)
                                where ((x1,x2,x3,x4), rp) = selectMove maze (x,y) (n,m) used (p:ps)                               
                                      w = (changeVal used (x,y) True)
                                      (a1,a2, a3) = case x3 of
                                                        Just x33 -> dfs b1 (x - 1, y) (n,m) b2 b3 x33
                                                        Nothing  -> (b1,b2,b3)
                                      (b1,b2, b3) = case x2 of
                                                        Just x22 -> dfs c1 (x, y + 1) (n,m) c2 c3 x22 
                                                        Nothing  -> (c1, c2, c3)
                                      (c1,c2, c3) = case x1 of
                                                        Just x11 -> dfs (changeVal maze (x,y) piece) (x, y - 1) (n,m) w rp x11
                                                        Nothing  -> ((changeVal maze (x,y) piece), w, rp)



-- | Given a maze surrounded by walls without any structures, returns the given maze with the ghost house and tunnel inserted
--
-- It iterates through each corridor, and based on the index of the current corridor, selects the appropriate structure to add
--
-- == /Example/
--
-- >>> fixMaze (20, 7) 1 (generateStartingMaze 20 7)
-- >>> [[#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#],
--      [#,#,#,#,#, , , , , , , , , , ,#,#,#,#,#],
--      [#,#,#,#,#, ,#,#,#, , ,#,#,#, ,#,#,#,#,#],
--      [ ,.,.,.,., ,#, , , , , , ,#, ,.,.,.,., ],
--      [#,#,#,#,#, ,#,#,#,#,#,#,#,#, ,#,#,#,#,#],
--      [#,#,#,#,#, , , , , , , , , , ,#,#,#,#,#],
--      [#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#,#]]

fixMaze :: (Int, Int) -- ^ Width and height of the maze
      -> Int -- ^ Index of current corridor (base 1)
      -> Maze -- ^ The original maze without structures
      -> Maze -- ^ The resulting maze with structures
fixMaze _ _ [] = []
fixMaze (n,m) i (c:cs) 
                    | i == 1 = c : remaining
                    | i == (div (m + 5) 2) = (generateArroundHouse n c) : remaining
                    | i == (div (m + 3) 2) = (generateBottomHouse n m c) : remaining
                    | i == (div (m + 1) 2) = (generateInsideHouse n c) : remaining
                    | i == (div (m - 1) 2) = (generateUpperHouse n c) : remaining
                    | i == (div (m - 3) 2) = (generateArroundHouse n c) : remaining
                    | i == m = c : remaining
                    | otherwise = c : remaining
                    where remaining = fixMaze (n,m) (i + 1) cs



-- | Generates a maze of the given size based on the given seed
--
-- It works by first generating a maze surrounded by walls without any structures by calling 'dfs' and then by adding the ghost house and tunnel by calling 'fixMaze'
--
-- == /Example/
-- >>> generateMaze 15 15 342
-- >>> [[#,#,#,#,#,#,#,#,#,#,#,#,#,#,#],
--      [#,.,.,.,.,.,.,.,#,#,.,.,.,.,#],
--      [#,.,.,#,.,.,#,.,.,.,.,.,#,.,#],
--      [#,.,#,#,.,.,.,.,.,.,#,.,.,.,#],
--      [#,.,.,.,.,.,.,.,.,.,.,.,.,.,#],
--      [#,., , , , , , , , , , , ,.,#],
--      [#,#, ,#,#,#, , , ,#,#,#, ,.,#],
--      [ ,., ,#, , , , , , , ,#, ,., ],
--      [#,., ,#,#,#,#,#,#,#,#,#, ,.,#],
--      [#,., , , , , , , , , , , ,#,#],
--      [#,.,.,.,.,.,.,.,.,.,.,.,.,.,#],
--      [#,#,.,.,.,.,.,.,.,.,.,#,.,.,#],
--      [#,.,.,.,.,.,.,.,#,#,.,.,.,.,#],
--      [#,.,.,.,#,.,.,.,.,.,.,.,.,.,#],
--      [#,#,#,#,#,#,#,#,#,#,#,#,#,#,#]]

generateMaze :: Int -- ^ Width of the maze
      -> Int -- ^ Height of the maze
      -> Int -- ^ The seed used to generate random numbers
      -> Maze -- ^ The resulting maze
generateMaze n m seed = fixMaze (n,m) 1 
                            (first (dfs (generateStartingMaze n m) (1,1) (n,m) 
                                    (take m (repeat (take n (repeat False))))
                                     (convertIntListToCorridor (generateRandoms (4*n*m) seed)) (Food Little)))