module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Test.QuickCheck.Modifiers
import Data.Maybe

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq , Read)

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isBetweenOneAndNine :: Maybe Int -> Bool
isBetweenOneAndNine (Just a) = a>0 && a<10
isBetweenOneAndNine Nothing = True

isSudoku :: Sudoku -> Bool
isSudoku sudoku = length su_rows == 9
               && map length su_rows == replicate 9 9
               && and [and (map isBetweenOneAndNine row) | row <- su_rows]
               where su_rows = rows sudoku

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks

isFilled :: Sudoku -> Bool
isFilled sudoku = not . and $ map (Nothing `elem`) su_rows
    where su_rows = rows sudoku

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen

convertCell :: Cell -> Char
convertCell (Just i)  = intToDigit i
convertCell Nothing   = '.'

convertRow :: Row -> String
convertRow row = map convertCell row

printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
    let converted = map convertRow (rows sudoku)
    putStrLn $ unlines converted

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku

convertCharToCell :: Char -> Cell
convertCharToCell c | c == '.'  = Nothing
                    | isDigit c = Just (digitToInt c) 
                    | otherwise = error "Not interpretable char."

readSudoku :: FilePath -> IO Sudoku
readSudoku filename = do 
    contents <- readFile filename
    let raw_rows = lines contents
    let sudoku = Sudoku $ map (map convertCharToCell) raw_rows
    if (isSudoku sudoku) 
        then return sudoku 
        else error "Malformed sudoku." 

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku

cell :: Gen Cell
cell = do
    let fillGen = (Just <$> choose (1, 9)) :: Gen Cell
    let emptyGen = return Nothing
    frequency [(8, emptyGen), (2, fillGen)]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> (vectorOf 9 (vectorOf 9 cell))

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
remove_nothing :: [Maybe Int] -> [Maybe Int]
remove_nothing [] = []
remove_nothing (Nothing:xs) = remove_nothing xs
remove_nothing (x:xs) = x:(remove_nothing xs)

isOkayBlock :: Block -> Bool
isOkayBlock block = nub block_without_nothing == block_without_nothing
    where block_without_nothing = remove_nothing block

-- * D2

row_blocks :: Sudoku -> [Block]
row_blocks = rows 

col_blocks :: Sudoku -> [Block]
col_blocks = transpose . row_blocks 

block_blocks :: Sudoku -> [Block]
block_blocks su = 
    [[ matrix!!x'!!y' | x' <- [x*3..x*3+2], y' <- [y*3..y*3+2]] | x <- [0..2], y <- [0..2]]
    where matrix = rows su

blocks :: Sudoku -> [Block]
blocks su = (row_blocks su) ++ (col_blocks su) ++ (block_blocks su) 

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths su = length all_blocks == 27 && 
                         and (map (\b -> 9 == length b) all_blocks)
                            where all_blocks = blocks su

-- * D3

isOkay :: Sudoku -> Bool
isOkay = and . map isOkayBlock . blocks 


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks su = [(r, c) | r <- [0..8], c <- [0..8], matrix!!r!!c == Nothing]
  where matrix = rows su

prop_blanks_allBlanks :: Sudoku -> Bool
prop_blanks_allBlanks su = and [matrix!!r!!c == Nothing | (r, c) <- blankList] 
  where blankList = blanks su
        matrix    = rows su

-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[]     !!= _     = []
(x:xs) !!= (0,y) = [y] ++ xs
(x:xs) !!= (i,y) = [x] ++ (xs !!= (i-1, y))

prop_bangBangEquals_correct :: [Int] -> (Int, Int) -> Bool
prop_bangBangEquals_correct [] (idx, val) = [] == converted
        where converted = ([] !!= (idx, val)) 
prop_bangBangEquals_correct xs (idx, val) | 0 <= idx && idx < length xs = 
        (take idx xs) ++ [val] ++ (drop (idx+1) xs) == converted
        | otherwise = xs == converted
        where converted = xs !!= (idx, val) 

-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update su (r,c) val | r >= 0 && r < 9 && c >= 0 && c < 9 = Sudoku $ (take r matrix) ++ [((matrix!!r) !!= (c, val))] ++ (drop (r+1) matrix)
                    | otherwise = su
  where matrix = rows su

prop_update_updated :: Sudoku -> (NonNegative Int, NonNegative Int)-> Cell -> Property 
prop_update_updated su (NonNegative r, NonNegative c) val = r < 9 && c < 9 ==> result!!r!!c == val
  where result = rows $ update su (r,c) val


------------------------------------------------------------------------------

-- * F1

allFineSudoku :: Sudoku -> Bool
allFineSudoku su = (isSudoku su) && (isOkay su)

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' su []      | isOkay su = Just su
                  | otherwise = Nothing 
solve' su (p:pos) | isOkay su = listToMaybe $ catMaybes [solve' (update su p fill) pos | fill <- [Just i | i <- [1..9]]]
                  | otherwise = Nothing

solve :: Sudoku -> Maybe Sudoku
solve su | allFineSudoku su  = solve' su $ blanks su 
         | otherwise         = Nothing 

-- * F2

readAndSolve :: FilePath -> IO () 
readAndSolve filename = do
    su <- readSudoku filename
    let solution = solve su
    if solution == Nothing
        then putStrLn "(no solution)"
        else printSudoku $ fromJust solution

-- * F3


isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solution su | isFilled solution && allFineSudoku solution = and [solutionMatrix!!i!!j == suMatrix!!i!!j | i <-[0..8], j <- [0..8], suMatrix!!i!!j /= Nothing ] 
                         | otherwise                                   = False
    where solutionMatrix = rows solution 
          suMatrix       = rows su

-- * F4
fewerChecks prop =
    quickCheckWith stdArgs{maxSuccess=30 } prop

prop_SolveSound :: Sudoku -> Property
-- prop_SolveSound su =  isJust solution ==> within timeout $ (fromJust solution) `isSolutionOf` su
--     where solution = solve su
--           timeout = 30000
prop_SolveSound su = within timeout $ isJust solution ==> (fromJust solution) `isSolutionOf` su
    where solution = solve su
          timeout = 60000000
