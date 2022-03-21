import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Point = (Int, Int)
type Polyomino = [Point]
monomino = [(0,0)]
monominoes = [monomino]

minimalPoint :: Polyomino -> Point  --find minimal coords of polyomino
minimalPoint [] = (0,0)
minimalPoint (a:as) = foldl (\(x, y) (mx, my) -> (min x mx, min y my)) a as


normalise :: Polyomino -> Polyomino     --align edges of polyomino with x and y axes
normalise a = map (\(x, y) -> (x - minx, y - miny)) a
        where (minx, miny) = minimalPoint a


normalise2 :: Polyomino -> Polyomino    --sort the normalised polyominoes
normalise2 = sort.normalise


distinct :: (Ord a) => [a] -> [a]       --erase duplicate polyominoes
distinct = (Set.toList).(Set.fromList)


adjacentPointList :: Point -> [Point]   --search for coordinates of possible new polyominoes
adjacentPointList (x, y) = [(x, y - 1),
                         (x, y + 1),
                         (x - 1, y),
                         (x + 1, y)]


newPointList :: Polyomino -> [Point]    --get list of possible coords for new plmns
newPointList a = let notInList = filter (`notElem` a) in 
    (distinct.(notInList.(concat.(map adjacentPointList)))) a


newPolyominoList :: Polyomino -> [Polyomino]    --get list of new plmns
newPolyominoList a = (distinct.(map (normalise2.(:a)))) (newPointList a)


generate :: Int -> [Polyomino]  --generate polyominoes
generate 0 = []
generate 1 = monominoes
generate n = (distinct.(concat.(map (newPolyominoList)))) (generate (n-1))


findMax :: Polyomino -> Int     --find max coordinate value
findMax [] = 0
findMax ((a,b):as) = foldl (\m (x, y) -> if m > max x y then m else max x y) 0 ((a,b):as)


area :: Int -> [[Point]]        --generate 2d plane of possible points
area n = [[(a,b) | b <- [0..n]] | a <- [0..n]]


mapToVisual :: [[Point]] -> Polyomino -> [[Char]]       --map polyomino to 2d plane
mapToVisual [] a = [[' ']]
mapToVisual n a = map (map (\(x,y) -> if (x,y) `elem` a then '*' else ' ')) n


printPoly :: Polyomino -> [String]      --return polyomino as a list of strings
printPoly a = mapToVisual (area (findMax a)) a


printMap :: [[Char]] -> IO ()   --print single polyomino 
printMap [] = putStrLn ['\n']
printMap (p:ps) = do
    putStrLn p
    printMap ps


printList :: [[[Char]]] -> IO ()    --print multiple polyominoes
printList [] = return ()
printList (l:ls) = do
    printMap l
    printList ls


pgen :: Int -> IO ()   --print all polyominoes of rank n
pgen n = printList (map (printPoly) (generate n))


--Example input and output:
-- pgen 2
-- **
  


-- * 
-- * 

-------------
--pgen 3
-- ***
   
   


-- **
-- * 


-- **
--  *


-- * 
-- **


-- *  
-- *  
-- *  


--  *
-- **

