import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (digitToInt)
import qualified Data.Set as Set

type Height = Int
type Coords = (Int, Int)
type Tree = (Height, Coords)
type Forest = [[Tree]]

main :: IO ()
main = do
    trees <- lines <$> readFile "Path\\File.txt"
    let forest = locationsOf $ map (map digitToInt) trees
        solution = length . removeDuplicates . (4 `times` scan) $ ([], forest)
    print solution

locationsOf :: [[Int]] -> Forest
locationsOf trees = zipWith zip trees coordinates
    where x = length . head $ trees
          y = length trees
          coordinates = chunksOf x $ (,) <$> [1..y] <*> [1..x]

removeDuplicates :: (Forest, Forest) -> Set.Set Tree
removeDuplicates (forest, _) = Set.fromList $ concat forest

times :: Int -> (a -> a) -> (a -> a)
times n f = foldr (.) id (replicate n f)

scan :: (Forest, Forest) -> (Forest, Forest)
scan (vs, ts) = (vs ++ visibleTrees, trees)
    where trees = rotate ts
          visibleTrees = map visible trees

rotate :: Forest -> Forest
rotate = transpose . map reverse

visible :: [Tree] -> [Tree]
visible [] = []
visible trees = v : (visible front)
    where (front, v:_) = break (tallestOf trees) trees

tallestOf :: [Tree] -> (Tree -> Bool)
tallestOf trees = (==) tallest . fst
    where tallest = maximum . fst $ unzip trees