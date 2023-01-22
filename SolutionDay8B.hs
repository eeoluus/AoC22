import Data.List (break, transpose)
import Data.Char (digitToInt)

main :: IO ()
main = do
    trees <- lines <$> readFile "Path\\File.txt"
    let heights      = map (map digitToInt) trees
        xPoints      = concat . rotate $ map score heights
        yPoints      = concat . map score $ rotate heights
        solution     = maximum $ zipWith (*) xPoints yPoints
    print solution

rotate :: [[Int]] -> [[Int]]
rotate = transpose . map reverse

score :: [Int] -> [Int]
score (t:trees) = [subscore] ++ score' [t] trees
    where subscore = points t trees

score' :: [Int] -> [Int] -> [Int]
score' left (t:trees) | null trees = [points t left']
                      | otherwise = [subscore] ++ score' (left ++ [t]) trees
                      where left' = reverse left
                            subscore = product $ map (points t) [left', trees]

points :: Int -> [Int] -> Int
points x trees = 
    case break (>= x) trees of (ts, []) -> length ts
                               (ts, _)  -> length ts + 1
                                            