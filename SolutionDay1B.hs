import Data.List.Split (splitOn)
import Data.List (sortBy)

main :: IO ()
main = do
    input <- readFile "Path\\File.txt"
    let solution = sum . threeWithMost $ parseAndSum input
    print solution

parseAndSum :: String -> [Int]
parseAndSum = map (sum . map read) . splitOn [""] . lines

threeWithMost :: [Int] -> [Int]
threeWithMost = take 3 . sortBy (flip compare)