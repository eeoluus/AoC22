import Data.List.Split (splitOn)
import Data.List (dropWhile, intersect, isInfixOf)

main :: IO ()
main = do
    input <- readFile "Path\\File.txt"
    let sections = map (map sectionsOf) . map (pairOf) $ lines input
    let highPriorities = filter nonemptyIntersection sections
    let solution = length highPriorities
    print solution

pairOf :: String -> [String]
pairOf = splitOn ","

sectionsOf :: String -> [Int]
sectionsOf elf = [a..b]
    where a : b : [] = map read $ splitOn "-" elf

nonemptyIntersection :: [[Int]] -> Bool
nonemptyIntersection pair =
    let a = head pair
        b = last pair
    in not . null $ intersect a b