import Data.List.Split (splitOn)
import Data.List (isInfixOf)

main :: IO ()
main = do
    input <- readFile "Path\\File.txt"
    let sections = map (map sectionsOf) . map (pairOf) $ lines input
    let highPriorities = filter oneIsSubset sections
    let solution = length highPriorities
    print solution

pairOf :: String -> [String]
pairOf = splitOn ","

sectionsOf :: String -> [Int]
sectionsOf elf = [a..b]
    where a : b : [] = map read $ splitOn "-" elf

oneIsSubset :: [[Int]] -> Bool
oneIsSubset pair = 
    let a = head pair
        b = last pair
    in a `isInfixOf` b || b `isInfixOf` a