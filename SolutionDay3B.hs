import Data.List.Split (chunksOf)
import Data.List (elemIndex, intersect)
import Data.Maybe (fromJust)

-- Should improve readability next time.

main :: IO ()
main = do
    rucksacks <- readFile "Path\\File.txt"
    let solution = sum . map (itemToValue . commonItem) . chunksOf 3 . lines $ rucksacks
    print solution

commonItem :: [String] -> Char
commonItem = head . foldl1 intersect

itemToValue :: Char -> Int
itemToValue item = (+) 1 . fromJust $ elemIndex item itemList 
    where itemList = ['a'..'z'] ++ ['A'..'Z']