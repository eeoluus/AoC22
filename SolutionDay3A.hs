import Data.List (elemIndex, intersect) 
import Data.Maybe (fromJust)

main :: IO ()
main = do
    rucksacks <- readFile "Path\\File.txt"
    let solution = sum . map (itemToValue . commonItem . halvesOf) $ lines rucksacks
    print solution

halvesOf :: [Char] -> ([Char], [Char])
halvesOf list = splitAt (length list `div` 2) list

commonItem :: ([Char], [Char]) -> Char
commonItem halves = head $ intersect (fst halves) (snd halves)

itemToValue :: Char -> Int
itemToValue item = (+) 1 . fromJust $ elemIndex item itemList 
    where itemList = ['a'..'z'] ++ ['A'..'Z']