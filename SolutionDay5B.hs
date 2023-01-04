import Data.List (transpose, isInfixOf, foldl')
import Data.List.Split (splitOn)
import Data.Text (strip, pack, unpack)

main :: IO ()
main = do
    input <- readFile "C:\\Users\\35850\\Desktop\\input.txt"
    let boxes : instructions : [] = splitOn [""] $ lines input
    let stacks = filter (not . null) $ contentsOf boxes
    let solution = map (head) $ foldl' rearrange stacks instructions
    print solution

contentsOf :: [String] -> [String]
contentsOf boxes =
    let opened = foldl' rejectByInfix (transpose boxes) ["[","]"]
        contents = map (unpack . strip . pack) opened
    in contents

rejectByInfix :: [String] -> String -> [String]
rejectByInfix strings sub = filter (not . isInfixOf sub) strings

everySecond :: [String] -> [String]
everySecond (x:y:xs) = y : everySecond xs
everySecond _ = []

rearrange :: [String] -> String -> [String]
rearrange stacks instructions = 
    let amount : oldPosition : newPosition : [] = everySecond $ words instructions
        (stacks', cargo) = popLeft stacks (read amount) oldPosition
    in appendLeft stacks' cargo newPosition

popLeft :: [String] -> Int -> String -> ([String], String)
popLeft stacks amount position = (stacks', cargo)
    where cargo = take amount . head $ filter (isInfixOf position) stacks
          stacks' = map (\x -> if position `isInfixOf` x then drop amount x else x) stacks

appendLeft :: [String] -> String -> String -> [String]
appendLeft stacks cargo position = map (\x -> if position `isInfixOf` x then cargo ++ x else x) stacks
-- Only changed the lambda not to reverse the "cargo"
