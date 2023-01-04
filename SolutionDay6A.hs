import Data.Set (fromList)

main :: IO ()
main = do
    input <- readFile "C:\\Users\\35850\\Desktop\\input.txt"
    let solution = (length $ parse input) + 4
    print solution

parse :: String -> String
parse signal@(x:xs)
    | (length . fromList $ take 4 signal) == 4 = []
    | otherwise = x : parse xs