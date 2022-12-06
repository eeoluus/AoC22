main :: IO ()
main = do
    input <- readFile "Path\\File.txt"
    let solution = sum . map (sum . parseSubscores) $ lines input
    print solution

parseSubscores :: String -> [Int]
parseSubscores x = case x of
    "A X" -> [1, 3]
    "A Y" -> [2, 6]
    "A Z" -> [3, 0]
    "B X" -> [1, 0]
    "B Y" -> [2, 3]
    "B Z" -> [3, 6]
    "C X" -> [1, 6]
    "C Y" -> [2, 0]
    "C Z" -> [3, 3]