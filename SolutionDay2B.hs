main :: IO ()
main = do
    input <- readFile "Path\\File.txt"
    let solution = sum . map (sum . parseSubscores) $ lines input
    print solution

parseSubscores :: String -> [Int]
parseSubscores x = case x of
    "A X" -> [3, 0]
    "A Y" -> [1, 3]
    "A Z" -> [2, 6]
    "B X" -> [1, 0]
    "B Y" -> [2, 3]
    "B Z" -> [3, 6]
    "C X" -> [2, 0]
    "C Y" -> [3, 3]
    "C Z" -> [1, 6]