import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "Path\\File.ext"
    let solution = maximum . map (sum . map read) . splitOn [""] $ lines input
    print solution