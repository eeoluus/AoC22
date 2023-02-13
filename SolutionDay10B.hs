import Control.Monad.Trans.State
import Data.Char (isSpace)
import Data.List.Split (chunksOf)

type Operation = String
type Register  = (Cycle, Value)
type Cycle     = Int
type Value     = Int
type Pixel     = Char

main = do
    ops <- lines <$> readFile "Path/File.txt"
    let ops'     = concat $ map execution ops
        pixels   = evalState (traverse phases ops') (0, 1)
        screen   = chunksOf 40 $ filter (not . isSpace) pixels
    mapM print screen

execution :: String -> [Operation]
execution "noop" = ["draw"]
execution addx   = ["draw", "draw", addx]

phases :: Operation -> State Register Pixel
phases op =
    case words op of ["draw"]    -> draw
                     ["addx", v] -> addx $ read v

draw :: State Register Pixel
draw = do
    (cycle, val) <- get
    if abs (cycle `mod` 40 - val) <= 1
        then pixel '#'
        else pixel '.'

pixel :: Char -> State Register Pixel
pixel char = state $ \(cycle, val) -> (char, (cycle + 1, val))

addx :: Int -> State Register Pixel
addx v = state $ \(cycle, val) -> (' ', (cycle, val + v))