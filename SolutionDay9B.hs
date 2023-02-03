import qualified Data.Set as Set (Set, empty, insert)
import Data.List (foldl')

type Simulation = (Rope, Set.Set Knot)
type Rope = [Knot]
type Knot = [Int]

main = do
    instructions <- lines <$> readFile "Path/File.txt"
    let rope     = replicate 10 [0, 0]
        tracks   = Set.empty
        solution = length $ whereTailWas (rope, tracks) instructions
    print solution

whereTailWas :: Simulation -> [String] -> Set.Set Knot
whereTailWas simulation instructions = snd $ foldl' track simulation instructions

track :: Simulation -> String -> Simulation
track simulation instr = n' `times` move side $ simulation
    where [side, n] = words instr
          n'        = read n

move :: String -> Simulation -> Simulation
move side (rope, visited) =
    let (end : rest) = rope
        end'        = step side end
        rest'       = rest `chainReactTo` end'
        rope'       = (end' : rest')
        visited'    = Set.insert (last rest') visited
    in (rope', visited')

step :: String -> Knot -> Knot
step side [x, y] = 
    case side of "U" -> [x, y + 1]
                 "R" -> [x + 1, y]
                 "D" -> [x, y - 1]
                 "L" -> [x - 1, y]

chainReactTo :: Rope -> Knot -> Rope
chainReactTo rope@(k : ks) knot
    | all (<= 1) . map abs $ difference knot k = rope
    | otherwise = if null ks then [k'] else (k' : (ks `chainReactTo` k'))
    where k' = k `follow` knot

follow :: Knot -> Knot -> Knot
follow rest end = zipWith (+) rest distance
    where distance = direction $ difference end rest

times :: Int -> (a -> a) -> (a -> a)
times 1 func = func
times reps func = func . times (reps - 1) func

direction :: [Int] -> [Int]
direction = map signum

difference :: [Int] -> [Int] -> [Int]
difference = zipWith (-)

