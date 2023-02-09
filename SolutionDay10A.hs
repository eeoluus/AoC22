import Control.Monad.Trans.State

type Operation = String
type Register  = (Cycle, Value)
type Cycle     = Int
type Value     = Int

main = do
    ops <- lines <$> readFile "Path/File.txt"
    let register = (0, 1)
        ops'     = concat $ map execution ops
        log      = fst $ runState (program ops') register
        cycles   = pick [20, 60, 100, 140, 180, 220] log
        solution = sum $ map (\(cycle, val) -> cycle * val) cycles
    print solution

execution :: String -> [String]
execution "noop" = ["tick"]
execution addx   = ["tick", "tick", addx]

program :: [Operation] -> State Register [Register]
program = traverse phases

phases :: Operation -> State Register Register
phases op = case words op of ["tick"]    -> tick >> get
                             ["addx", v] -> (addx $ read v) >> get

addx :: Int -> State Register ()
addx v = state $ \(cycle, val) -> ((), (cycle, val + v))

tick :: State Register ()
tick = state $ \(cycle, val) -> ((), (cycle + 1, val))

pick :: [Cycle] -> [Register] -> [Register]
pick [] regs       = []
pick (c : cs) regs = let (r : rs) = snd $ break (matchCycle c) regs
                     in [r] ++ pick cs rs

matchCycle :: Cycle -> Register -> Bool
matchCycle cycle (c, _) = (cycle == c)