import Data.Time
import System.Random

main = do
  gen <- getStdGen
--  randomVal <- random RPS


-- Rock Paper Scissors game functions
data RPS = Rock | Paper | Scissors deriving (Eq, Show, Read)
type Score = (Int, Int)

-- Who beats whom Logic
beats :: RPS -> RPS -> Bool
beats Rock Scissors = True
beats Paper Rock    = True
beats Scissors Paper = True
beats _ _ = False

-- Play a single Rock, Paper, Scissors 
play :: RPS -> RPS -> Score -> Score
play a b (sa, sb)
    | a `beats` b = (sa+1, sb)
    | b `beats` a = (sa, sb+1)
    | otherwise   = (sa, sb)

-- Can feed a list of moves (touples) to play at once
multiPlay :: [(RPS,RPS)] -> Score -> Score
multiPlay [] (sa,sb) = (sa,sb)
multiPlay (x:xs) (sa,sb) = multiPlay xs (play (fst x) (snd x) (sa,sb))

-- Helper Functions
num2RPS :: Int -> RPS
num2RPS x = rpsReturn
    where
    rpsReturn
      | x == 0    = Rock
      | x == 1    = Paper
      | otherwise = Scissors

numList2RPS :: [Int] -> [RPS]
numList2RPS n = map num2RPS n

-- Computer Challenger Code



-- Strategy Functions
-- ------------------
-- Experimenting with making "Strategy" functions that I can have face
-- off against each other.
buildMoveList :: Int -> ([RPS] -> [RPS] -> [RPS]) -> [RPS] -> ([RPS] -> [RPS] -> [RPS]) -> [RPS] -> [(RPS, RPS)]
buildMoveList  n fa a fb b = if n == 0 then zip (reverse a) (reverse b) else buildMoveList (n - 1) fa (fa a b) fb (fb b a)

altThree :: [RPS] -> [RPS] -> [RPS]
altThree [] _ = [Paper]
altThree (x:xs) _ = rpsReturn
    where
    rpsReturn
      | x == Rock     = Paper:x:xs
      | x == Paper    = Scissors:x:xs
      | x == Scissors = Rock:x:xs

oppLast :: [RPS] -> [RPS] -> [RPS]
oppLast _ []     = [Rock]
oppLast x (y:ys) = y : x


