import Data.Time
import System.Random

main = do
  putStrLn("Welcome to hRPS!")
  -- Get number of Rounds
--  rounds <- roundsPrompt
  --putStrLn("Selected " ++ rounds ++ " rounds.")

  final <- playRounds 3 (0,0)
  return(final)
  

-- Get Rounds
roundsPrompt :: IO String
roundsPrompt = do
  putStrLn "How many rounds?"

  -- Will add game types in future
--  putStrLn "a - Best of 3"
--  putStrLn "b - Best of 5"
--  putStrLn "c - Best of 7"
--  putStrLn "# - Best of N (enter a number for N)"
--  putStrLn "0 - Endless (Keep playing till quit)"
  rounds <- getLine
  return(rounds)
 

-- Generate a random play 
randomVal :: IO RPS
randomVal = do 
  num <- randomRIO (0, 2) :: IO Int
  return ([Rock, Paper, Scissors] !! num)

-- Get User Round
getUserVal :: IO RPS
getUserVal = do
  putStrLn "Select Play: 0] Rock   1] Paper   2] Scissors"
  cmd <- getLine
  let num = (read cmd)
  return ([Rock, Paper, Scissors] !! num)
  

-- Play Round
--playRounds :: Int -> Score -> IO Score
playRounds 0 score = do
  let finalScoreStr = "Final Score: Player= " ++ show(fst score) ++ " Computer= " ++ show(snd score) in putStrLn finalScoreStr
  return(score)
playRounds n score = do
  let round = fst(score) + snd(score) + 1
  let roundString = "Round " ++ show(round) ++ ":" in putStrLn roundString
  newScore <- playRound score
  -- Print new Score
  let scoreString = "New Score: " ++ show(newScore) ++ "\n" in  putStrLn scoreString
  let newN = if (fst newScore) + (snd newScore) + 1 > round then n - 1 else n
  -- Final Score
  finalScore <- playRounds newN newScore
  return(finalScore)

playRound score = do
  -- Get Move Values
  playerMove <- getUserVal
  aiMove <- randomVal

  -- Print out move results
  let moveSelectionString = "\nPlayer Selected: " ++ show(playerMove) ++ "   Computer Selected: " ++ show(aiMove)
  putStrLn moveSelectionString

  -- Calculate Score
  let newScore = play playerMove aiMove score

  return(newScore)

-----------------------
-- "Pure" Functions  --
-----------------------
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


