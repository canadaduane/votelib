import Numeric.LinearAlgebra
import Foreign.Marshal.Utils (fromBool)
import Data.List (elemIndex)

contenders :: [String]
contenders = ["Let's watch a movie", "Let's build a fort", "Let's go swimming"]

v1 :: [Int]
v1 = [1, 2, 0] -- Active person

v2 :: [Int]
v2 = [0, 1, 2] -- Movie watcher

v3 :: [Int]
v3 = [1, 0, 2] -- Fort builder

ballots :: [[Int]]
ballots = [v1, v2, v3]

ordered :: Int -> [Int] -> Matrix Double
ordered size prefs = buildMatrix size size builder
  where
    builder (row, col) | row == col = 0 -- zeros down the diagonal
    builder (row, col) =
      case elemIndex row prefs of
        Just a  ->
          case elemIndex col prefs of
            Just b  -> fromBool(a < b)
            Nothing -> 1.0
        Nothing -> 0.0

tally :: Int -> [[Int]] -> Matrix Double
tally size ballots = foldr (+) zeroMatrix ballotMatrices
  where zeroMatrix = (size >< size $ repeat 0)
        ballotMatrices = map (ordered size) ballots

main = do
  putStrLn $ show (tally size ballots)
  where size = length contenders