import Numeric.LinearAlgebra
import Foreign.Marshal.Utils (fromBool)
import Data.List (elemIndex)
-- import Data.Packed.Matrix

--m :: Matrix Double
--m = (4><4)(repeat 0)


issues :: [String]
issues = ["Let's watch a movie", "Let's build a fort", "Let's go swimming"]

v1 :: [Int]
v1 = [1, 2, 0] -- Active person

v2 :: [Int]
v2 = [0, 1, 2] -- Movie watcher

v3 :: [Int]
v3 = [1, 0, 2] -- Fort builder

ballots :: [[Int]]
ballots = [v1, v2, v3]

ordered :: [Int] -> Matrix Double
ordered prefs = buildMatrix n n builder
	where
		n = length prefs
		builder (row, col) | row == col = 0
		builder (row, col) =
			case elemIndex row prefs of
				Just a  -> case elemIndex col prefs of
								Just b  -> fromBool(a < b)
								Nothing -> error "Couldn't find it"
				Nothing -> error "Couldn't find it"

tally = foldr (+) (3><3 $ repeat 0) $ map ordered ballots

main = do
	putStrLn $ show tally