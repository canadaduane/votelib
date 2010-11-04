module SquareVector (
  SquareVector(..),
  svBuild,
  svAdd,
  svFromList,
  svToLists,
  svToRows,
  svToColLists
)
where
  import Data.Vector as V
  import Data.List (transpose)
  
  data SquareVector = SquareVector Int (V.Vector Int)
    deriving (Show, Eq)
  
  svBuild :: Int -> ((Int, Int) -> Int) -> SquareVector
  svBuild n builder = SquareVector n (generate (n*n) lookup)
    where lookup z = let row = z `div` n
                         col = z `mod` n
                     in builder (row, col)
  
  svAdd :: SquareVector -> SquareVector -> SquareVector
  svAdd (SquareVector n1 vec1) (SquareVector n2 vec2) =
    if n1 == n2
      then SquareVector n1 (V.zipWith (+) vec1 vec2)
      else error "SquareVectors are of unequal dimensions"
  
  svFromList :: Int -> [Int] -> SquareVector
  svFromList n list =
    if (Prelude.length list) == n * n
      then SquareVector n (V.fromList list)
      else error "SquareVector size does not equal list length squared"
  
  svToLists :: SquareVector -> [[Int]]
  svToLists sv = Prelude.map V.toList (svToRows sv)
  
  svToRows :: SquareVector -> [V.Vector Int]
  svToRows (SquareVector n vec) = [V.slice k n vec | k <- [0,n..(n-1)*n]]
  
  svToColLists :: SquareVector -> [[Int]]
  svToColLists sv = transpose (svToLists sv)