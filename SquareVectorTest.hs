import Test.HUnit

import Data.Vector as V
import SquareVector


simpleBuildSquareVector = TestCase (assertEqual "Vectors" (SquareVector 3 (zeroDiag)) sqVec)
  where zeroDiag = V.fromList [0, 1, 1,   1, 0, 1,   1, 1, 0]
        builder (row, col) = if row == col then 0 else 1
        sqVec = svBuild 3 builder

squareVectorFromList = TestCase (assertEqual "SquareVector should come from list" control sample)
  where control = SquareVector 2 (V.fromList [1, 2, 3, 4])
        sample  = svFromList 2 [1, 2, 3, 4]

squareVectorToLists = TestCase (assertEqual "SquareVector should convert to lists" control sample)
  where control = [[1, 2], [3, 4]]
        sample = svToLists (SquareVector 2 (V.fromList [1, 2, 3, 4]))

addVectors = TestCase (assertEqual "Add vectors" sv3 (sv1 `svAdd` sv2))
  where sv1 = SquareVector 3 (V.fromList [1, 2, 3,   3, 2, 1,   1, 0, 1])
        sv2 = SquareVector 3 (V.fromList [2, 4, 1,   0, 1, 1,   3, 2, 1])
        sv3 = SquareVector 3 (V.fromList [3, 6, 4,   3, 3, 2,   4, 2, 2])

toColumnLists = TestCase (assertEqual "Get columns as lists" cols (svToColLists sv1))
  where sv1 = SquareVector 3 (V.fromList [1, 2, 3,   3, 2, 1,   1, 0, 1])
        cols = [[1,3,1],[2,2,0],[3,1,1]]

-- Tests GO!
main = runTestTT $ TestList
  [TestLabel "simpleBuildSquareVector"       $ simpleBuildSquareVector,
   TestLabel "squareVectorFromList"          $ squareVectorFromList,
   TestLabel "squareVectorToLists"           $ squareVectorToLists,
   TestLabel "addVectors"                    $ addVectors,
   TestLabel "toColumnLists"                 $ toColumnLists
  ]