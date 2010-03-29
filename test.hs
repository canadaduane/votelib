import Test.HUnit
import Vote
import Numeric.LinearAlgebra

assertBallot varName matName var mat =
  assertEqual ("Ballot " ++ varName ++ " not equal to matrix " ++ matName) mat (ballotToMatrix (length var) var)

-- Be able to construct a matrix from a ballot
v1 = map Just [0, 1, 2]
m1 = fromLists
  [[0, 1, 1]
  ,[0, 0, 1]
  ,[0, 0, 0]
  ]
v2 = map Just [1, 0, 2, 3]
m2 = fromLists
  [[0, 0, 1, 1]
  ,[1, 0, 1, 1]
  ,[0, 0, 0, 1]
  ,[0, 0, 0, 0]
  ]
basicBallot = TestCase (do
  assertBallot "v1" "m1" v1 m1
  assertBallot "v2" "m2" v2 m2
  )

-- Consider cases when ballot includes Nothing placeholders
v3 = [Nothing, Just 0, Just 1, Nothing]
m3 = fromLists
  [[0, 0, 0, 0]
  ,[1, 0, 1, 1]
  ,[1, 0, 0, 1]
  ,[0, 0, 0, 0]
  ]
placeholderBallot = TestCase (do
  assertBallot "v3" "m3" v3 m3
  )

-- Ballots should be able to show "no preference" by double listing ordinals
v4 = map Just [0, 1, 1, 2]
m4 = fromLists
  [[0, 1, 1, 1]
  ,[0, 0, 0, 1]
  ,[0, 0, 0, 1]
  ,[0, 0, 0, 0]
  ]
noPrefBallot = TestCase (do
  assertBallot "v4" "m4" v4 m4
  )

main = runTestTT $ TestList
  [TestLabel "basicBallot"       $ basicBallot
  ,TestLabel "placeholderBallot" $ placeholderBallot
  ,TestLabel "noPrefBallot"      $ noPrefBallot
  ]