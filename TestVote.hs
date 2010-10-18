import Test.HUnit
import Numeric.LinearAlgebra
import Data.Graph.Inductive

import Vote
import Chain

assertBallot varName matName var mat =
  assertEqual ("Ballot " ++ varName ++ " not equal to matrix " ++ matName) mat (ballotToMatrix (length var) var)

labUEdges = map (\(i,j) -> (i,j,()))

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


-- Test the outcomes of some simple polls
tonightPoll =
  Poll ["Let's watch a movie"
       ,"Let's build a fort"
       ,"Let's go swimming"]
       [map Just [2, 1, 0] -- Active person
       ,map Just [0, 1, 2] -- Movie watcher
       ,map Just [1, 0, 2] -- Fort builder
       ]

tennesseePoll =
  Poll ["Memphis"
       ,"Nashville"
       ,"Chattanooga"
       ,"Knoxville"]
       ((take 42 $ repeat (map Just [0, 1, 2, 3])) ++
        (take 26 $ repeat (map Just [3, 0, 1, 2])) ++
        (take 15 $ repeat (map Just [3, 2, 0, 1])) ++
        (take 17 $ repeat (map Just [3, 2, 1, 0])))

simplePoll = TestCase (do
  assertEqual "Unexpected poll outcome" "Let's build a fort" (winner rankedPairs tonightPoll)
  assertEqual "Unexpected poll outcome" "Nashville" (winner rankedPairs tennesseePoll)
  )


-- Sample voters used below
threeVoters = [Voter "Kelty" (Just (Vote 1 (map Just [1,0,1,2]) 0))
              ,Voter "Chris" Nothing
              ,Voter "Mandy" (Just (Vote 1 (map Just [0,1,2,3]) 1))
              ]

-- Test that cycles are reduced to a single node

g1 :: Gr (Voter Int) ()
g1 = mkGraph (zip [0..] threeVoters)
             (labUEdges [(0,1),(1,2),(2,0)])
cycleGraph = TestCase (assertEqual failure expected actual)
  where
    failure  = "Cycle not reduced properly" 
    expected = [Voter "Mandy" (Just (Vote 2 [Just 0,Just 1,Just 2,Just 3] 1))
               ,Voter "Kelty" (Just (Vote 1 [Just 1,Just 0,Just 1,Just 2] 0))]
    actual   = proxyVote g1


-- Test that subtrees of delegated proxies are working
g2 :: Gr (Voter Int) ()
g2 = mkGraph (zip [0..] threeVoters)
             (labUEdges [(0,1),(1,2)])
delegateGraph = TestCase (assertEqual failure expected actual)
  where
    failure  = "Delegated proxy not working" 
    expected = [Voter "Mandy" (Just (Vote 2 (map Just [0,1,2,3]) 1))
               ,Voter "Kelty" (Just (Vote 1 (map Just [1,0,1,2]) 0))]
    actual   = proxyVote g2


-- Tests GO!
main = runTestTT $ TestList
  [TestLabel "basicBallot"       $ basicBallot
  ,TestLabel "placeholderBallot" $ placeholderBallot
  ,TestLabel "noPrefBallot"      $ noPrefBallot

  ,TestLabel "simplePoll"        $ simplePoll
  
  ,TestLabel "cycleGraph"        $ cycleGraph
  ,TestLabel "delegateGraph"     $ delegateGraph
  ]