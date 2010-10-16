import Data.Graph.Inductive
import Vote
import Chain

voterName :: (Ord o) => Voter o -> String
voterName (Voter name _) = name

vote prefs time = Just (Vote 1 prefs time)

voters = [Voter "Kelty" $ vote [1,0,1,2] 0
         ,Voter "Chris" Nothing
         ,Voter "Mandy" $ vote [0,1,2,3] 1
         ,Voter "Duane" Nothing
         
         ,Voter "Yuan"  $ vote [0,0,1] 0
         ,Voter "Ming"  $ vote [0,1,1] 1
         ,Voter "Wan"   Nothing
         ,Voter "Ping"  Nothing
         ]

g1 :: Gr (Voter Int) ()
g1 = mkGraph (zip [0..] voters) [(0,1,()),(1,2,()),(2,0,()),(3,1,()),(6,5,()),(5,4,()),(7,6,())]

votersDisseminated = proxyVote g1

ballots = votersToBallots votersDisseminated

options = ["Chicago", "New York", "Lethbridge", "Toronto"]
poll = Poll options ballots

showb bs = concatMap (nl . show) bs
  where nl = (++ "\n")

-- rankedBallot :: Ballot -> [[Int]]


-- showBallot :: [String] -> Ballot -> String
showBallot :: [String] -> Ballot -> [(Int, String)]
showBallot opts b = cs
  where cs = zip b opts

showOptions :: Ballot -> [(Int, String)]
showOptions = showBallot options

main = do
  putStrLn (show (map showOptions ballots))
  -- putStrLn ("Redundant Edges: " ++ show (redundantEdges g1) ++ "\n")
  putStrLn (showb (zip3 (map voterName votersDisseminated)
                        (map voterGetWeight votersDisseminated)
                        (map voterGetBallot votersDisseminated)))
  putStrLn (show (winner rankedPairs poll))

