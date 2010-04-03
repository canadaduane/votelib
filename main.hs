import Data.Graph.Inductive
import Vote
import Chain

vote prefs time = Just (Vote 1 (map Just prefs) time)

voters = [Voter "Kelty" $ vote [1,0,1,2] 0
         ,Voter "Chris" Nothing
         ,Voter "Mandy" $ vote [0,1,2,3] 1
         ,Voter "Duane" Nothing
         
         ,Voter "Yuan"  $ vote [0,0,1] 0
         ,Voter "Ming"  $ vote [0,1,1] 1
         ,Voter "Wan"   Nothing
         ]

g1 :: Gr (Voter Int) ()
g1 = mkGraph (zip [0..] voters) [(0,1,()),(1,2,()),(2,0,()),(3,1,()),(6,5,()),(5,4,())]

ballots = votersToBallots (proxyVote g1)

poll = Poll ["Option 0", "Option 1", "Option 2", "Option 3"] ballots

showb bs = concatMap (nl . show) bs
  where nl = (++ "\n")
        

main = do
  putStrLn (showb ballots)
  putStrLn (show (winner rankedPairs poll))