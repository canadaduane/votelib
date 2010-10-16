import Data.Graph.Inductive
import Maybe (catMaybes, fromJust)
import Data.Function (on)
import Data.List (sortBy)
import Vote
import Chain

voterName :: (Ord o) => Voter o -> String
voterName (Voter name _) = name

vote prefs time = Just (Vote 1 (map Just prefs) time)

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

rankedBallot :: Ballot -> [(Int, Int)]
rankedBallot ballot = sortBy (compare `on` snd) listOnly
  where indexed = zip [0..] ballot
        unwrap (a, Just b) = Just (a, b+1)
        unwrap (a, Nothing) = Nothing
        listOnly = catMaybes (map unwrap indexed)
        

-- showBallot :: [String] -> Ballot -> String
-- showBallot :: [String] -> Ballot -> [(String, Int)]
showBallot opts b = map namePair ranked
  where ranked = rankedBallot b
        candidate k = opts !! k
        namePair (k, rank) = (candidate k, rank)

showOptions :: Ballot -> [(String, Int)]
showOptions = showBallot options



main = do
  putStrLn (showb (zip3 (map voterName votersDisseminated)
                        (map voterGetWeight votersDisseminated)
                        (map (showOptions . fromJust . voterGetBallot) votersDisseminated)))
  putStrLn (show (winner rankedPairs poll))

