import Data.Graph.Inductive
import Data.Function (on)
import Data.List (sortBy, elemIndex)
import Data.List.Split (splitOn)
import Maybe (catMaybes, fromJust)
import Text.CSV
import Vote
import Chain
import LoadData

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
        

showBallot :: [String] -> Ballot -> [(String, Int)]
showBallot opts b = map namePair ranked
  where ranked = rankedBallot b
        candidate k = opts !! k
        namePair (k, rank) = (candidate k, rank)

showOptions :: Ballot -> [(String, Int)]
showOptions = showBallot options


voterFromRecord :: Record -> Record -> Voter Int
voterFromRecord header rec =
  let choice = (if prefs == "" then Nothing else vote parsedPrefs time)
  in Voter name choice
  where cell columnName = readCell header columnName rec
        name            = (cell "Name") :: String
        time            = read (cell "Time") :: Int
        prefs           = (cell "Preferences") :: String
        parsedPrefs     = map read (splitOn "|" prefs) :: [Int]

graphFromCSV :: CSV -> Gr (Voter Int) ()
graphFromCSV csv = mkGraph (zip ids voters) edges
  where ids        = map read (readColumn csv "ID") :: [Int]
        voters     = map (voterFromRecord (head csv)) (tail csv)
        delegates  = readColumn csv "DelegateID"
        edges      = map convert (filter empty (zip3 ids delegates (repeat ())))
          where convert (a, b, c) = (a, read b, c) :: (Int, Int, ())
                empty   (a, b, c) = (b /= "")

-- electionResult :: String -> String
-- electionResult electionString = winner rankedPairs (Poll options ballots)
--   where
--     electionCSV = toCSV electionString
--     graph       = graphFromCSV electionCSV
--     proxies     = proxyVote graph
--     ballots     = votersToBallots proxies
--     options     = catMaybes (nodes graph)

main = do
  optionsString <- readFile "options.csv"
  let options = map snd (sortBy (compare `on` fst) (readPairs (toCSV optionsString) "OptionID" "OptionName"))
  
  electionString <- readFile "election.csv"
  let electionCSV = toCSV electionString
  let graph = graphFromCSV electionCSV
  let proxies = proxyVote graph
  let ballots = votersToBallots proxies
  
  let poll = Poll options ballots
  
  -- putStrLn (showb (zip3 (map voterName proxies)
  --                       (map voterGetWeight proxies)
  --                       (map (showOptions . fromJust . voterGetBallot) proxies)))
  
  putStrLn (winner rankedPairs poll)
