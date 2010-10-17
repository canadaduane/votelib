import Data.Graph.Inductive
import Data.Function (on)
import Data.List (sortBy, elemIndex)
import Data.List.Split (splitOn)
import Maybe (catMaybes, fromJust)
import Vote
import Chain
import Text.CSV

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

readColumn :: String -> CSV -> [String]
readColumn columnName csv = map get rows
 where header = head csv
       rows = tail csv
       columnIndex = fromJust (elemIndex columnName header)
       get row = if columnIndex < length row 
               then row !! columnIndex
               else error ("Umm, can't get column " ++ (show columnIndex) ++ " when length is " ++ (show (length row)) ++ ": " ++ (show row))

voterFromRecord :: Record -> Voter Int
voterFromRecord rec =
    if prefs == ""
      then Voter name Nothing
      else Voter name $ vote parsedPrefs time
  where name        = (rec !! 1)
        prefs       = (rec !! 3)
        parsedPrefs = map read (splitOn "|" prefs) :: [Int]
        time        = read (rec !! 4) :: Int

graphFromCSV :: CSV -> Gr (Voter Int) ()
graphFromCSV csv = mkGraph (zip ids voters) edges
  where ids        = map read (readColumn "ID" csv) :: [Int]
        voters     = map voterFromRecord (tail csv)
        delegates  = readColumn "DelegateID" csv
        edges      = map convert (filter empty (zip3 ids delegates (repeat ())))
          where convert (a, b, c) = (a, read b, c) :: (Int, Int, ())
                empty   (a, b, c) = (b /= "")

main = do
  optionsCSV <- (parseCSVFromFile "options.csv")
  let options = either (fail "can't load options.csv") (readColumn "OptionName") optionsCSV
  -- putStrLn (show x)
  
  electionCSV <- (parseCSVFromFile "election.csv")
  let graph = either (fail "can't load election.csv") graphFromCSV electionCSV
  -- let y = either (fail "can't load election.csv") id electionCSV
  -- putStrLn (show y)
  
  -- putStrLn (show $ voterFromRecord (y !! 1))
  
  let proxies = proxyVote graph
  let ballots = votersToBallots proxies
  let poll = Poll options ballots
  
  putStrLn (showb (zip3 (map voterName proxies)
                        (map voterGetWeight proxies)
                        (map (showOptions . fromJust . voterGetBallot) proxies)))
  putStrLn (show (winner rankedPairs poll))

