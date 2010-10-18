import Data.Graph.Inductive
import Data.Function (on)
import Data.List (sortBy, elemIndex)
import Data.List.Split (splitOn)
import Maybe (catMaybes, fromJust)
import Text.CSV
import Vote
import Chain
import LoadData

vote prefs time = Just (Vote 1 (map Just prefs) time)

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

electionResult :: String -> String
electionResult electionString =
  if electionString == ""
    then "Usage: Input should be a CSV file"
    else winner rankedPairs (Poll options ballots) ++ "\n"
  where
    electionCSV = toCSV electionString
    graph       = graphFromCSV electionCSV
    proxies     = proxyVote graph
    ballots     = votersToBallots proxies
    options     = map show [0..maximum (catMaybes $ concat ballots)] :: [String]

main = interact electionResult
