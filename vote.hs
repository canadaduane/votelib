module Vote (
   ElectoralMethod
  ,Candidate
  ,Ballot
  ,Poll(..)
  ,tally
  ,winner
  
  ,simpleCount
  ,rankedPairs
)
where

  import Numeric.LinearAlgebra
  import Foreign.Marshal.Utils (fromBool)
  import Data.List (elemIndex, maximumBy)

  -- An ElectoralMethod is a function that takes a tallied voting matrix
  -- and returns an index to the winning candidate
  type ElectoralMethod = (Matrix Double -> Maybe Int)

  -- A Candidate is the person or issue that is being voted on
  type Candidate = String

  -- A Ballot is a ranked list of preferrences corresponding to a list
  -- of Candidates
  type Ballot = [Int]

  -- A Poll is a list of Candidates and a list of Ballots
  data Poll = Poll [Candidate] [Ballot]
  
  -- Here is the main function used to determine a winner.  It takes an
  -- ElectoralMethod function as a parameter so that different ways of
  -- choosing a winner can be used.  The winning candidate is returned.
  winner :: ElectoralMethod -> Poll -> Candidate
  winner electoralMethod poll@(Poll cs bs) =
    cs !! (winnerIndex electoralMethod poll)
  
  -- Converts a Ballot (list of indices) into a matrix by comparing
  -- each candidate against every other candidate and marking a 1 
  -- wherever the row's candidate wins against the column's candidate.
  ballotToMatrix :: Int -> Ballot -> Matrix Double
  ballotToMatrix size prefs = buildMatrix size size builder
    where
      builder (row, col) | row == col = 0 -- zeros down the diagonal
      builder (row, col) =
        case elemIndex row prefs of
          Just a  ->
            case elemIndex col prefs of
              Just b  -> fromBool(a < b)
              Nothing -> 1.0
          Nothing -> 0.0

  -- Adds up all of the ballots and returns a square matrix of the
  -- specified size.
  tally :: Int -> [Ballot] -> Matrix Double
  tally size ballots = foldr (+) zeroMatrix ballotMatrices
    where zeroMatrix = (size >< size $ repeat 0)
          ballotMatrices = map (ballotToMatrix size) ballots

  -- Index of the winning Candidate in the list of Candidates
  winnerIndex :: (Matrix Double -> Maybe Int) -> Poll -> Int
  winnerIndex electoralMethod (Poll cs bs) =
    case (electoralMethod $ tally (length cs) bs) of
      Just i  -> i
      Nothing -> error "no winner"
  
  
  ------------------------------
  -- Electoral Methods
  ------------------------------
  
  -- This method simply counts the total number of cross-product wins for each
  -- candidate and declares the highest count the winner.  For example, if the
  -- matrix looks like this, the 3rd candidate would win:
  --
  -- [ 0 1 0 2    => 3
  -- , 1 0 0 0    => 1
  -- , 5 1 0 1    => 7
  -- , 3 2 0 0 ]  => 5
  --
  simpleCount :: ElectoralMethod
  simpleCount m = elemIndex (maximum sumr) sumr
    where rows = toLists m
          sumr = map sum rows
  
  -- The rankedPairs method implements Maximum Majority Voting.
  -- See:
  --   http://wiki.electorama.com/wiki/Maximum_Majority_Voting
  --   http://radicalcentrism.org/majority_voting.html
  rankedPairs :: ElectoralMethod
  rankedPairs m = Just $ fst (maximumBy sortWLT summary)
    where rows = map toList $ toRows m
          cols = map toList $ toColumns m
          comparisons = zipWith (zipWith compare) rows cols
          -- Count the number of things in a list.
          count sym = length . filter (== sym)
          -- We will count GT, LT and EQ occurrences to get the number of wins, losses and ties ("WLT")
          winsLossesTies ns = (count GT ns, count LT ns, (count EQ ns) - 1)
          -- Our final list gives is a pair of the index and the WLT triple
          summary = zip [0..] (map winsLossesTies comparisons)
          -- The winning candidate is the one that wins by the largest margin.
          -- If a tie needs to be broken, then the candidate with the fewest losses wins.
          sortWLT (i1,(w1,l1,t1)) (i2,(w2,l2,t2)) = compare (w1-l1,-l1) (w2-l2,-l2)
  
  -- Be able to pass in a string name for the electoral method.
  -- For example,
  --   winner (registeredElectoralMethod "ranked-pairs") poll
  registeredElectoralMethod :: String -> ElectoralMethod
  registeredElectoralMethod name =
    case name of
      "simple-count" -> simpleCount
      "ranked-pairs" -> rankedPairs
