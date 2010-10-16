module Chain (
  Vote(..)
  ,Voter(..)
  ,proxyVote
  ,votersToBallots
  ,voterGetWeight
  ,voterGetBallot
) where
  
  import Data.Graph.Inductive
  import Data.Graph.Analysis
  import Data.List
  import Data.Function (on)
  -- import Time
  import Vote (Ballot)


  data (Ord o) => Vote o =
    Vote
      Int              -- The multiplier (weighting) of this vote
      Ballot           -- The list of ordered preferences
      o                -- A precedence indicator such as time, or sequence
    deriving (Show, Eq)

  data (Ord o) => Voter o =
    Voter
      String           -- The name of the voter
      (Maybe (Vote o)) -- Possibly a vote, if the voter submitted a ballot
    deriving (Show, Eq)

  voterHasBallot :: (Ord o) => Voter o -> Bool
  voterHasBallot (Voter _ Nothing) = False
  voterHasBallot _ = True

  voterGetBallot :: (Ord o) => Voter o -> Maybe Ballot
  voterGetBallot (Voter _ (Just (Vote _ b _))) = Just b
  voterGetBallot _ = Nothing

  voterGetWeight :: (Ord o) => Voter o -> Int
  voterGetWeight (Voter _ (Just (Vote w _ _))) = w
  voterGetWeight (Voter _ Nothing) = 1

  voterSetWeight :: (Ord o) => Voter o -> Int -> Voter o
  voterSetWeight (Voter n (Just (Vote _ b o))) w = Voter n (Just (Vote w b o))
  voterSetWeight v _ = v

  -- Order voters in such a way that Nothing votes are greater than Just votes
  -- We'll use this to determine the "first voter" (i.e. the vote time of the
  -- first voter is least, and Nothing votes should all be Infinitely later in
  -- time)
  instance (Ord o) => Ord (Voter o) where
    compare (Voter _ a) (Voter _ b) =
      case a of
        Nothing ->
          case b of
            Nothing -> EQ
            Just (Vote _ _ x) -> GT
        Just (Vote _ _ y) ->
          case b of
            Nothing -> LT
            Just (Vote _ _ x) -> compare y x

  -- Edges that point from a voter who has a ballot, to a voter who does NOT have a ballot
  -- are redundant because if a voter has a ballot, then their proxy will not be used.
  redundantEdges :: (DynGraph gr, Ord o) => gr (Voter o) b -> [(Node, Node)]
  redundantEdges gr = ufold didVote [] gr
    where 
      didVote (_, node, voter, _) edges = 
        let next = out gr node
        in if voterHasBallot voter && length next > 0
             then case head next of
               (n1, n2, l) -> (n1, n2) : edges
             else edges

  delRedundantEdges :: (DynGraph gr, Ord o) => gr (Voter o) b -> gr (Voter o) b
  delRedundantEdges gr = delEdges (redundantEdges gr) gr


  collapseCycle :: (DynGraph gr, Ord o) => gr (Voter o) b -> gr (Voter o) b
  collapseCycle gr = nmap combine collapsed
    where collapsed = collapseGraphBy [cyclesIn'] gr
          combine n = voterSetWeight earliest weight
            where earliest = minimum n
                  weight = foldr (+) 0 (map voterGetWeight n)

  sumOfSubtree :: (DynGraph gr, Ord o) => gr (Voter o) b -> (Voter o)
  sumOfSubtree gr = voterSetWeight leaf weight
    where leaf = snd . head . leavesOf $ gr
          weight = ufold ((+) . voterGetWeight . lab') 0 gr

  -- Reduce a graph to a list of Participating Voters, each weighted according to the
  -- trust network established by the delegable proxy graph.
  proxyVote :: (DynGraph gr, Ord o) => gr (Voter o) b -> [Voter o]
  proxyVote gr = map sumOfSubtree denseTrees
    where
      trees = map collapseCycle ((componentsOf . delRedundantEdges) gr)
      denseTrees = concatMap componentsOf trees

  votersToBallots :: (Ord o) => [Voter o] -> [Ballot]
  votersToBallots vs = concatMap vballots vs
    where vballots (Voter _ (Just (Vote n b _))) = take n $ repeat b
          vballots (Voter _ Nothing) = []

  