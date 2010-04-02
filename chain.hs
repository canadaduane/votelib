import Data.Graph.Inductive
import Data.Graph.Inductive.Graph (Adj)
import Data.Graph.Analysis
import Data.List
import Data.Function (on)
import Time
import Vote (Ballot)


data (Ord a) => Vote a =
  Vote
    Int    -- The multiplier (weighting) of this vote
    Ballot -- The list of ordered preferences
    a      -- A precedence indicator such as time, or sequence
  deriving (Show, Eq)

type IVote = Vote Int

data Voter =
  Voter
    String        -- The name of the voter
    (Maybe IVote) -- Possibly a vote, if the voter submitted a ballot
  deriving (Show, Eq)

voterHasBallot (Voter _ Nothing) = False
voterHasBallot _ = True

voteWeight :: (Ord a) => Vote a -> Int
voteWeight (Vote w _ _) = w

voteWeight' :: (Ord a) => Maybe (Vote a) -> Int
voteWeight' Nothing = 1
voteWeight' (Just v) = voteWeight v

sumVoteWeights' :: (Ord a) => [Maybe (Vote a)] -> Int
sumVoteWeights' vs = foldr (+) 0 weights
  where weights = map voteWeight' vs

voterVote (Voter _ v) = v

voterWithWeight (Voter n (Just (Vote _ b o))) w = Voter n (Just (Vote w b o))
voterWithWeight v _ = v

-- Order voters in such a way that Nothing votes are greater than Just votes
-- We'll use this to determine the "first voter" (i.e. the vote time of the
-- first voter is least, and Nothing votes should all be Infinitely later in
-- time)
instance Ord Voter where
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

-- options = map Option ["Go to sleep", "Build a fort", "Watch a movie", "Go swimming"]
-- voters  = map ($) (map Voter ["Duane", "Kelty"]) --(repeat Nothing)
voters = 
  [Voter "Duane" Nothing
  ,Voter "Kelty" (Just (Vote 1 (map Just [1,0,1,2]) 0))
  ,Voter "Chris" Nothing
  ,Voter "Mandy" (Just (Vote 1 (map Just [0,1,2,3]) 1))
  ,Voter "D1" Nothing
  ,Voter "D2" Nothing
  ,Voter "C1" Nothing
  ,Voter "C2" Nothing
  ]

labUEdges = map (\(i,j) -> (i,j,()))

votG :: Gr Voter ()
votG = mkGraph (zip [0..] voters)
               (labUEdges [(0,1),(1,2),(2,3),(3,0),(4,0),(5,0),(6,2),(7,2)])

v2G :: Gr Voter ()
v2G = mkGraph (zip [0..] voters)
               (labUEdges [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7)])


-- rootsAndCoreOf :: (DynGraph g, Eq a, Eq b) => g a b -> (LNGroup a, g a b)
-- rootsAndCoreOf gr = (rootsOf gr, coreOf gr)

-- Given a graph component, determine the vote of the component
-- based on the leaf (if it has no cycles) or the earliest vote
-- (if it has a cycle)
-- proxyVoteGraph :: (DynGraph g, Ord a) => g a b -> g a b
-- proxyVoteGraph gr =
--   case length leaves of
--     0 -> case minimum (map snd firstCycle) of
--            Voter name Nothing -> error "No one voted in a cycle"
--            Voter name vote    -> vote
--     1 -> snd firstLeaf
--     _ -> error "Graph has more than one root"
--   where cycles     = cyclesIn gr
--         leaves     = leavesOf gr
--         firstCycle = head . cyclesIn $ gr
--         firstLeaf  = head . leavesOf $ gr

-- cycleReducedGraph gr =
--   if length cycles == 0
--     then gr
--     else 
--   where cycles     = cyclesIn gr
--         firstCycle = head . cyclesIn $ gr

-- proxyVote :: (DynGraph g) => g a b -> [IVote]
-- proxyVote gr = ufold voteCollapse [] gr
--   where
--     voteCollapse (froms, node, voter, tos) ls =
--       case voterHasBallot voter of
--         True -> 
--         False -> ls
--       where
--         to = if length tos == 1
--                then lab gr (head tos)
--                else Nothing
-- 

redundantEdges :: (Graph gr) => gr Voter b -> [(Node, Node)]
redundantEdges gr = ufold didVote [] gr
  where 
    didVote (_, node, voter, _) edges = 
      let next = out gr node
      in if voterHasBallot voter && length next > 0
           then case head next of
             (n1, n2, l) -> (n1, n2) : edges
           else edges

collapse gr = nmap combine collapsed
  where collapsed = collapseGraphBy [cyclesIn'] gr
        combine n = voterWithWeight earliest weight
          where earliest = minimum n
                weight = (sumVoteWeights' . map voterVote) n

-- 
main = do
  putStrLn $ "Graph of g:\n" ++ (show votG)
  putStrLn $ "\nComponents of g:\n" ++ (show (componentsOf votG))
  -- putStrLn $ "\nIndep of g:\n" ++ (show (indep votG))