import Data.Graph.Inductive

labUEdges = map (\(i,j) -> (i,j,()))

g :: Gr String ()
g = mkGraph (zip [0..] ["A","B","C","D"])
            (labUEdges [(0,1),(1,2),(2,3)])

outEdges gr = ufold between [] gr
  where between context@(prev, node, voter, next) edges = (node, out gr node) : edges
