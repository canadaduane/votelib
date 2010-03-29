import Vote

main = do
  putStrLn $ show (winner simpleCount testPoll1)
  putStrLn $ show (winner rankedPairs testPoll2)

-- Test Polls

votes1 =
  [[1, 2, 0] -- Active person
  ,[0, 1, 2] -- Movie watcher
  ,[1, 0, 2] -- Fort builder
  ]
testPoll1 = Poll ["Let's watch a movie",
                  "Let's build a fort",
                  "Let's go swimming"]
                 votes1

-- votes2 =
--   (take 42 $ repeat [0, 1, 2, 3]) ++
--   (take 26 $ repeat [1, 2, 3, 0]) ++
--   (take 15 $ repeat [2, 3, 1, 0]) ++
--   (take 17 $ repeat [3, 2, 1, 0])

votes2 =
  (take 42 $ repeat [0, 1, 2, 3]) ++
  (take 26 $ repeat [3, 0, 1, 2]) ++
  (take 15 $ repeat [3, 2, 0, 1]) ++
  (take 17 $ repeat [3, 2, 1, 0])

testPoll2 = Poll ["Memphis", "Nashville", "Chattanooga", "Knoxville"] votes2


-- votes = take 10000 $ repeat [0..9]
-- poll = Poll ["1","2","3","4","5","6","7","8","9","10"] votes
-- 
-- main = do
--   putStrLn $ show (winner rankedPairs poll)
--   -- putStrLn $ show (tally 10 votes)