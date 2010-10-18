import Test.HUnit
import Text.CSV
import LoadData

h1 = ["H1", "H2", "H3"]
r1 = ["X", "Y", "Z"]
r2 = ["A", "B", "C"]

csv1 = [h1, r1, r2]

testReadCell = TestCase (do
  let cell = readCell h1 "H2" r1
  assertEqual "2nd cell should be Y" "Y" cell
  )

testReadColumn = TestCase (do
  let col = readColumn csv1 "H3"
  assertEqual "3rd column should be Z, C" ["Z", "C"] col
  )

testToCSV = TestCase (do
  let csv = toCSV "A1,B1\nP,Q\nR,S\n"
  assertEqual "CSV" [["A1", "B1"], ["P", "Q"], ["R", "S"]] csv
  )

testReadPairs = TestCase (do
  let pairs = readPairs csv1 "H1" "H2"
  assertEqual "Should be pairs" [("X", "Y"), ("A", "B")] pairs
  )

main = runTestTT $ TestList 
  [TestLabel "testReadCell"       $ testReadCell
  ,TestLabel "testReadColumn"     $ testReadColumn
  ,TestLabel "testReadPairs"      $ testReadPairs
  ,TestLabel "testToCSV"          $ testToCSV
  ]