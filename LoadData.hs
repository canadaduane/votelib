module LoadData (
  readCell
  ,readColumn
  ,readPairs
  ,toCSV
) where
  import Data.List (elemIndex)
  import Text.CSV

  readCell :: Record -> String -> Record -> String
  readCell header columnName rec =
    if columnIndex < length rec
      then rec !! columnIndex
      else error ("Index out of range for column (" ++ (show columnIndex) ++ "/" ++ (show (length rec)) ++ ")")
    where columnIndex = case (elemIndex columnName header) of
                          Just idx -> idx
                          Nothing  -> error ("Can't get column named " ++ columnName ++ ".")

  readColumn :: CSV -> String -> [String]
  readColumn csv columnName = map (readCell header columnName) records
    where header = head csv
          records = tail csv

  readPairs :: CSV -> String -> String -> [(String, String)]
  readPairs csv colName1 colName2 = zip col1 col2
    where col1 = readColumn csv colName1
          col2 = readColumn csv colName2

  toCSV :: String -> CSV
  toCSV text = either failure id (parseCSV "CSV" cleanText)
    where failure = error ("Can't parse CSV")
          isBlank line = (line == "" || line == "\n" || line == "\r\n")
          cleanText = init $ (unlines . (filter (not . isBlank)) . lines) text
  