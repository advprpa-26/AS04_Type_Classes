module MapReduce.WordCount where

import           Control.Monad (filterM)
import           Data.Char (toLower, isAlphaNum)
import           Data.List (sortOn, isSuffixOf)
import           Data.Monoid (Sum(..))
import           Data.Ord (Down(..))
import           System.Directory (listDirectory, doesFileExist)
import           Map.TreeMap

-- Mapping from String to Sum Int.
type WordCount = Map String (Sum Int)

-- Translates all characters to lower case.
lower :: String -> String
lower = map toLower

-- Replaces non-alphanumeric characters with spaces.
clear :: String -> String
clear = map (\c -> if isAlphaNum c then c else ' ')

-- Takes an input text, cleans it, and splits it into a list of words.
cleanWords :: String -> [String]
cleanWords = words . clear . lower

-- Takes an input text, constructs pairs ("WORD", Sum 1), and builds the Map.
wordCount :: String -> WordCount
wordCount = fromListMerge . map (\w -> (w, Sum 1)) . cleanWords

-- Maps each element into a Monoid m and reduces them with mconcat.
mapReduce :: Monoid m => (a -> m) -> [a] -> m
mapReduce _ []    = mempty
mapReduce f (a:l) = f a <> mapReduce f l

-- Main program, loads text files and calculates the word count across all of them.
main :: IO ()
main = do
  elements <- listDirectory "."
  files    <- filterM doesFileExist elements
  let txtFiles = filter (isSuffixOf ".txt") files
  contents <- mapM readFile txtFiles
  let result = mapReduce wordCount contents
  putStrLn (showResults result)

showResults :: WordCount -> String
showResults m = concat ["count: ", show (length kvs), "\ntop10: ", show (take 10 topN)]
  where topN = sortOn (Down . snd) kvs
        kvs  = toList m