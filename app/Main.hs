{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import           Types

import Control.Monad
import Data.Aeson (decode)
import Data.Array.IO
import Data.Bits (xor)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Network.HTTP.Client (newManager, responseBody, parseRequest, httpLbs, method, requestHeaders)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Options.Applicative.Simple
import System.Environment
import System.Random


loadData :: String -> String -> IO DB
loadData dbId apiKey = do
  let url = "https://api.notion.com/v1/databases/" ++ dbId ++ "/query"
  iRequest <- parseRequest url
  let request = iRequest
        { method = "POST"
        , requestHeaders =
                [ ("Authorization", pack $ "Bearer " ++ apiKey)
                , ("Notion-Version", pack "2022-06-28")
                ]
        }

  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return . fromJust . decode $ responseBody response


shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs


dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) | x `elem` xs = dedup xs
             | otherwise   = x : dedup xs

            
isValidStudy :: StudyPlan -> Bool
isValidStudy StudyPlan
  { blockOne = (ts1, ts1', isArgusFirst)
  , blockTwo = (ts2, ts2', isArgusFirst')
  } =
  -- none can be the same
  all'neq [ts1, ts1', ts2, ts2'] &&
  -- cannot use (or not use) argus twice
  xor isArgusFirst isArgusFirst' &&
  -- one block cannot have the same kind twice
  kind ts1 /= kind ts1' &&
  kind ts2 /= kind ts2' &&
  -- needs to be two synthetic crates of the same kind, but different crate
  2 == length synthetics &&
  all'eq (map kind synthetics) &&
  all'neq (map crate synthetics) &&
  -- needs to be two non-synthetic crates of the same kind, but different
  2 == length real'worlds &&
  all'eq (map kind real'worlds) &&
  all'neq real'worlds
  where
    synthetics = filter isSynthetic [ts1, ts1', ts2, ts2']
    real'worlds = filter (not . isSynthetic) [ts1, ts1', ts2, ts2']
    all'eq = (1 ==) . length . dedup
    all'neq ls = (length ls ==) . length . dedup $ ls
    isSynthetic Task { crate } = elem crate ["space", "brew"]



allStudies :: DB -> [StudyPlan]
allStudies DB { tasks } = do
  ts1 <- tasks
  ts1' <- tasks
  ts2 <- tasks
  ts2' <- tasks
  b <- [True, False]
  return StudyPlan
    { blockOne = (ts1, ts1', b)
    , blockTwo = (ts2, ts2', not b)
    }

validStudySpace :: DB -> [StudyPlan]
validStudySpace = dedup . filter isValidStudy . allStudies

parseOptions :: Parser Exclusions
parseOptions = Exclusions
  <$> many (argument str (metavar "STRING" <> help "A list of strings"))

main :: IO ()
main = do
  (Exclusions { crates }, ()) <- simpleOptions
    "0.1"
    "Study Plan Generator"
    "Figure it out yourself"
    parseOptions
    empty
  dbId <- getEnv "NOTION_DB_ID"
  apiKey <- getEnv "NOTION_API_KEY"
  db <- loadData dbId apiKey
  shuffle $ validStudySpace db
  >>= print
