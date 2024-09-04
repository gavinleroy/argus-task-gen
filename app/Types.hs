{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Types where

import           Data.Text (unpack)
import           Control.Lens ((^.), (^?))
import           Data.Aeson (FromJSON, (.:), withObject, decode, parseJSON)
import           Data.Aeson.Lens (key, _String, _Integer, nth)
import           Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe (fromJust)
import           Network.HTTP.Client (newManager, responseBody, parseRequest, httpLbs, method, requestHeaders)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           System.Environment

data StudyPlan = StudyPlan
  { blockOne :: (Task, Task, Bool)
  , blockTwo :: (Task, Task, Bool)
  } deriving (Eq)

instance Show StudyPlan where
  show StudyPlan { blockOne = (ts1, ts2, wA), blockTwo = (ts1', ts2', wA') } =
    unlines
    [ txt wA
    , replicate 3 '-'
    , tsk ts1
    , tsk ts2
    , "\n"
    , txt wA'
    , replicate 3 '-'
    , tsk ts1'
    , tsk ts2'
    , "\n"
    , "\n"
    ]
    where
      txt b = if b then "With Argus" else "No Argus"
      tsk Task { crate, n, kind  } = crate ++ " " ++ show n ++ " " ++ show kind

data Exclusions = Exclusions
  { crates :: [String]
  } deriving Show

data DB = DB
  { tasks :: [Task]
  } deriving Show

data Task = Task
  { crate :: String
  , n :: Integer
  , kind :: TaskKind
  } deriving (Show, Eq)

data TaskKind = Typestate | Tree deriving (Show, Eq)

taskKind :: String -> TaskKind
taskKind "typestate" = Typestate
taskKind "tree" = Tree
taskKind _ = error "Invalid task kind"

instance FromJSON DB where
  parseJSON = withObject "DB" $ \v -> DB
    <$> v .: "results"


instance FromJSON Task where
  parseJSON v =
    let crateName o = unpack $ o ^. key "properties" . key "Crate" . key "title" . nth 0 . key "plain_text" . _String
        idN o = fromJust $ o ^? key "properties" . key "ID" . key "number" . _Integer
        kindN o = (taskKind . unpack) $ o ^. key "properties" . key "Kind" . key "select" . key "name" . _String
    in
    Task <$> pure (crateName v)
         <*> pure (idN v)
         <*> pure (kindN v)
