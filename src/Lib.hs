{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( create,
      delete,
      update,
      read,
      Todo(..),
      Name(..),
      Description(..),
      Status(..),
      Error(..)
    ) where

import Prelude hiding (sum, read)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text(..))
import qualified Data.Maybe as Maybe

import GHC.Generics(Generic)
import Data.Aeson(ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions)

data Todo = Todo {
    name :: Name
    , description :: Description
    , status :: Status
} deriving(Generic, Show)

newtype Name = Name Text deriving (Generic, Show, Eq, Ord)

newtype Description  = Description Text deriving(Generic, Show)

data Status = InProgress | Completed | Pending deriving(Generic, Show)

data Error = AlreadyExistingTodo Name 
            | TodoNotFound Name
            deriving (Show)
    
create :: Map Name Todo -> Todo -> Either Error (Map Name Todo)
create datas todo = case Map.lookup (name todo) datas of
    Just _ -> Left $ AlreadyExistingTodo (name todo)
    Nothing -> Right $ Map.insert (name todo) todo datas

update :: Map Name Todo -> Todo -> Either Error (Map Name Todo)
update datas todo = case Map.lookup (name todo) datas of
    Just _ -> Right $ Map.insert (name todo) todo datas
    Nothing -> Left $ TodoNotFound (name todo)

delete :: Map Name Todo -> Name -> Either Error (Map Name Todo)
delete datas name = case Map.lookup name datas of
    Just _ -> Right $ Map.delete name datas
    Nothing -> Left $ TodoNotFound name

read :: Map Name Todo 
     -> Name 
     -> Either Error Todo
read datas name = 
    let error = Left $ TodoNotFound name
     in Maybe.maybe error Right $ Map.lookup name datas

-- Technical generating Json stuffs
instance ToJSON Todo where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Name where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Description where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Status where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Todo
instance FromJSON Name
instance FromJSON Description
instance FromJSON Status