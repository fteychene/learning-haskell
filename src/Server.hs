{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Server(
    run
) where

import Prelude hiding (read)
import Web.Scotty
    ( get, html, json, jsonData, param, post, scotty, ScottyM )
import qualified Web.Scotty as Scotty
import Lib
    ( Status(Pending),
      Description(..),
      Name(..),
      Todo(Todo),
      create,
      update,
      read,
      delete )
import Data.IORef
import Control.Monad.IO.Class

import Data.Map(Map)
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map

getTodo :: IORef (Map Name Todo) -> ScottyM ()
getTodo dataRef = get "/:name" $ do
    name <- param "name"
    datas <- liftIO $ readIORef dataRef
    case read datas (Name name) of
        Left error -> html $ T.pack $ show error
        Right todo -> json todo

createTodo :: IORef (Map Name Todo) -> ScottyM ()
createTodo dataRef = post "/create" $ do
    newTodo <- jsonData 
    datas <- liftIO $ readIORef dataRef
    case create datas newTodo of
        Left error -> html $ T.pack $ show error
        Right updated -> successResponse updated
    where
        successResponse updated = do
            liftIO $ writeIORef dataRef updated
            html $ T.pack "Todo created"

updateTodo :: IORef (Map Name Todo) -> ScottyM () 
updateTodo dataRef = post "/update" $ do
    newTodo <- jsonData
    datas <- liftIO $ readIORef dataRef
    case update datas newTodo of
        Left error -> html $ T.pack $ show error
        Right updated -> successResponse updated
    where
        successResponse updated = do
            liftIO $ writeIORef dataRef updated
            html $ T.pack "Todo updated"

deleteTodo :: IORef (Map Name Todo) -> ScottyM () 
deleteTodo dataRef = Scotty.delete "/delete/:name" $ do
    name <- param "name"
    datas <- liftIO $ readIORef dataRef
    case delete datas (Name name) of
        Left error -> html $ T.pack $ show error
        Right updated -> successResponse updated
    where
        successResponse updated = do
            liftIO $ writeIORef dataRef updated
            html $ T.pack "Todo deleted"

run :: IO ()
run = 
    let name = Name "Hello"
        datas = Map.fromList [(name, Todo name (Description "Bonjour Twitch") Pending)]
    in do 
        refData <- newIORef datas
        scotty 3000 $ do 
            getTodo refData 
            createTodo refData
            updateTodo refData
            deleteTodo refData