# Haskell recipes

## Read and print to console

```haskell
main :: IO ()
main = do
  putStrLn("Hello! what is your name?")
  name <- getLine
  putStrLn("Hello " ++ name ++ "!")
```

## Print content of a file

```haskell
import System.IO

main :: IO ()
main = do
  content <- readFile "hello.txt"
  putStrLn("Content of the file:")
  putStrLn(content)
```

## Reverse content of a file and print it

```haskell
import System.IO

main :: IO ()
main = do
  content <- readFile "hello.txt"
  putStrLn("Reverse content of the file:")
  let rev = reverse content
  putStrLn(rev)
```

## Read, parse and print json from file 

```haskell
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           System.IO

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
import           GHC.Generics

data Person = Person
              { name      :: T.Text
              , firstname :: T.Text
              , age       :: Int
              } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person


main :: IO ()
main = do
  content <- L.readFile "persons.json"
  let persons = decode content :: Maybe [ Person ]
  putStrLn("Content of the file:")
  putStrLn(show persons)
```

## Read json data from http server

```haskell
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           System.IO

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
import           GHC.Generics
import           Network.HTTP.Simple


data User = User
            { id       :: Int
            , name     :: T.Text
            , username :: T.Text
            , email    :: T.Text
            , phone    :: T.Text
            , website  :: T.Text
            , address  :: Address
            , company  :: Company
            } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User


data Address = Address
               { street  :: T.Text
               , suite   :: T.Text
               , city    :: T.Text
               , zipcode :: T.Text
               , geo     :: Geo
               } deriving (Show, Generic)
instance FromJSON Address
instance ToJSON Address


data Geo = Geo
           { lat :: T.Text
           , lng :: T.Text
           } deriving (Show, Generic)
instance FromJSON Geo
instance ToJSON Geo


data Company = Company
               { name        :: T.Text
               , catchPhrase :: T.Text
               , bs          :: T.Text
               } deriving (Show, Generic)
instance FromJSON Company
instance ToJSON Company


main :: IO ()
main = do
  response <- httpLBS "http://jsonplaceholder.typicode.com/users/1"
  let status = getResponseStatusCode response
  let body = getResponseBody response
  let eitherUser = eitherDecode body :: Either String User
  case eitherUser of
    Left msg   -> putStrLn("error: " ++ msg)
    Right user -> putStrLn("user: " ++ show user)
```

## Read json data from http server

```haskell
```
