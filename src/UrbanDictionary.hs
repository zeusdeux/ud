{-# LANGUAGE DeriveGeneric #-}
module UrbanDictionary
    ( ud
    ) where

import Control.Monad
import Data.List
import System.Environment
import Network.Wreq (get)
import GHC.Generics
import Data.Aeson
-- import Data.ByteString.Lazy.Internal

endpoint :: String
endpoint = "http://api.urbandictionary.com/v0/define?term="

data DefinitionObject = DefinitionObject {
  defid :: Int
  , word :: String
  , author :: String
  , permalink :: String
  , definition :: String
  , example :: String
  , thumbs_up :: Int
  , thumbs_down :: Int
  , current_vote :: String
  } deriving (Generic, Show)

instance ToJSON DefinitionObject where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DefinitionObject


data UDResponse = UDResponse {
  tags :: [String]
  , result_type :: String
  , list :: [DefinitionObject]
  , sounds :: [String]
  } deriving (Generic, Show)

instance ToJSON UDResponse where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON UDResponse



ud :: IO ()
ud = do
  args <- getArgs
  r <- getUd (head args)
  print r

type Term = String
getUd t = get $ endpoint ++ t


-- mapM' :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
-- mapM' f xs = sequence $ fmap f xs

-- mapM_' :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
-- mapM_' f xs = foldr (\x acc -> (>>) (f x) acc) (return ()) xs


-- -- foldM' (\acc e -> print e; IO acc) IO () [1, 2, 3]
-- foldM' :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
-- foldM' f acc xs = undefined


-- class Tofu t where
--   tofu :: j a -> t a j

-- -- j :: * -> *
-- -- t :: * -> (* -> *) -> *

-- data P a j = PVal {pval :: j a} deriving (Show)

-- -- P Int Maybe
-- -- P 10 Maybe

-- data X = Bool | String deriving (Show)

-- instance Tofu P where
--   tofu x = PVal x
