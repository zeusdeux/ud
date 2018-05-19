module UrbanDictionary
    ( ud
    ) where

import System.Environment
import Data.List

ud :: IO ()
ud = do
  args <- getArgs
  mapM_ print args
