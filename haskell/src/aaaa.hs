{-# LANGUAGE DeriveGeneric #-}

-- Importação de módulos 

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.List (words, foldl')
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Data.Csv
import GHC.Generics