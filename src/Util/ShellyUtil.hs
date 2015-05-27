{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Util.ShellyUtil where

import Data.String
import Shelly
import qualified Data.Text as T
default (T.Text)

tail_f name f = shelly $ do
  runHandle "tail" ["-n", "0", "-f", fromString name] $ \h -> liftIO $ f h