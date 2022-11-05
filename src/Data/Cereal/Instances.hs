{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Cereal.Instances where

import Data.Cereal.TH
import Data.Aeson (Value(..), Object)

$(makeCereal ''Value)
