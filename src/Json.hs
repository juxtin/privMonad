{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import           Data.Aeson
import           Data.ByteString.Lazy as B
import           Data.Text            as T
import           GHC.Generics
import qualified Priv                 as P

hide :: a -> P.Priv a
hide = pure

instance FromJSON a => FromJSON (P.Priv a) where
  parseJSON = fmap hide . parseJSON

data Patient = Patient
  { name           :: P.Priv String
  , age            :: P.Priv Integer
  , dead           :: P.Priv Bool
  , favoriteNumber :: Integer
  } deriving (Generic, Show)

instance FromJSON Patient

jJson :: B.ByteString
jJson = "{\"name\":\"Justin\",\"age\":666,\"favoriteNumber\":13,\"dead\":true}"

justin :: Patient
justin = let (Just j) = decode jJson in
           j
