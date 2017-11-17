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

instance FromJSON (P.Priv String) where
  parseJSON = withText "Priv String" $ \text ->
    pure $ hide (T.unpack text)

instance FromJSON (P.Priv Integer) where
  parseJSON = withScientific "Priv Integer" $
    pure . hide . round

data Patient = Patient
  { name           :: P.Priv String
  , age            :: P.Priv Integer
  , favoriteNumber :: Integer
  } deriving (Generic, Show)

instance FromJSON Patient

jJson :: B.ByteString
jJson = "{\"name\":\"Justin\",\"age\":666,\"favoriteNumber\":13}"

justin :: Patient
justin = let (Just j) = decode jJson in
           j
