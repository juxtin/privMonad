module Main where

import           Control.Monad.IO.Class (liftIO)
import           Priv                   (MonadPriv, Priv, PrivT, liftPriv,
                                         runPrivT)

data User = MkUser
  { name   :: Priv String
  , guid   :: Priv Integer
  , active :: Bool
  , logins :: Integer
  } deriving (Show, Eq)

data UnmaskedUser = MkUnmaskedUser
  { _name   :: String
  , _guid   :: Integer
  , _active :: Bool
  , _logins :: Integer
  } deriving (Show, Eq)

unmask :: MonadPriv m => User -> m UnmaskedUser
unmask user = do
  n <- liftPriv $ name user
  g <- liftPriv $ guid user
  let a = active user
  let l = logins user
  return $ MkUnmaskedUser n g a l

newUser :: String -> Integer -> User
newUser name guid = MkUser (pure name) (pure guid) True 0

login :: User -> User
login user = user {logins = logins user + 1}

deactivate :: User -> User
deactivate user = user { active = False }

justin :: User
justin = deactivate . login . login $ newUser "Justin" 666

wrapCurly :: String -> String
wrapCurly s = "{" ++ s ++ "}"

demo :: PrivT IO ()
demo = do
  s <- unmask justin
  liftIO $ print s

main :: IO ()
main = do
  putStrLn "Without the Priv Monad:"
  print justin
  putStrLn ""
  putStrLn "With the Priv Monad:"
  runPrivT demo
  return ()
