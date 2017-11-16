module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (intercalate)
import           Priv                   (MonadPriv, Priv, PrivT, liftPriv,
                                         runPrivT)

data User = MkUser
  { name   :: Priv String
  , guid   :: Priv Integer
  , active :: Bool
  , logins :: Integer
  } deriving (Show, Eq)

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

showUserPriv :: (MonadPriv m) => m User -> m String
showUserPriv user' = do
  user <- user'
  name' <- liftPriv $ name user
  guid' <- liftPriv $ guid user
  return . wrapCurly $ intercalate ", "
    [ "name = " ++ name'
    , "guid = " ++ show guid'
    , "active = " ++ show (active user)
    , "logins = " ++ show (logins user)
    ]

demo :: PrivT IO ()
demo = do
  s <- showUserPriv $ pure justin
  liftIO $ putStrLn s

main :: IO ()
main = do
  putStrLn "Without the Priv Monad:"
  print justin
  putStrLn ""
  putStrLn "With the Priv Monad:"
  runPrivT demo
  return ()
