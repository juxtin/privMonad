module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Phi                    (MonadPrivileged, Privileged,
                                         PrivilegedT, liftPHI, runPrivilegedT)
import Data.List (intercalate)

data User = MkUser
  { name :: Privileged String
  , guid :: Privileged Integer
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

showUserPHI :: (MonadPrivileged m) => m User -> m String
showUserPHI user' = do
  user <- user'
  name' <- liftPHI $ name user
  guid' <- liftPHI $ guid user
  return . wrapCurly $ intercalate ", "
    [ "name = " ++ name'
    , "guid = " ++ show guid'
    , "active = " ++ show (active user)
    , "logins = " ++ show (logins user)
    ]

demo :: PrivilegedT IO ()
demo = do
  s <- showUserPHI $ pure justin
  liftIO $ putStrLn s

main :: IO ()
main = do
  putStrLn "Without the PHI Monad:"
  print justin
  putStrLn ""
  putStrLn "With the PHI Monad:"
  runPrivilegedT demo
  return ()
