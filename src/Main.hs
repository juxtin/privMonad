module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Phi                    (MonadPrivileged, Privileged,
                                         PrivilegedT, liftPHI, runPrivilegedT)

myName :: Privileged String
myName = pure "Justin"

myId :: Privileged Integer
myId = pure 666

-- Note that the type signature makes no mention of PHI:
demoBad :: (MonadIO io) => io ()
demoBad = do
  liftIO . putStrLn $ "ID: " ++ show myId
  liftIO . putStrLn $ "Hello, " ++ show myName ++ "."

-- MonadPrivileged noted in the type:
demoGood :: (MonadPrivileged m, MonadIO m) => m ()
demoGood = do
  name <- liftPHI myName
  id <- liftPHI myId
  liftIO $ putStrLn $ "ID: " ++ show id
  liftIO $ putStrLn $ "Hello, " ++ name ++ "."
  return ()

main :: IO ()
main = do
  putStrLn "Without the PHI Monad:"
  demoBad
  putStrLn "With the PHI Monad:"
  runPrivilegedT demoGood
  return ()
