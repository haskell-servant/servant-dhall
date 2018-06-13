{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Maybe         (fromMaybe)
import Network.Wai        (Application)
import Servant
import Servant.Dhall
import System.Environment (getArgs, lookupEnv)
import Text.Read          (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

type API = "post" :> ReqBody '[DHALL] [Integer] :> Post '[DHALL] [Integer]
      :<|> "get" :> Get '[DHALL] [Integer]

api :: Proxy API
api = Proxy

server :: Server API
server = pure . map (+1) :<|> pure [1..10]

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        _ -> putStrLn "To run, pass run argument"
