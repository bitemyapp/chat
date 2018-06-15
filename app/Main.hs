module Main where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import Data.Conduit
import Data.Conduit.Network
import qualified Data.Streaming.Network as SN
import qualified Safe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- ConduitT i ByteString m () -> IO ()
-- ConduitT i ByteString IO () -> IO ()
-- runConduit :: Monad m => ConduitT () Void m r -> m r

-- appSink ad =
--   awaitForever $ \d ->
--     liftIO $ SN.appWrite ad d >> Conc.yield

data Message =
    MsgInt Int
  | MsgDouble Double
  -- | MsgCreateUser User Token
  deriving (Show)

parseMessage :: BS.ByteString -> Maybe Message
parseMessage d = do
  let t = T.unpack $ T.decodeUtf8 d
      maybeInt = MsgInt <$> (Safe.readMay t)
      maybeDouble = MsgDouble <$> (Safe.readMay t)
    in maybeInt <|> maybeDouble

myAppSink ad = do
  awaitForever $ \d ->
    liftIO $ do
      let parsed = parseMessage d
      putStrLn "Hey we got data, it was: "
      print parsed
      putStrLn "^^^^^^^"
      SN.appWrite ad d

main :: IO ()
main = do
  let serSet = serverSettings 9001 "127.0.0.1"
      runServer :: AppData -> IO ()
      runServer appData = do
        let conduitSource = appSource appData
            conduitSink = myAppSink appData
        -- conduitSource $$ conduitSink
        runConduit $ conduitSource .| conduitSink

  runTCPServer serSet runServer
  putStrLn "Hello, world!"
