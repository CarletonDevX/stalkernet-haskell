module Main where

import           Stalkernet.Types
import           Stalkernet.Parsing
import           Stalkernet.Fetching
import           Data.Either.Extra

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Serialize
import           Text.PrettyPrint.ANSI.Leijen (putDoc, pretty)
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = handle handler $ do
    args <- getArgs
    case args of
        ["fetch", ithreads, iw] -> maybeDo (liftA2 (,) (maybeRead ithreads) (writer iw)) $ \(threads, w) -> do
            -- TChan for counting pages, TVar for storing people
            (ticks, ppl) <-  atomically $ (,) <$> newTChan <*> newTVar ([] :: [Person])
            let -- Loop to update progress message in stderr
                prog n = do
                    renderProgress n
                    atomically $ readTChan ticks
                    prog (n + 1)
                -- Actual fetching
                fetch = fetchPeople threads $ \ps' -> atomically $ do
                    writeTChan ticks ()
                    ps <- readTVar ppl
                    writeTVar ppl (ps' ++ ps)
            -- Execute and cleanup
            race fetch (prog 0)
            -- Make stuff look nice
            renderProgress 676 >> hPutChar stderr '\n'
            -- Write collected people
            atomically (readTVar ppl) >>= w
        ["dump", ir, iw] -> maybeDo (liftA2 (,) (reader ir) (writer iw)) $ \(r, w) -> do
            L.getContents >>= (maybe (error "Parse error.") w . r)
        _ -> oops
  where
    maybeDo = flip (maybe oops)
    maybeRead = fmap (fst . fst) . uncons . reads
    renderProgress n = hPutStr stderr $ "\r\ESC[K" ++ show n ++ "/676 pages scraped"
    oops = hPutStr stderr (unlines usage)
    usage = [ "Usage: stalk fetch <threads> (binary|json|pretty)"
            , "       - fetch all data from stalkernet and"
            , "         write it to stdout in the given format,"
            , "         with a maximum of <threads> outstanding"
            , "         requests at once."
            , ""
            , "       stalk dump (binary|json) (binary|json|pretty)"
            , "       - read stalkernet date from stdin in the first"
            , "         format, and write it to stdout in the second."
            ]

handler :: SomeException -> IO ()
handler e = do
    hPutStrLn stderr $ "\nstalk crashed: " ++ displayException e
    exitFailure

-- | Parse a person parser from command line argument
reader :: String -> Maybe (L.ByteString -> Maybe [Person])
reader "binary" = Just (eitherToMaybe . Data.Serialize.decodeLazy)
reader "json" = Just Data.Aeson.decode
reader _ = Nothing

-- | Parse a person sink from command line argument
writer :: String -> Maybe ([Person] -> IO ())
writer "binary" = Just $ (L.putStr . Data.Serialize.encodeLazy)
writer "json" = Just $ (hPutBuilder stdout . Data.Aeson.Encode.encodeToBuilder . Data.Aeson.toJSON)
writer "pretty" = Just $ (putDoc . pretty)
writer _ = Nothing
