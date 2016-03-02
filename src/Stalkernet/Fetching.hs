{-# LANGUAGE OverloadedStrings #-}

module Stalkernet.Fetching ( fetchPeople
                           , fetchPeopleSafe
                           , fetchPeopleSafe'
                           ) where

import           Stalkernet.Types
import           Stalkernet.Parsing

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (singleton)
import           Network.Wreq (params, getWith, defaults, responseBody, Response)
import           Text.HTML.DOM

-- | Results of searching for people in the directory whose first name
-- starts with the first argument and whose last name starts with the second
request :: Char -> Char -> IO (Response ByteString)
request first last = getWith opts root
  where
    opts = defaults & params .~ assocs
    assocs = [("first_name", singleton first), ("last_name", singleton last)]
    root = "http://apps.carleton.edu/campus/directory"

-- | Fetches data using the given number of threads, and asynchronously executes
-- the given action on each batch of results
fetchPeople :: Int -> ([Person] -> IO ()) -> IO ()
fetchPeople threads io = void $ do
    sem <- newQSem threads
    let go (first, last) = do
          waitQSem sem
          resp <- request first last
          signalQSem sem
          let doc = parseLBS $ resp ^. responseBody
          either error io $ people doc
    -- Changing these enumerations is nice for testing.
    -- Is there a better way?
    mapConcurrently go $ liftA2 (,) ['a'..'z'] ['a'..'z']

-- | Same as `fetchPeople` except the action is executed atomically
fetchPeopleSafe :: Int -> ([Person] -> IO ()) -> IO ()
fetchPeopleSafe threads io = void $ do
    sem <- newMVar ()
    fetchPeople threads $ \ppl -> do
        takeMVar sem 
        io ppl
        putMVar sem ()

-- | Same as `fetchPeopleSafe`, with a `TChan` based implementation.
-- Is there any advantage of having all effects in one thread,
-- or is the other implementation just as good?
fetchPeopleSafe' :: Int -> ([Person] -> IO ()) -> IO ()
fetchPeopleSafe' threads io = void $ do
    -- TChan for chunks of results, TVar for status
    (chan, done) <- atomically $ (,) <$> newTChan <*> newTVar False
    -- Read from chan and act until fetching is done
    let go = do mppl <- atomically $ do
                    end <- (&&) <$> readTVar done <*> isEmptyTChan chan
                    if end
                     then return Nothing
                     else Just <$> readTChan chan
                case mppl of
                    Nothing -> return ()
                    Just ppl -> io ppl >> go
    concurrently go $ do
        -- Fetch and report when done
        fetchPeople 10 (atomically . writeTChan chan)
        atomically (writeTVar done True)

