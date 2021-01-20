-- | UDP Chunked support for sending messages to graylog.
module Graylog.UDP
   ( sendLog

   , module Export
   ) where

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy           as LBS
import           Data.Word
import           Network.Socket.ByteString.Lazy
import           System.Random

import           Graylog.Gelf                   as Export
import           Graylog.Types                  as Export

sendLog :: Graylog -> GELF -> IO ()
sendLog glog msg = do
   cks <- chunky glog raw
   mapM_ (send $ _graylogSocket glog) cks
   where
      raw = encode msg

chunky :: Graylog -> LBS.ByteString -> IO [LBS.ByteString]
chunky glog raw = do
   groupId <- randomIO
   let groups = divide totalNum raw
   return $ append groupId groups seqNums
   where
      magic           = word8 0x1e <> word8 0x0f
      seqNums         = [0..] :: [Word8]
      totalNum        = if excess > 0 then count + 1 else count
      (count, excess) = quotRem (LBS.length raw) gsize
      hlen            = 12
      gsize           = (fromIntegral (_graylogChunkSize glog)) - hlen

      divide   0 dat = [dat]
      divide num dat = let (pre, post) = LBS.splitAt gsize dat
                        in pre : divide (num - 1) post

      append _   []     _      = []
      append _   _      []     = error "the impossible has happened."
      append gid (g:gs) (s:ss) = (toLazyByteString
         $ magic
        <> word64BE gid
        <> word8 s
        <> word8 (fromIntegral totalNum)
        <> lazyByteString g) : append gid gs ss
