{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- | Default formatting for Graylog messages,
-- see http://docs.graylog.org/en/latest/pages/gelf.html
module Graylog.Gelf where

import           Data.Aeson          (ToJSON (..), Value (..), object, toJSON,
                                      (.=))
import           Data.HashMap.Strict (HashMap)
import           Data.Scientific     (Scientific)
import           Data.Text           (Text)
import           Data.Time
import           Data.Typeable
import           GHC.Exts            (toList)
import           GHC.Generics

data GELF
   = GELF
      { _gelfVersion      :: Version
      , _gelfHost         :: Text
      , _gelfShortMessage :: Text
      , _gelfFullMessage  :: Maybe Text
      , _gelfTimestamp    :: Maybe UTCTime
      , _gelfLevel        :: Maybe SyslogLevel
      , _gelfLine         :: Maybe Word
      , _gelfFile         :: Maybe Text
      , _gelfMeta         :: HashMap Text MetaValue
      }
   deriving (Show, Typeable, Generic)

data MetaValue
  = TextValue Text
  | NumberValue Scientific
  deriving (Show)

instance ToJSON MetaValue where
  toJSON (TextValue txt) = toJSON txt
  toJSON (NumberValue n) = toJSON n

class ToMeta a where
  toMeta :: a -> MetaValue

instance ToMeta Text where
  toMeta = TextValue

instance ToMeta Scientific where
  toMeta = NumberValue

instance ToMeta Integer where
  toMeta = NumberValue . fromInteger

instance ToMeta Int where
  toMeta = toMeta . toInteger

instance ToJSON GELF where
  toJSON GELF{..} = object $ [ "version"        .= _gelfVersion
                             , "host"           .= _gelfHost
                             , "short_message"  .= _gelfShortMessage
                             , "full_message"   .= _gelfFullMessage
                             , "timestamp"      .= _gelfTimestamp
                             , "level"          .= _gelfLevel
                             , "line"           .= _gelfLine
                             , "file"           .= _gelfFile
                             ] <> toList (toJSON <$> _gelfMeta)

--

data Version
   = Version1x1
   deriving (Eq, Show, Typeable, Generic)

instance ToJSON Version where
   toJSON Version1x1 = String "1.1"

--

data SyslogLevel
   = Emergency
   | Alert
   | Critical
   | Error
   | Warning
   | Notice
   | Informational
   | Debug
   deriving (Eq, Ord, Show, Typeable, Generic)

instance ToJSON SyslogLevel where
   toJSON Emergency     = Number 0
   toJSON Alert         = Number 1
   toJSON Critical      = Number 2
   toJSON Error         = Number 3
   toJSON Warning       = Number 4
   toJSON Notice        = Number 5
   toJSON Informational = Number 6
   toJSON Debug         = Number 7

--

simpleGelf
   :: Text     -- ^ Hostname
   -> Text     -- ^ Short message
   -> GELF
simpleGelf host short =
  GELF Version1x1 host short Nothing Nothing Nothing Nothing Nothing mempty
