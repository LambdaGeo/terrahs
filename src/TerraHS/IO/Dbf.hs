-- | Reading DBF (dBASE III) files, the format Shapefile uses to
-- store attributes — the equivalent of the original TerraHS's
-- @TeTable@.
--
-- A much simpler format than @.shp@: a fixed header, a list of
-- fixed-size field descriptors, and then fixed-width text records —
-- yes, even numbers are stored as ASCII text, right-aligned.
--
-- Covers the most common field types: @C@ (Character/text), @N@\/@F@
-- (Numeric\/Float), and @L@ (Logical/boolean). @D@ (date) and @M@
-- (memo) fields are read as raw text.
module TerraHS.IO.Dbf
  ( DbfField (..)
  , DbfValue (..)
  , DbfTable (..)
  , parseDbf
  , readDbf
  ) where

import Control.Monad (replicateM)
import Data.Binary.Get
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, isSpace)
import Text.Read (readMaybe)

-- | A field descriptor: name, type (dBASE letter: C, N, F, L, D...),
-- width in bytes, and decimal places (only relevant for numeric
-- fields).
data DbfField = DbfField
  { dbfFieldName     :: String
  , dbfFieldType     :: Char
  , dbfFieldLength   :: Int
  , dbfFieldDecimals :: Int
  } deriving (Eq, Show)

-- | A field's value, already interpreted according to its
-- descriptor's type.
data DbfValue
  = DbfText String
  | DbfNumber Double
  | DbfBool (Maybe Bool) -- ^ 'Nothing' represents dBASE's "undefined" ('?') value
  | DbfNull               -- ^ an empty field
  deriving (Eq, Show)

-- | The full table: the field descriptors (in the order they appear
-- in the file) and one record per row, as a list of (field name,
-- value) pairs in the same order as the descriptors.
data DbfTable = DbfTable
  { dbfFields  :: [DbfField]
  , dbfRecords :: [[(String, DbfValue)]]
  } deriving (Eq, Show)

trimNuls :: String -> String
trimNuls = takeWhile (/= '\NUL')

trimSpacesBoth :: String -> String
trimSpacesBoth = f . f
  where f = reverse . dropWhile isSpace

data DbfHeader = DbfHeader
  { dhNumRecords :: Int
  , dhHeaderSize :: Int
  , dhRecordSize :: Int
  } deriving (Show)

getHeader :: Get DbfHeader
getHeader = do
  _version    <- getWord8
  skip 3                                     -- last-update date (YY MM DD)
  numRecords  <- fromIntegral <$> getWord32le
  headerSize  <- fromIntegral <$> getWord16le
  recordSize  <- fromIntegral <$> getWord16le
  skip 20                                    -- rest of the 32-byte fixed header, unused here
  pure (DbfHeader numRecords headerSize recordSize)

getFieldDescriptor :: Get DbfField
getFieldDescriptor = do
  nameBytes <- getByteString 11
  typeByte  <- getWord8
  skip 4                                     -- field address (unused when reading)
  len       <- fromIntegral <$> getWord8
  dec       <- fromIntegral <$> getWord8
  skip 14                                    -- rest of the 32-byte descriptor, unused here
  pure DbfField
    { dbfFieldName     = trimNuls (BC.unpack nameBytes)
    , dbfFieldType     = chr (fromIntegral typeByte)
    , dbfFieldLength   = len
    , dbfFieldDecimals = dec
    }

-- | Field descriptors end with a marker byte, @0x0D@.
getFieldDescriptors :: Get [DbfField]
getFieldDescriptors = do
  marker <- lookAhead getWord8
  if marker == 0x0D
    then getWord8 >> pure []
    else (:) <$> getFieldDescriptor <*> getFieldDescriptors

interpretValue :: Char -> String -> DbfValue
interpretValue _ "" = DbfNull
interpretValue 'N' s = numberOrNull s
interpretValue 'F' s = numberOrNull s
interpretValue 'L' s = case s of
  (c : _) | c `elem` ("TtYy" :: String) -> DbfBool (Just True)
  (c : _) | c `elem` ("FfNn" :: String) -> DbfBool (Just False)
  _                                     -> DbfBool Nothing
interpretValue _ s = DbfText s

numberOrNull :: String -> DbfValue
numberOrNull s = maybe DbfNull DbfNumber (readMaybe (dropWhile (== '+') s))

getFieldValue :: DbfField -> Get (String, DbfValue)
getFieldValue field = do
  raw <- getByteString (dbfFieldLength field)
  let txt = trimSpacesBoth (BC.unpack raw)
  pure (dbfFieldName field, interpretValue (dbfFieldType field) txt)

getRecord :: [DbfField] -> Get [(String, DbfValue)]
getRecord fields = do
  _deletionFlag <- getWord8 -- ' ' = valid, '*' = marked as deleted
  mapM getFieldValue fields

getDbf :: Get DbfTable
getDbf = do
  hdr     <- getHeader
  fields  <- getFieldDescriptors
  records <- replicateM (dhNumRecords hdr) (getRecord fields)
  pure (DbfTable fields records)

-- | Parses the bytes of a @.dbf@ file already loaded in memory.
parseDbf :: BL.ByteString -> Either String DbfTable
parseDbf bytes =
  case runGetOrFail getDbf bytes of
    Left (_, _, err)     -> Left err
    Right (_, _, table)  -> Right table

-- | Reads and parses a @.dbf@ file from disk.
readDbf :: FilePath -> IO (Either String DbfTable)
readDbf path = parseDbf <$> BL.readFile path
