-- | Reading and writing WKT (Well-Known Text).
--
-- Example: @POLYGON ((0 0, 4 0, 4 4, 0 4, 0 0))@.
--
-- Implemented as a hand-written parser combinator, without depending
-- on @parsec@/@megaparsec@ — the goal here is didactic: showing how
-- 'Functor', 'Applicative', and 'Monad' combine for parsing, without
-- hiding the mechanics behind a library.
--
-- Supports only @POINT@, @LINESTRING@, and @POLYGON@ (no holes, no
-- MULTI*, no Z/M) — enough for the types that 'TerraHS.Geometry'
-- already models.
module TerraHS.IO.WKT
  ( parseWKT
  , renderWKT
  ) where

import Data.Char (isDigit, isSpace, toUpper)
import Data.List (intercalate)

import TerraHS.Geometry.Coord (Coord (..))
import TerraHS.Geometry.Point (Point (..))
import TerraHS.Geometry.Line (Line (..), mkLine)
import TerraHS.Geometry.Polygon (Polygon (..), mkPolygon)
import TerraHS.Geometry.Any (AnyGeometry (..))

-- * A minimal parser combinator

-- | A parser that consumes a prefix of a 'String' and returns a value
-- plus the unconsumed remainder, or fails with 'Nothing'.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Nothing        -> Nothing
    Just (a, rest) -> Just (f a, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser pf <*> Parser pa = Parser $ \s -> do
    (f, rest)   <- pf s
    (a, rest')  <- pa rest
    pure (f a, rest')

instance Monad Parser where
  Parser p >>= f = Parser $ \s -> do
    (a, rest) <- p s
    runParser (f a) rest

-- | Tries the first parser; if it fails, tries the second, without
-- consuming input on failure (full backtracking, simple here because
-- the input is always an immutable 'String').
orElse :: Parser a -> Parser a -> Parser a
orElse (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of
  Nothing -> p2 s
  r       -> r

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
  (c : cs) | predicate c -> Just (c, cs)
  _                      -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

many0 :: Parser a -> Parser [a]
many0 p = orElse (do { x <- p; xs <- many0 p; pure (x : xs) }) (pure [])

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many0 p
  pure (x : xs)

spaces :: Parser ()
spaces = () <$ many0 (satisfy isSpace)

-- | Consumes a parser and any whitespace right after it.
token :: Parser a -> Parser a
token p = p <* spaces

-- | Matches a keyword case-insensitively (WKT is typically written in
-- uppercase, but that isn't mandatory).
keyword :: String -> Parser String
keyword kw = token (mapM matchChar kw)
  where
    matchChar c = satisfy (\x -> toUpper x == toUpper c)

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x  <- p
  xs <- many0 (sep >> p)
  pure (x : xs)

-- | A floating-point number, with an optional sign.
double :: Parser Double
double = token $ do
  sign  <- orElse (fmap (: []) (char '-')) (pure "")
  intP  <- many1 (satisfy isDigit)
  fracP <- orElse (do { _ <- char '.'; ds <- many1 (satisfy isDigit); pure ('.' : ds) }) (pure "")
  pure (read (sign ++ intP ++ fracP))

coord :: Parser Coord
coord = Coord <$> double <*> double

parens :: Parser a -> Parser a
parens p = token (char '(') *> p <* token (char ')')

coordList :: Parser [Coord]
coordList = sepBy1 coord (token (char ','))

-- * WKT grammar

pointBody :: Parser Point
pointBody = Point <$> parens coord

lineBody :: Parser (Either String Line)
lineBody = toLine <$> parens coordList
  where
    toLine cs = maybe (Left ("LINESTRING needs at least 2 points: " ++ show cs)) Right (mkLine cs)

polygonBody :: Parser (Either String Polygon)
polygonBody = toPolygon <$> parens (parens coordList)
  where
    toPolygon cs = maybe (Left ("POLYGON needs at least 3 distinct vertices: " ++ show cs)) Right (mkPolygon cs)

geometry :: Parser (Either String AnyGeometry)
geometry =
  orElse (fmap (Right . AGPoint)   (keyword "POINT" *> pointBody))
  (orElse (fmap (fmap AGLine)      (keyword "LINESTRING" *> lineBody))
          (fmap (fmap AGPolygon)   (keyword "POLYGON" *> polygonBody)))

-- | Parses a WKT string into an 'AnyGeometry'.
--
-- >>> parseWKT "POINT (1 2)"
-- Right (AGPoint (Point {pointCoord = Coord {coordX = 1.0, coordY = 2.0}}))
parseWKT :: String -> Either String AnyGeometry
parseWKT input =
  case runParser (spaces *> geometry) input of
    Nothing              -> Left ("invalid WKT, could not parse input: " ++ input)
    Just (Left err, _)   -> Left err
    Just (Right g, rest)
      | all isSpace rest -> Right g
      | otherwise        -> Left ("unconsumed trailing text after the geometry: " ++ rest)

-- * Rendering (the reverse direction, useful for round-tripping and debugging)

renderCoord :: Coord -> String
renderCoord (Coord x y) = show x ++ " " ++ show y

-- | Serializes an 'AnyGeometry' back to WKT.
renderWKT :: AnyGeometry -> String
renderWKT (AGPoint (Point c)) =
  "POINT (" ++ renderCoord c ++ ")"
renderWKT (AGLine (Line cs)) =
  "LINESTRING (" ++ intercalate ", " (map renderCoord cs) ++ ")"
renderWKT (AGPolygon (Polygon ring)) =
  "POLYGON ((" ++ intercalate ", " (map renderCoord ring) ++ "))"
