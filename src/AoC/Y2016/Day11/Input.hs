-- | Four-floor inventory, with exactly one generator and chip per element.
module AoC.Y2016.Day11.Input (Floor (FloorOne, FloorTwo, FloorThree, FloorFour), Input (Input), parseInput) where

import Data.Map.Strict qualified as Map
import Text.Megaparsec (Parsec, eof, errorBundlePretty, parse, sepBy1, try)
import Text.Megaparsec.Char (char, eol, lowerChar, string)

data Floor = FloorOne | FloorTwo | FloorThree | FloorFour
    deriving stock (Eq, Ord)

newtype Input = Input [(Floor, Floor)]

data Component = Generator | Microchip
    deriving stock (Eq, Ord)

type Parser = Parsec Void Text

parseInput :: Text -> Either Text Input
parseInput text = do
    inventory <- parse inventoryParser "facility" text & first (toText . errorBundlePretty)
    pairs <- inventory & Map.fromListWith (<>) & traverse matchPair
    pure (Input (Map.elems pairs))

inventoryParser :: Parser [(Text, [(Component, Floor)])]
inventoryParser = do
    floors <- sequence [floorParser floorNumber name | (floorNumber, name) <- zip [FloorOne, FloorTwo, FloorThree, FloorFour] ["first", "second", "third", "fourth"]]
    _ <- optional eol
    eof
    pure (concat floors)

floorParser :: Floor -> Text -> Parser [(Text, [(Component, Floor)])]
floorParser floorNumber name = do
    when (floorNumber /= FloorOne) (void eol)
    _ <- string ("The " <> name <> " floor contains ")
    items <- ([] <$ string "nothing relevant") <|> (itemParser `sepBy1` separator)
    _ <- char '.'
    pure [(element, [(component, floorNumber)]) | (element, component) <- items]

separator :: Parser Text
separator = try (string ", and ") <|> string ", " <|> string " and "

itemParser :: Parser (Text, Component)
itemParser = do
    _ <- string "a "
    element <- some lowerChar <&> toText
    component <- Generator <$ string " generator" <|> Microchip <$ string "-compatible microchip"
    pure (element, component)

matchPair :: [(Component, Floor)] -> Either Text (Floor, Floor)
matchPair components = case sort components of
    [(Generator, generator), (Microchip, microchip)] -> Right (generator, microchip)
    _invalid -> Left "Each element needs exactly one generator and one microchip"
