-- | Four-floor inventory, with exactly one generator and chip per element.
module AoC.Y2016.Day11.Input (Floor (FloorOne, FloorTwo, FloorThree, FloorFour), Component (Generator, Microchip), Locations (generatorFloor, microchipFloor), Input (Input), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Map.Strict qualified as Map
import Text.Megaparsec (sepBy1, try)
import Text.Megaparsec.Char (char, eol, lowerChar, string)

data Floor = FloorOne | FloorTwo | FloorThree | FloorFour
    deriving stock (Eq, Ord)

newtype Input = Input (Map Text Locations)

data Locations = Locations {generatorFloor :: Floor, microchipFloor :: Floor}

data Component = Generator | Microchip
    deriving stock (Eq, Ord)

parseInput :: Text -> Either Text Input
parseInput text = do
    inventory <- parseText inventoryParser text
    pairs <- inventory & Map.fromListWith (<>) & traverse matchPair
    pure (Input pairs)

inventoryParser :: Parser [(Text, [(Component, Floor)])]
inventoryParser = do
    floors <- sequence [floorParser floorNumber name | (floorNumber, name) <- zip [FloorOne, FloorTwo, FloorThree, FloorFour] ["first", "second", "third", "fourth"]]
    _ <- optional eol
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

matchPair :: [(Component, Floor)] -> Either Text Locations
matchPair components = case sort components of
    [(Generator, generator), (Microchip, microchip)] -> Right (Locations generator microchip)
    _invalid -> Left "Each element needs exactly one generator and one microchip"
