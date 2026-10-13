{- | Rectangular ducts with unique numbered locations and a required start.
Coordinates are (row, column); only passages and locations are walkable.
-}
module AoC.Y2016.Day24.Input (Input (passages, locations), parseInput) where

import AoC.Parse (Parser, parseText)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Text.Megaparsec (oneOf, sepEndBy1)
import Text.Megaparsec.Char (char, eol)

-- | Walkable cells and numbered targets in zero-based (row, column) coordinates.
data Input = Input
    { passages :: Set (Int, Int)
    -- ^ Walkable cells, including numbered locations.
    , locations :: Map Int (Int, Int)
    -- ^ Unique digit labels mapped to walkable cells; includes label 0.
    }

data Cell = Wall | Passage | Location Int

{- | Accept a rectangular map of walls, passages and single-digit locations; reject
duplicate numbers, missing location 0 and ragged rows. Errors are returned as text.
-}
parseInput :: Text -> Either Text Input
parseInput text = do
    rows <- parseText (some cell `sepEndBy1` eol) text
    unless (size (ordNub (map size rows)) == 1) (Left "Ragged duct map")
    let cells = [((row, column), value) | (row, values) <- zip [0 ..] rows, (column, value) <- zip [0 ..] values]
        numbered = [(number, position) | (position, Location number) <- cells]
        points = Map.fromList numbered
    unless (Map.size points == size numbered) (Left "Duplicate numbered location")
    unless (Map.member 0 points) (Left "Missing starting location 0")
    pure (Input (Set.fromList [position | (position, value) <- cells, walkable value]) points)

cell :: Parser Cell
cell = (Wall <$ char '#') <|> (Passage <$ char '.') <|> (oneOf ['0' .. '9'] <&> \digit -> Location (ord digit - ord '0'))

walkable :: Cell -> Bool
walkable = \case
    Wall -> False
    Passage -> True
    Location _ -> True

size :: [a] -> Int
size = foldl' (\total _ -> total + 1) 0
