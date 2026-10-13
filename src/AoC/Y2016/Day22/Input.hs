-- | Storage nodes with nonnegative coordinates and consistent disk capacities.
module AoC.Y2016.Day22.Input (Node (Node, position, capacity, used, available), parseInput) where

import AoC.Parse (Parser, boundedDecimal, parseText)
import Data.Map.Strict qualified as Map
import Text.Megaparsec (sepEndBy1, takeWhileP)
import Text.Megaparsec.Char (char, eol, hspace, hspace1, string)

{- | A storage node with positive capacity and used plus available equal to capacity.
Parsing enforces these invariants; direct construction must preserve them.
-}
data Node
    = -- | Coordinates and total, occupied and free storage.
      Node
      { position :: (Int, Int)
    -- ^ Nonnegative (x, y) grid coordinates.
      , capacity :: Int
    -- ^ Positive total storage in terabytes.
      , used :: Int
    -- ^ Occupied storage in terabytes, at most capacity.
      , available :: Int
    -- ^ Free storage in terabytes, equal to capacity minus used.
      }

{- | Accept node listings with optional root/Filesystem headers; reject duplicate
coordinates, out-of-range Int values and inconsistent capacities. Errors are returned
as text.
-}
parseInput :: Text -> Either Text [Node]
parseInput text = do
    nodes <- parseText parser text
    unless (Map.size (Map.fromList [(node.position, ()) | node <- nodes]) == foldl' (\count _ -> count + 1) 0 nodes) (Left "Duplicate node coordinates")
    pure nodes

parser :: Parser [Node]
parser = do
    _ <- optional (string "root@" *> takeWhileP Nothing (/= '\n') <* eol)
    _ <- optional (string "Filesystem" *> takeWhileP Nothing (/= '\n') <* eol)
    nodeParser `sepEndBy1` eol

nodeParser :: Parser Node
nodeParser = do
    _ <- string "/dev/grid/node-x"
    position <- (,) <$> boundedDecimal <* string "-y" <*> boundedDecimal
    capacity <- hspace1 *> terabytes
    used <- hspace1 *> terabytes
    available <- hspace1 *> terabytes
    percentage <- hspace1 *> (boundedDecimal :: Parser Int) <* char '%' <* hspace
    -- Bounded nonnegative values and used <= capacity make subtraction safe.
    unless (capacity > 0 && used <= capacity && available == capacity - used && percentage <= 100) (fail "Inconsistent node capacity")
    pure (Node position capacity used available)

terabytes :: Parser Int
terabytes = boundedDecimal <* char 'T'
