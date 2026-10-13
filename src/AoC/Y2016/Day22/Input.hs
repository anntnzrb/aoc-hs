-- | Storage nodes with nonnegative coordinates and consistent disk capacities.
module AoC.Y2016.Day22.Input (Node (Node, position, capacity, used, available), parseInput) where

import Data.Map.Strict qualified as Map
import Text.Megaparsec (Parsec, eof, errorBundlePretty, runParser, sepEndBy1, takeWhileP)
import Text.Megaparsec.Char (char, eol, hspace, hspace1, string)
import Text.Megaparsec.Char.Lexer qualified as Lexer

data Node = Node
    { position :: (Int, Int)
    , capacity :: Int
    , used :: Int
    , available :: Int
    }

type Parser = Parsec Void Text

parseInput :: Text -> Either Text [Node]
parseInput text = case runParser parser "storage nodes" text of
    Left err -> Left (toText (errorBundlePretty err))
    Right nodes
        | Map.size (Map.fromList [(node.position, ()) | node <- nodes]) == foldl' (\count _ -> count + 1) 0 nodes -> Right nodes
        | otherwise -> Left "Duplicate node coordinates"

parser :: Parser [Node]
parser = do
    _ <- optional (string "root@" *> takeWhileP Nothing (/= '\n') <* eol)
    _ <- optional (string "Filesystem" *> takeWhileP Nothing (/= '\n') <* eol)
    nodeParser `sepEndBy1` eol <* eof

nodeParser :: Parser Node
nodeParser = do
    _ <- string "/dev/grid/node-x"
    position <- (,) <$> Lexer.decimal <* string "-y" <*> Lexer.decimal
    capacity <- hspace1 *> terabytes
    used <- hspace1 *> terabytes
    available <- hspace1 *> terabytes
    percentage <- hspace1 *> (Lexer.decimal :: Parser Int) <* char '%' <* hspace
    if capacity > 0 && used <= capacity && available == capacity - used && percentage <= 100
        then pure (Node position capacity used available)
        else fail "Inconsistent node capacity"

terabytes :: Parser Int
terabytes = Lexer.decimal <* char 'T'
