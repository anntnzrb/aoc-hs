{- |
approach: Breadth-first search over complete paths, using the first four MD5
hex digits to open doors and grid bounds to reject walls. Stop at the vault.
cost: O(N * (P + D)) time and O(W * D + P) memory for N explored paths,
maximum depth D, frontier width W and passcode length P; one input pass.
trade: Lazy breadth-first levels yield the shortest path without a visited
set: revisiting a room with a different path changes its doors. A passcode
with no route prints 0; part 2 remains an unfinished 0 placeholder.
-}
module AoC.Y2016.Day17.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day17.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BS

data Route = Route Int Int ByteString

solver :: Solver
solver text = do
    Input passcode <- parseInput text
    let shortest =
            [Route 0 0 ""]
                & iterate (concatMap (nextRoutes (encodeUtf8 passcode)))
                & takeWhile (not . null)
                & concat
                & find atVault
                & maybe "0" routeText
    pure (Answers shortest (Just "0"))

atVault :: Route -> Bool
atVault (Route row column _) = row == 3 && column == 3

routeText :: Route -> Text
routeText (Route _ _ path) = decodeUtf8 path

nextRoutes :: ByteString -> Route -> [Route]
nextRoutes passcode route@(Route row column path)
    | atVault route = []
    | otherwise =
        [ Route nextRow nextColumn (BS.snoc path direction)
        | (door, (direction, dr, dc)) <- zip doors moves
        , door >= 'b' && door <= 'f'
        , let nextRow = row + dr
        , let nextColumn = column + dc
        , nextRow >= 0 && nextRow < 4
        , nextColumn >= 0 && nextColumn < 4
        ]
  where
    doors = MD5.hash (passcode <> path) & Base16.encode & BS.take 4 & BS.unpack
    moves :: [(Char, Int, Int)]
    moves = [('U', -1, 0), ('D', 1, 0), ('L', 0, -1), ('R', 0, 1)]
