{- |
approach: Breadth-first search over paths and incremental MD5 contexts, using
the first four digest nibbles to open doors and grid bounds to reject walls.
Vault arrivals are terminal: take the first path and greatest length.
cost: O(N + P + D) time and O(W * D + P) memory for N explored paths,
maximum depth D, frontier width W and passcode length P; one input pass.
trade: Each path shares its reversed prefix and extends a pure hash context,
avoiding path copying and rehashing. Lazy levels share both answers without a
visited set: revisiting a room with a different path changes its doors.
Exhaustive search assumes the path tree is finite; no route prints 0 for both.
-}
module AoC.Y2016.Day17.Primary (solver) where

import AoC.MD5 (hexNibbles)
import AoC.Solution (Solver, answers)
import AoC.Y2016.Day17.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS

data Route = Route {row :: Int, column :: Int, path :: [Word8], depth :: Int, context :: MD5.Ctx}

data Direction = Upward | Downward | Leftward | Rightward

directionLetter :: Direction -> Char
directionLetter = \case
    Upward -> 'U'
    Downward -> 'D'
    Leftward -> 'L'
    Rightward -> 'R'

directionOffset :: Direction -> (Int, Int)
directionOffset = \case
    Upward -> (-1, 0)
    Downward -> (1, 0)
    Leftward -> (0, -1)
    Rightward -> (0, 1)

{- | Return the shortest vault path and longest successful path length; use 0 for both
when no path reaches the vault; exhaustive search requires a finite path tree. Input
grammar errors are returned as text.
-}
solver :: Solver
solver text = do
    Input passcode <- parseInput text
    let arrivals =
            [Route 0 0 [] 0 (MD5.update MD5.init (encodeUtf8 passcode))]
                & iterate (concatMap nextRoutes)
                & takeWhile (not . null)
                & concatMap (filter atVault)
        shortest = arrivals & listToMaybe & maybe "0" routeText
        longest = arrivals & foldl' (\best route -> max best route.depth) 0
    answers shortest (Just (show longest))

atVault :: Route -> Bool
atVault route = route.row == 3 && route.column == 3

routeText :: Route -> Text
routeText route =
    route.path
        & BS.pack
        & BS.reverse
        & decodeUtf8

nextRoutes :: Route -> [Route]
nextRoutes route
    | atVault route = []
    | otherwise =
        [ Route nextRow nextColumn (letter : route.path) (route.depth + 1) (MD5.update route.context (one letter))
        | direction <- openDirections route.context
        , let (dr, dc) = directionOffset direction
        , let letter = fromIntegral (ord (directionLetter direction))
        , let nextRow = route.row + dr
        , let nextColumn = route.column + dc
        , nextRow >= 0 && nextRow < 4
        , nextColumn >= 0 && nextColumn < 4
        ]

openDirections :: MD5.Ctx -> [Direction]
openDirections context = [direction | (door, direction) <- zip doors [Upward, Downward, Leftward, Rightward], door >= 11]
  where
    -- Digest nibbles describe doors in up, down, left, right order.
    doors =
        MD5.finalize context
            & BS.take 2
            & hexNibbles
