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
import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day17.Input (Input (Input), parseInput)
import Crypto.Hash.MD5 qualified as MD5
import Data.ByteString qualified as BS

data Route = Route {row :: Int, column :: Int, path :: [Word8], depth :: Int, context :: MD5.Ctx}

solver :: Solver
solver text = do
    Input passcode <- parseInput text
    let arrivals =
            [Route 0 0 [] 0 (MD5.update MD5.init (encodeUtf8 passcode))]
                & iterate (concatMap nextRoutes)
                & takeWhile (not . null)
                & concatMap (filter atVault)
        shortest = arrivals & find (const True) & maybe "0" routeText
        longest = arrivals & foldl' (\best route -> max best route.depth) 0
    pure (Answers shortest (Just (show longest)))

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
        [ Route nextRow nextColumn (fromIntegral (ord direction) : route.path) (route.depth + 1) (MD5.update route.context (one (fromIntegral (ord direction))))
        | (door, (direction, dr, dc)) <- zip doors moves
        , door >= 11
        , let nextRow = route.row + dr
        , let nextColumn = route.column + dc
        , nextRow >= 0 && nextRow < 4
        , nextColumn >= 0 && nextColumn < 4
        ]
  where
    doors = MD5.finalize route.context & BS.take 2 & hexNibbles
    moves :: [(Char, Int, Int)]
    moves = [('U', -1, 0), ('D', 1, 0), ('L', 0, -1), ('R', 0, 1)]
