module AoC.Y2016.Day11 (tests) where

import AoC.Solution (render)
import AoC.Y2016.Day11.Primary (solver)
import Data.Set qualified as Set
import Hedgehog (Property, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

-- ops:property-imports

type Arrangement = (Int, [(Int, Int)])

tests :: TestTree
tests =
    testGroup
        "2016/11"
        [ testProperty "safe inventories and element renaming" inventories
        , testCase "oversized safe stranded inventory" overflow
        -- ops:property-entries
        ]

inventories :: Property
inventories =
    ( do
        pairs <-
            forAll
                ( Gen.choice
                    [ one <$> ((,) <$> Gen.element [0 .. 3] <*> Gen.element [0 .. 3])
                    , replicateM 2 ((,0) <$> Gen.element [1 .. 3])
                    ]
                )
        let actual = fmap render (solver (inventory ["hydrogen", "lithium"] pairs))
        actual === reference pairs
        fmap render (solver (inventory ["zinc", "argon"] pairs)) === actual
    )
        & property
        & withTests 12

-- Sixteen pairs overflow even a 64-bit key; no cargo on floor one means
-- the structural fallback terminates immediately, without an enormous BFS.
overflow :: IO ()
overflow =
    fmap
        render
        ( solver
            ( inventory
                (map (\letter -> "element" <> one letter) ['a' .. 'p'])
                (replicate 16 (3, 2))
            )
        )
        @?= reference (replicate 16 (3, 2))

inventory :: [Text] -> [(Int, Int)] -> Text
inventory names pairs =
    unlines
        [ "The " <> name <> " floor contains " <> contents floorNumber <> "."
        | (floorNumber, name) <- zip [0 ..] ["first", "second", "third", "fourth"]
        ]
  where
    contents floorNumber = case [ "a " <> element <> suffix
                                | (element, (generator, chip)) <- zip names pairs
                                , (floorIndex, suffix) <- [(generator, " generator"), (chip, "-compatible microchip")]
                                , floorIndex == floorNumber
                                ] of
        [] -> "nothing relevant"
        items -> intersperse " and " items & mconcat

reference :: [(Int, Int)] -> Either Text Text
reference pairs = do
    steps <- search pairs & maybeToRight "No safe elevator route reaches the fourth floor"
    pure (unlines [show steps, show (fromMaybe 0 (search ((0, 0) : (0, 0) : pairs)))])

search :: [(Int, Int)] -> Maybe Int
search pairs
    | not (safe pairs) = Nothing
    | otherwise = layers Set.empty 0 [(0, pairs)]
  where
    layers :: Set Arrangement -> Int -> [Arrangement] -> Maybe Int
    layers seen depth frontier
        | any (all (== (3, 3)) . snd) frontier = Just depth
        | null fresh = Nothing
        | otherwise = layers (Set.union seen (Set.fromList fresh)) (depth + 1) (concatMap moves fresh)
      where
        fresh = Set.toList (Set.difference (Set.fromList frontier) seen)

safe :: [(Int, Int)] -> Bool
safe pairs = all (\(generator, chip) -> generator == chip || notElem chip (map fst pairs)) pairs

moves :: Arrangement -> [Arrangement]
moves (floorNumber, pairs) =
    [ (destination, moved)
    | destination <- [floorNumber - 1, floorNumber + 1]
    , destination >= 0
    , destination < 4
    , cargo <- cargoes
    , let moved = relocate destination cargo
    , safe moved
    ]
  where
    items =
        [ (index, component)
        | (index, (generator, chip)) <- zip [0 :: Int ..] pairs
        , (component, location) <- [(True, generator), (False, chip)]
        , location == floorNumber
        ]
    cargoes = map one items <> [[a, b] | (index, a) <- zip [0 :: Int ..] items, b <- drop (index + 1) items]
    relocate destination cargo = [(move index True generator, move index False chip) | (index, (generator, chip)) <- zip [0 ..] pairs]
      where
        move index component original = if (index, component) `elem` cargo then destination else original
