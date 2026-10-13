{- |
approach: Queue chip deliveries; record matching comparisons and multiply three output chips.
cost: O((n + d) log m) time, O(n + m) memory; three input passes, d deliveries, m destinations.
trade: Persistent maps keep the simulation pure and retain only one waiting chip per bot.
-}
module AoC.Y2016.Day10.Primary (solver) where

import AoC.Solution (Answers (Answers), Solver)
import AoC.Y2016.Day10.Input (Bot, Chip (Chip), Destination (Output, ToBot), Input (Input), Instruction (Give, Route), parseInput)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (Empty, (:<|)), (|>))
import Data.Sequence qualified as Seq

-- Missing target comparisons use 0, so examples with other chip values remain valid.
data Factory = Factory
    { waiting :: Map Bot Chip
    , deliveries :: Seq (Destination, Chip)
    , comparing :: Maybe Bot
    , outputs :: Map Natural Chip
    }

solver :: Solver
solver text = do
    Input instructions <- parseInput text
    factory <- simulate (routes instructions) (initial instructions)
    pure (Answers (maybe "0" show factory.comparing) (Just (outputProduct factory)))

routes :: [Instruction] -> Map Bot (Destination, Destination)
routes instructions = Map.fromList [(bot, (low, high)) | Route bot low high <- instructions]

initial :: [Instruction] -> Factory
initial instructions = Factory Map.empty (Seq.fromList [(ToBot bot, chip) | Give chip bot <- instructions]) Nothing Map.empty

simulate :: Map Bot (Destination, Destination) -> Factory -> Either Text Factory
simulate rules factory = case factory.deliveries of
    Empty -> Right factory
    (destination, chip) :<| rest -> do
        next <- deliver rules (destination, chip) (factory{deliveries = rest})
        simulate rules next

deliver :: Map Bot (Destination, Destination) -> (Destination, Chip) -> Factory -> Either Text Factory
deliver _ (Output bin, chip) factory = Right factory{outputs = Map.insertWith (\_ old -> old) bin chip factory.outputs}
deliver rules (ToBot bot, chip) factory = case Map.lookup bot factory.waiting of
    Nothing -> Right factory{waiting = Map.insert bot chip factory.waiting}
    Just held -> do
        (lowDestination, highDestination) <- Map.lookup bot rules & maybeToRight "Bot has two chips but no routing rule"
        let low = min chip held
            high = max chip held
        pure
            factory
                { waiting = Map.delete bot factory.waiting
                , deliveries = factory.deliveries |> (lowDestination, low) |> (highDestination, high)
                , comparing = if low == Chip 17 && high == Chip 61 then Just bot else factory.comparing
                }

-- Missing output bins use the undefined-part placeholder; retain the first chip per bin.
outputProduct :: Factory -> Text
outputProduct factory = maybe "0" (show . product) (traverse chipValue [0, 1, 2])
  where
    chipValue :: Natural -> Maybe Natural
    chipValue bin = Map.lookup bin factory.outputs <&> (\(Chip value) -> value)
