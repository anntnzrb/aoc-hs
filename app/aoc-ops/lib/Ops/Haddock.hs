-- | Offline validation of Haddock's library coverage and diagnostics.
module Ops.Haddock (checkHaddock) where

import Data.Text qualified as T

{- | Require exactly one complete coverage row for each expected module.
Fail closed on absent, duplicate, unexpected or malformed rows and on warnings,
including unresolved links to external packages. Cabal's missing package-index
notice is infrastructure output, not a Haddock documentation diagnostic.
-}
checkHaddock :: [Text] -> Text -> Either Text [Text]
checkHaddock expected transcript = do
    rows <- traverse coverage candidates
    let modules = map fst rows
        incomplete = [line | (_, (complete, line)) <- rows, not complete]
        missing = ["missing coverage: " <> name | name <- expected, name `notElem` modules]
        unexpected = ["unexpected coverage: " <> name | name <- modules, name `notElem` expected]
        duplicates = ["duplicate coverage: " <> name | name <- ordNub modules, count name modules /= 1]
        problems = incomplete <> missing <> unexpected <> duplicates <> warnings
    unless (null problems) (Left (unlines problems))
    pure (map (snd . snd) rows)
  where
    candidates = lines transcript & filter (T.isInfixOf "%")
    warnings = lines transcript & filter documentationWarning
    count :: Text -> [Text] -> Int
    count name = foldl' (\total value -> total + fromEnum (value == name)) (0 :: Int)

coverage :: Text -> Either Text (Text, (Bool, Text))
coverage line = case words line of
    [percentage, "(", documented, "/", total, "in", name] -> do
        percent <- natural (T.dropEnd 1 percentage)
        present <- natural documented
        items <- natural (T.dropEnd 1 total)
        unless (T.isSuffixOf "%" percentage && T.isSuffixOf ")" total && T.isPrefixOf "'" name && T.isSuffixOf "'" name && items > 0 && present <= items && percent <= 100) invalid
        pure (T.dropEnd 1 (T.drop 1 name), (percent == 100 && present == items, T.strip line))
    _malformed -> invalid
  where
    invalid :: Either Text a
    invalid = Left ("malformed Haddock coverage: " <> line)
    natural :: Text -> Either Text Integer
    natural text = case readMaybe (toString text) :: Maybe Integer of
        Just value
            | value >= 0 -> Right value
            | otherwise -> invalid
        Nothing -> invalid

documentationWarning :: Text -> Bool
documentationWarning line =
    T.isInfixOf "warning:" (T.toCaseFold line)
        && not (T.isPrefixOf "Warning: The package list for 'hackage.haskell.org'" line)
