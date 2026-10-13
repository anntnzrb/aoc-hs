-- | Pure protocol and source-policy checks. No personal data is retained.
module Ops.Logic (validAnswer, Verdict (RightAnswer, WrongAnswer, Cooldown, AlreadyComplete, UnknownResponse), classify, articleText, statement, docViolations, refuseExisting) where

import Data.Char (isAlphaNum, isAscii)
import Data.List (lookup)
import Data.Text qualified as T
import Language.Haskell.Lexer (PosToken, Token (Conid, NestedComment, Qconid, Reservedop), lexerPass0, rmSpace, simpPos)
import System.FilePath (splitDirectories, takeFileName)
import Text.HTML.TagSoup (Tag (TagClose, TagOpen, TagText), isTagOpenName, parseTags, sections)

validAnswer :: Text -> Bool
validAnswer answer = not (T.null answer) && T.all allowed answer
  where
    allowed c = (isAscii c && isAlphaNum c) || c == ',' || c == '-'

data Verdict = RightAnswer | WrongAnswer | Cooldown | AlreadyComplete | UnknownResponse
    deriving stock (Eq, Show)

classify :: Text -> Verdict
classify text
    | "That's the right answer" `T.isInfixOf` text = RightAnswer
    | "That's not the right answer" `T.isInfixOf` text = WrongAnswer
    | "You gave an answer too recently" `T.isInfixOf` text = Cooldown
    | "You don't seem to be solving the right level" `T.isInfixOf` text = AlreadyComplete
    | otherwise = UnknownResponse

articleText :: Text -> Text
articleText =
    parseTags
        >>> articles (const True)
        >>> foldMap readable
        >>> T.strip

statement :: Text -> Either Text Text
statement html = case articles dayDesc (parseTags html) of
    [] -> Left "response contains no puzzle articles"
    found -> Right (T.strip (foldMap (readable . removeAnswers) found) <> "\n")
  where
    dayDesc :: [(Text, Text)] -> Bool
    dayDesc attrs = lookup "class" attrs & maybe False (elem "day-desc" . words)

articles :: ([(Text, Text)] -> Bool) -> [Tag Text] -> [[Tag Text]]
articles predicate = sections starts >>> map (takeWhile (/= TagClose "article"))
  where
    starts (TagOpen "article" attrs) = predicate attrs
    starts _ = False

removeAnswers :: [Tag Text] -> [Tag Text]
removeAnswers = unfoldr paragraph >>> concatMap keep
  where
    paragraph :: [Tag Text] -> Maybe ([Tag Text], [Tag Text])
    paragraph [] = Nothing
    paragraph tags@(TagOpen "p" _ : _) =
        Just (body <> take 1 closing, drop 1 closing)
      where
        (body, closing) = break (== TagClose "p") tags
    paragraph tags = Just (break (isTagOpenName "p") tags)
    keep tags@(TagOpen "p" _ : _)
        | "Your puzzle answer was" `T.isPrefixOf` T.strip (readable tags) = []
    keep tags = tags

readable :: [Tag Text] -> Text
readable = foldMap piece
  where
    piece :: Tag Text -> Text
    piece (TagText text) = text
    piece (TagOpen "li" _) = "\n- "
    piece (TagOpen "br" _) = "\n"
    piece (TagClose name)
        | name `elem` ["p", "pre", "h2", "li", "article"] = "\n"
    piece _ = ""

-- | Paths are repository-relative. Tags must be in the opening Haddock block.
docViolations :: FilePath -> Text -> [Text]
docViolations path source = map ((toText path <> ": ") <>) (headers <> strings <> annotations)
  where
    parts = splitDirectories path
    solution = "src/AoC/" `isPrefixOf` path && any ("Y" `isPrefixOf`) parts && any ("Day" `isPrefixOf`) parts && takeFileName path /= "Input.hs"
    header =
        source
            & T.stripStart
            & T.breakOn "-}"
            & fst
            & lines
            & map T.strip
    haddock = any (`T.isPrefixOf` T.stripStart source) ["{- |", "{-|"]
    headers = ["missing header " <> tag | solution, tag <- ["approach:", "cost:", "trade:"], not haddock || not (any (T.isPrefixOf tag) header)]
    tokens = lexerPass0 (toString source)
    strings = ["String in type signature" | "src/AoC/" `isPrefixOf` path, stringSignature (rmSpace tokens)]
    annotations = ["forbidden annotation" | any annotation tokens]
    annotation :: PosToken -> Bool
    annotation (kind, (_, text))
        | kind == NestedComment = T.stripPrefix "{-#" (toText text) & maybe False forbidden
        | otherwise = False
    forbidden :: Text -> Bool
    forbidden = words >>> viaNonEmpty head >>> maybe False (`elem` ["HLINT", "ANN"])

stringSignature :: [PosToken] -> Bool
stringSignature = foldl' scan ((0, 1), Nothing, False) >>> (\(_, _, found) -> found)
  where
    scan :: ((Int, Int), Maybe (Int, Int), Bool) -> PosToken -> ((Int, Int), Maybe (Int, Int), Bool)
    scan ((previousLine, previousColumn), active, found) token@(kind, (pos, text)) =
        ((line, columnBase), next, found || (inside && stringType token))
      where
        (line, column) = simpPos pos
        columnBase = bool column (min column previousColumn) (line == previousLine)
        inside = maybe False (\(startLine, startColumn) -> line == startLine || column > startColumn) active
        next
            | kind == Reservedop && text == "::" = Just (line, min column previousColumn)
            | not inside || text `elem` ["=", ";"] = Nothing
            | otherwise = active
    stringType :: PosToken -> Bool
    stringType (kind, (_, text)) = kind `elem` [Conid, Qconid] && (text == "String" || ".String" `T.isSuffixOf` toText text)

refuseExisting :: [FilePath] -> [FilePath] -> Either Text ()
refuseExisting existing requested = case filter (\path -> T.toCaseFold (toText path) `elem` map (T.toCaseFold . toText) existing) requested of
    [] -> Right ()
    paths -> Left ("refusing to overwrite: " <> T.intercalate ", " (map toText paths))
