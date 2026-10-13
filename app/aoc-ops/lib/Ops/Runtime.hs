-- | Runtime effect interpreter. Authentication never leaves the HTTP boundary.
module Ops.Runtime (Runtime, runRuntime, readText, writeText, exists, isDirectory, makeDirectory, directories, http, output, formatFiles) where

import Data.Text qualified as T
import Effectful (Eff, IOE, runEff, (:>))
import Network.HTTP.Client (Request (checkResponse, redirectCount, requestHeaders), httpLbs, newManager, parseRequest, responseBody, responseStatus, urlEncodedBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory)
import System.FilePath (takeDirectory)
import System.Process (callProcess)

{- | External operations explicitly require effectful's IOE effect.
These boundary functions are the only interpreters that lift bare IO.
-}
type Runtime = IOE

runRuntime :: Eff '[IOE] a -> IO a
runRuntime = runEff

readText :: (Runtime :> es) => FilePath -> Eff es Text
readText path = liftIO (readFileBS path <&> decodeUtf8)

writeText :: (Runtime :> es) => FilePath -> Text -> Eff es ()
writeText path text = liftIO $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFileBS path (encodeUtf8 text)

exists :: (Runtime :> es) => FilePath -> Eff es Bool
exists path = liftIO (doesPathExist path)

isDirectory :: (Runtime :> es) => FilePath -> Eff es Bool
isDirectory path = liftIO (doesDirectoryExist path)

makeDirectory :: (Runtime :> es) => FilePath -> Eff es ()
makeDirectory path = liftIO (createDirectoryIfMissing True path)

directories :: (Runtime :> es) => FilePath -> Eff es [FilePath]
directories path = liftIO (listDirectory path)

http :: (Runtime :> es) => Text -> Maybe [(ByteString, ByteString)] -> Eff es (Int, Text)
http url body = liftIO (authenticatedRequest url body)

output :: (Runtime :> es) => Text -> Eff es ()
output text = liftIO (putTextLn text)

formatFiles :: (Runtime :> es) => [FilePath] -> Eff es ()
formatFiles paths = liftIO (callProcess "fourmolu" (["--mode", "inplace"] <> paths))

authenticatedRequest :: Text -> Maybe [(ByteString, ByteString)] -> IO (Int, Text)
authenticatedRequest url body = do
    env <- readFileBS ".env" <&> decodeUtf8
    token <- env & lines & mapMaybe (T.stripPrefix "AOC_SESSION=") & viaNonEmpty head & maybe (fail "missing AOC_SESSION") pure
    when (T.null token || T.any (`elem` ['\r', '\n', ' ']) token) (fail "invalid session configuration")
    request <- parseRequest (toString url)
    let authenticated = request{requestHeaders = [("Cookie", "session=" <> encodeUtf8 token), ("User-Agent", "github.com/anntnzrb/aoc-hs by anntnzrb")], redirectCount = 0, checkResponse = \_ _ -> pass}
    manager <- newManager tlsManagerSettings
    response <- httpLbs (maybe authenticated (`urlEncodedBody` authenticated) body) manager
    pure (statusCode (responseStatus response), decodeUtf8 (responseBody response))
