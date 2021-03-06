module Main where

import           Prelude

import           Control.Foldl          (Fold)
import qualified Control.Foldl
import           Data.Aeson             (Value, (.=))
import qualified Data.Aeson             as Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy   as LBS
import           Data.Proxy             (Proxy (..))
import qualified Data.String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GitHub                 (Auth, Name, Release, ReleaseAsset)
import qualified GitHub
import           GitHub.AssetFold.Main  (Config (..), Platform)
import qualified GitHub.AssetFold.Main  as AssetFold
import qualified System.Environment

parseDhallAsset :: Release -> ReleaseAsset -> a -> (Name Release, Name Platform, Name ReleaseAsset)
parseDhallAsset r asset _ =
  let (p, a) = parseExeName $ GitHub.releaseAssetName asset
  in (GitHub.mkName Proxy $ GitHub.releaseName r, p, a)

parseExeName :: Text -> (Name Platform, Name ReleaseAsset)
parseExeName name = case Text.splitOn "-" name of
  [ "dhall", _, arch, os ]         -> (mkPlatform arch os, mkAsset "dhall")
  [ "dhall", rest, _, arch, os ]   -> (mkPlatform arch os, mkAsset ("dhall-" <> rest))
  [ "dhall", r1, r2, _, arch, os ] -> (mkPlatform arch os, mkAsset ("dhall-" <> r1 <> "-" <> r2))
  _ -> (mkPlatform "unknown" "unknown", mkAsset name)
  where
    mkPlatform arch os = GitHub.mkName Proxy (arch <> "-" <> Text.takeWhile (not . (==) '.') os)
    mkAsset = GitHub.mkName Proxy

-- N.B. the Binary instance for Value comes from binary-instances, which we transitively depend
-- on via the github package.
dhallAssetFold :: Fold ByteString (Release -> ReleaseAsset -> Value)
dhallAssetFold = (fmap . fmap) (uncurry assetInfo) <$> AssetFold.assetUrlAndSha256
  where
    assetInfo t bs = Aeson.object
      [ "sha256" .= Base16.encodeBase16 bs
      , "url"    .= t
      ]

config :: String -> Config Auth
config token = Config
  { configAuth = GitHub.OAuth $ Data.String.fromString token
  , configMaxRequests = 4
  , configOwner = "dhall-lang"
  , configRepo = "dhall-haskell"
  , configFetchCount = GitHub.FetchAll
  }

parseArgs :: [String] -> (FilePath, FilePath)
parseArgs [db, json] = (db, json)
parseArgs _ = error "Usage: dhall-release-hashes DATABASE FILEPATH"

main :: IO ()
main = do
  token <- System.Environment.getEnv "GITHUB_TOKEN"
  (db, json) <- parseArgs <$> System.Environment.getArgs
  AssetFold.releaseMapMain
    (config token)
    (Control.Foldl.generalize dhallAssetFold)
    (fmap (either (\t -> error ("Decoding error " <> show t)) id) AssetFold.binaryCodecStrict)
    parseDhallAsset
    db
    (LBS.writeFile json . Aeson.encode)
