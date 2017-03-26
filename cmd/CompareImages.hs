module Main (main) where

import Protolude hiding ((<>))

import qualified Data.Map as Map
import Options.Applicative
import CompareRevisions.Kube
  ( KubeObject(..)
  , ImageDiff(..)
  , loadEnvFromDisk
  , getDifferingImages
  )


data Config = Config FilePath FilePath deriving (Eq, Show)

options :: Parser Config
options = Config
  <$> argument str (metavar "SOURCE" <> help "The environment with the newer images.")
  <*> argument str (metavar "DESTINATION" <> help "The environment with the older images.")

formatDiff :: Map KubeObject [ImageDiff] -> Text
formatDiff = Map.foldMapWithKey formatSingleDiff
  where
    formatSingleDiff kubeObj diffs =
      mconcat ([ formatKubeObject kubeObj, "\n" ] <>
               [ "  " <> formatImageDiff diff <> "\n" | diff <- diffs ]) <> "\n"
    formatKubeObject (KubeObject namespace kind name) = namespace <> "/" <> name <> " (" <> kind <> ")"

    formatImage name label = name <> ":" <> fromMaybe "default" label

    formatImageDiff (ImageAdded name label) = "+ " <> formatImage name label
    formatImageDiff (ImageChanged name srcLabel dstLabel) = "~ " <> formatImage name srcLabel <> " -> " <> formatImage name dstLabel
    formatImageDiff (ImageRemoved name label) = "- " <> formatImage name label


main :: IO ()
main = do
  Config srcDir dstDir <- execParser opts
  srcEnv <- loadEnvFromDisk srcDir
  dstEnv <- loadEnvFromDisk dstDir
  let diff = getDifferingImages srcEnv dstEnv
  putStr (formatDiff diff)
  where
    opts = info (helper <*> options)
           (fullDesc <>
            progDesc "Compare images between two Kubernetes environments" <>
            header "compare-images - what is different between two k8s envs")
