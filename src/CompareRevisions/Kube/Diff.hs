{-# LANGUAGE DeriveGeneric #-}
module CompareRevisions.Kube.Diff
  ( getDifferingImages
  , ImageDiff(..)
  , getImageName
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map as Map
import Data.Text (splitOn)

import CompareRevisions.Kube.Types (Env, KubeObject, Image(..), ImageName, ImageLabel)

data ImageDiff
  = ImageAdded ImageName (Maybe ImageLabel)
  | ImageChanged ImageName (Maybe ImageLabel) (Maybe ImageLabel)
  | ImageRemoved ImageName (Maybe ImageLabel)
  deriving (Eq, Ord, Show, Generic)

instance Aeson.ToJSON ImageDiff

getImageName :: ImageDiff -> ImageName
getImageName (ImageAdded name _) = name
getImageName (ImageChanged name _ _) = name
getImageName (ImageRemoved name _) = name


-- | Given two sets of Kubernetes objects, return the images that differ
-- between them.
getDifferingImages :: Env -> Env -> Map KubeObject [ImageDiff]
getDifferingImages sourceEnv targetEnv =
  Map.mapMaybe getImageDiffs (diffMap sourceImages targetImages)
  where
    sourceImages, targetImages :: Map KubeObject (Map ImageName (Maybe ImageLabel))
    sourceImages = map getImagesFromObject sourceEnv
    targetImages = map getImagesFromObject targetEnv

    -- If a Kubernetes object was added, we don't really care about the images
    -- within.
    getImageDiffs (Added _) = Nothing
    -- Likewise, we don't care if an object was removed.
    getImageDiffs (Removed _) = Nothing
    -- If an object was changed, we want to get the changes.
    getImageDiffs (Changed src tgt) = Just (compareImageSets src tgt)


-- | Get the images from a Kubernetes object definition.
--
-- Because a single definition can have multiple images, we return a map of
-- image name to image label, where the label is optional.
getImagesFromObject :: Value -> Map ImageName (Maybe ImageLabel)
getImagesFromObject value =
  Map.fromList [ (name, label) | Image name label <- images ]
  where
    images = mapMaybe parseImageName (getImageNames value)

    -- | Get all the names of images within a JSON value.
    getImageNames :: Value -> [Text]
    getImageNames (Object obj) =
      image <> rest
      where
        image =
          case HashMap.lookup "image" obj of
            Just (String name) -> [name]
            _ -> []
        rest = concatMap getImageNames obj
    getImageNames (Array arr) = concatMap getImageNames arr
    getImageNames _ = []

    -- | Parse an image name.
    parseImageName :: Text -> Maybe Image
    parseImageName imageName =
      case splitOn ":" imageName of
        [name] -> Just (Image name Nothing)
        [name, label] -> Just (Image name (Just label))
        _ -> Nothing


-- | Given two sets of image, a @source@ and a @target@ set, that map names of
-- images to labels, return a list of all image differences.
compareImageSets :: Map ImageName (Maybe ImageLabel) -> Map ImageName (Maybe ImageLabel) -> [ImageDiff]
compareImageSets source target =
  foreach (Map.toList (diffMap source target)) $
  \(name, diff) ->
    case diff of
      Added x -> ImageAdded name x
      Changed x y -> ImageChanged name x y
      Removed x -> ImageRemoved name x



-- | A difference in a set of generic values.
data Diff value
  = Added value
  | Changed value value
  | Removed value
  deriving (Eq, Ord, Show)

-- | Compare two maps. The returned map has the union of the keys of both maps.
diffMap :: (Ord key, Eq value) => Map key value -> Map key value -> Map key (Diff value)
diffMap source target = Map.fromList (go (Map.toAscList source) (Map.toAscList target))
  where
    go xs [] = map (second Added) xs
    go [] ys = map (second Removed) ys
    go xs@((xKey, xValue):xs') ys@((yKey, yValue):ys') =
      case compare xKey yKey of
        LT -> (xKey, Added xValue):go xs' ys
        EQ
          | xValue == yValue -> go xs' ys'
          | otherwise -> (xKey, Changed xValue yValue):go xs' ys'
        GT -> (yKey, Removed yValue):go xs ys'
