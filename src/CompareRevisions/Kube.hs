{-# LANGUAGE DuplicateRecordFields #-}
module CompareRevisions.Kube
  ( -- * Objects in Kubernetes
    KubeObject(..)
  , namespacedName
    -- * Environments
  , Env
  , loadEnvFromDisk
  , loadEnvFromCluster
  , ProcessError(..)
    -- * Images
  , ImageName
  , ImageLabel
    -- * Image diffs
  , ImageDiff(..)
  , getDifferingImages
  , getImageName
  ) where

import CompareRevisions.Kube.Diff
import CompareRevisions.Kube.Load
import CompareRevisions.Kube.Types
