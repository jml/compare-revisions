{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Generic library for validating a thing that can have multiple errors.
--
-- Copied from jml/graphql-api with permission.
module CompareRevisions.Validator
  ( Validator(..)
  , throwE
  , throwErrors
  , mapErrors
  ) where

import Protolude

import Data.List.NonEmpty (NonEmpty(..))

-- | A 'Validator' is a value that can either be valid or have a non-empty
-- list of errors.
newtype Validator e a = Validator { runValidator :: Either (NonEmpty e) a } deriving (Eq, Show, Functor, Monad)

-- | Throw a single validation error.
throwE :: e -> Validator e a
throwE e = throwErrors (e :| [])

-- | Throw multiple validation errors. There must be at least one.
throwErrors :: NonEmpty e -> Validator e a
throwErrors = Validator . Left

-- | Map over each individual error on a validation. Useful for composing
-- validations -- i.e. taking a 'Validator' with one error type and turning it
-- into a 'Validator' with another error type.
--
-- This is /somewhat/ like 'first', but 'Validator' is not, and cannot be, a
-- 'Bifunctor', because the left-hand side is specialized to @NonEmpty e@,
-- rather than plain @e@. Also, whatever function were passed to 'first' would
-- get the whole non-empty list, whereas 'mapErrors' works on one element at a
-- time.
--
-- >>> mapErrors (+1) (pure "hello")
-- Validator {runValidator = Right "hello"}
-- >>> mapErrors (+1) (throwE 2)
-- Validator {runValidator = Left (3 :| [])}
-- >>> mapErrors (+1) (throwErrors (NonEmpty.fromList [3, 5]))
-- Validator {runValidator = Left (4 :| [6])}
mapErrors :: (e1 -> e2) -> Validator e1 a -> Validator e2 a
mapErrors f (Validator (Left es)) = Validator (Left (map f es))
mapErrors _ (Validator (Right x)) = Validator (Right x)

-- | The applicative on Validator allows multiple potentially-valid values to
-- be composed, and ensures that *all* validation errors bubble up.
instance Applicative (Validator e) where
  pure x = Validator (Right x)
  Validator (Left e1) <*> (Validator (Left e2)) = Validator (Left (e1 <> e2))
  Validator (Left e) <*> _ = Validator (Left e)
  Validator _ <*> (Validator (Left e)) = Validator (Left e)
  Validator (Right f) <*> Validator (Right x) = Validator (Right (f x))
