{-# LANGUAGE DerivingStrategies #-}

module Errors (Error (..)) where

data Error = CannotParseBooks deriving stock (Show, Eq)
