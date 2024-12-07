{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Text.Read (readMaybe)

data (Num a) => Point a = Point
  { x :: a,
    y :: a
  }
  deriving (Show)

xlower :: (Ord a, Num a) => Point a -> Point a -> Bool
xlower a b = x a < x b

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

arrayToPoint :: (Num a) => [a] -> Point a
arrayToPoint [x, y] = Point x y
arrayToPoint _ = error "The list didn't have 2 elements"

parseInput :: (Read a, Num a) => String -> Maybe (Point a)
parseInput input = arrayToPoint <$> (ensure (\x -> length x == 2) =<< traverse readMaybe (words input))

getPoint :: IO (Maybe (Point Float))
getPoint = do
  parseInput <$> getLine

program :: IO ()
program =
  do
    point <- getPoint
    case point of
      Just a -> do
        print a
        program
      Nothing ->
        putStrLn "No points parsed - ending!"
