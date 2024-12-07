{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib where

import Control.Applicative (Alternative)
import Control.Monad (guard)
import Data.Maybe (fromJust)
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

data Context = Context
  { linearWindow :: [Point Float],
    lagrangeWindow :: [Point Float]
  }

linearCoeff :: (Fractional b) => [Point b] -> (b, b)
linearCoeff (p1 : p2 : _) =
  (a, b)
  where
    a = (y p2 - y p1) / (x p2 - x p1)
    b = y p1 - a * x p1
linearCoeff _ = error "linear coeff"

linear :: [Point Float] -> [Float] -> [Point Float]
linear train = map (\p -> Point {x = p, y = a * p + b})
  where
    (a, b) = linearCoeff train

program :: IO ()
program =
  do
    point1 <- fromJust <$> getPoint
    point2 <- fromJust <$> getPoint
    point3 <- fromJust <$> getPoint
    let p = linear [point1, point2] [x point3]
        in print p
    return ()

-- case point of
--   Just a -> do
--     print a
--     program
--   Nothing ->
--     putStrLn "No points parsed - ending!"
