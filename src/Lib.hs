{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Lib (program, Options (Options)) where

import Control.Applicative (Alternative)
import Control.Monad (guard, when)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Text.Printf (printf)
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
    lagrangeWindow :: [Point Float],
    linearActive :: Bool,
    lagrangeActive :: Bool,
    step :: Float
  }

data Options = Options
  { linearFlag :: Bool,
    lagrangeFlag :: Bool,
    outputStep :: Float
  } deriving (Show)

linearInterpolationCoeff :: [Point Float] -> (Float, Float)
linearInterpolationCoeff (p1 : p2 : _) =
  (a, b)
  where
    a = (y p2 - y p1) / (x p2 - x p1)
    b = y p1 - a * x p1
linearInterpolationCoeff _ = error "linear coeff"

linearInterpolation :: [Point Float] -> [Float] -> [Point Float]
linearInterpolation train = map (\p -> Point {x = p, y = a * p + b})
  where
    (a, b) = linearInterpolationCoeff train

lagrangeInterpolationCoeff :: [Point Float] -> ([Float], [Float])
lagrangeInterpolationCoeff ps
  | length ps < 2 = error "Not enough points for Langrange"
  | otherwise =
      ( zipWith
          ( \x y ->
              y
                / foldr (\a nx -> if nx == x then a else a * (x - nx)) 1 xs
          )
          xs
          ys,
        xs
      )
  where
    xs = map x ps
    ys = map y ps

lagrangeInterpolation :: [Point Float] -> [Float] -> [Point Float]
lagrangeInterpolation train =
  map
    ( \x ->
        Point
          { x = x,
            y =
              sum $
                zipWith
                  ( \coeff cx ->
                      coeff
                        * foldr (\na ncx -> if ncx == cx then na else na * (x - ncx)) 1 xs
                  )
                  coeffs
                  xs
          }
    )
  where
    (coeffs, xs) = lagrangeInterpolationCoeff train

contextNormalizeWindows :: Context -> Context
contextNormalizeWindows ctx@Context {linearWindow = lin, lagrangeWindow = lagr}
  | length lin > 2 = contextNormalizeWindows ctx {linearWindow = tail lin}
  | length lagr > 4 = contextNormalizeWindows ctx {lagrangeWindow = tail lagr}
  | otherwise = ctx

contextIsLinearPossible :: Context -> Bool
contextIsLinearPossible Context {linearWindow = lin} = length lin == 2

contextIsLagrangePossible :: Context -> Bool
contextIsLagrangePossible Context {lagrangeWindow = lagr} = length lagr == 4

contextAddPoint :: Context -> Point Float -> Maybe Context
contextAddPoint ctx p
  | null (linearWindow ctx) = Just ctx {linearWindow = linearWindow ctx ++ [p], lagrangeWindow = lagrangeWindow ctx ++ [p]}
  | xlower (last $ linearWindow ctx) p && xlower (last $ lagrangeWindow ctx) p =
      Just $ ctx {linearWindow = linearWindow ctx ++ [p], lagrangeWindow = lagrangeWindow ctx ++ [p]}
  | otherwise = Nothing

linear :: [Point Float] -> [Float] -> IO ()
linear train xs = do
  putStrLn "Linear Interpolation: "
  let pts = linearInterpolation train xs
  foldl (\a p -> a >> printf "%10.2f %10.2f\n" (x p) (y p)) (pure ()) pts

lagrange :: [Point Float] -> [Float] -> IO ()
lagrange train xs = do
  putStrLn "Lagrange Interpolation: "
  let pts = lagrangeInterpolation train xs
  foldl (\a p -> a >> printf "%10.2f %10.2f\n" (x p) (y p)) (pure ()) pts

generateXs :: Float -> Float -> Float -> [Float]
generateXs start end step
  | start > end = []
  | otherwise = go start end step []
  where
    go curr end step xs
      | curr >= end = xs ++ [curr]
      | otherwise = go (curr + step) end step (xs ++ [curr])

contextFromOpts :: Options -> Context
contextFromOpts opts = trace (show opts)
  Context [] [] (linearFlag opts) (lagrangeFlag opts) (outputStep opts)

program :: Options -> IO ()
program opts =
  go $ contextFromOpts opts
  where
    go ctx =
      do
        point <- getPoint
        case point of
          Nothing -> do
            putStrLn "No points parsed - ending!"
            return ()
          Just p -> do
            let newctx = contextNormalizeWindows <$> contextAddPoint ctx p
            case newctx of
              Just newctx -> do
                let start = x $ head $ linearWindow newctx
                    end = x $ last $ linearWindow newctx
                when (contextIsLinearPossible newctx && linearActive newctx) $ linear (linearWindow newctx) (generateXs start end (step newctx))
                let start = x $ head $ lagrangeWindow newctx
                    end = x $ last $ lagrangeWindow newctx
                when (contextIsLagrangePossible newctx && lagrangeActive newctx) $ lagrange (lagrangeWindow newctx) (generateXs start end  (step newctx))
                go newctx
              Nothing -> do
                putStrLn "Failed to add point to window - ending!"
                return ()
