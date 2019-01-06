{-# LANGUAGE FlexibleContexts #-}

import Data.Array
import Control.Arrow
import Text.Regex.Posix

main = interact $ show . (countWater &&& countWater2) . solve . parse

parse input = accumArray (flip const) '.' ((yMin,xMin-1),(yMax,xMax+1)) $
              map (flip (,) '#') points
  where
    ((yMin,yMax),(xMin,xMax)) = (minMax *** minMax) (unzip points)
    points = concatMap toPoints $ lines input
    toPoints l = [ (y,x) | x <- xRange, y <- yRange ]
      where xRange = parseRange "x" l
            yRange = parseRange "y" l

    parseRange r l = [low..high] where
      [_,a,b] = getAllTextSubmatches (l =~ (r ++ "=([0-9]*)\\.?\\.?([0-9]*)"))
      low = read a
      high | null b = low
           | otherwise = read b
    minMax = minimum &&& maximum

solve m0 = drip (yMin,500) m0 where
  ((yMin,_),(yMax,_)) = bounds m0
  drip (y,x) m | m!(y,x) == '|'           = m -- cut
               | y >= yMax                = m'
               | m!(y+1,x) `notElem` ".|" = flood (y,x) m
               | otherwise                = drip (y+1,x) m'
    where m' = m // [((y,x),'|')]
  flood (y,x) m
    | Just ss <- leftSpill <> rightSpill = foldr drip (spill y bed m) ss
    | otherwise = settle (y,x) bed m
    where bed = x : leftRange ++ rightRange
          (leftRange,leftSpill) = seek pred
          (rightRange,rightSpill) = seek succ
          seek f = foldr adv undefined $ tail $ iterate f x
          adv x r | m!(y,x) == '#'        = ([],Nothing)
                  | m!(y+1,x) `elem` ".|" = ([],Just [(y,x)])
                  | otherwise             = first (x:) r
  spill y xs m = m // [ ((y,x),'|') | x <- xs ]
  settle (y,x) bed m = flood (y-1,x) $ m // [ ((y,x),'~') | x <- bed ]

countWater = length . filter (`elem` "~|") . elems
countWater2 = length . filter (== '~') . elems
