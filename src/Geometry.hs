{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Geometry (module Geometry) where

import Settings
import Graphics.Gloss

type Radius = Float 
type Position = (Float, Float)

brickPositionX :: Int -> Float
brickPositionX col = -fromIntegral screenWidth / 2 + brickMarginLeft + (brickWidth + brickMarginBetween) * (fromIntegral col) + (brickWidth / 2)

brickPositionY :: Int -> Float
brickPositionY row = -80 + fromIntegral screenHeight / 2 - brickMarginTop - (brickHeight + brickMarginBetween) * (fromIntegral row) - (brickHeight / 2)

class Distance a where
  distX   :: a -> a -> Float
  distY   :: a -> a -> Float

instance Distance Position where
  distX pos1 pos2  = abs ((fst pos1) - (fst pos2))
  distY pos1 pos2  = abs ((snd pos1) - (snd pos2))