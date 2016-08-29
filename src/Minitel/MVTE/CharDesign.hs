{-|
Module      : CharDesign
Description : Character design types
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Minitel.MVTE.CharDesign (CharDesign, Font, toFont) where

import Data.Vector (Vector, fromList)

type CharDesign = Vector [Int]
type Font = Vector CharDesign

toFont :: [[[Int]]] -> Font
toFont = fromList . fmap fromList
