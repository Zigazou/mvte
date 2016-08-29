{-|
Module      : MVTE
Description : Minitel VideoTex Emulator
Copyright   : (c) Frédéric BISSON, 2016
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Minitel.MVTE.MVTE
( MVTE( MVTE, mvteBuffer, mvteFonts, mvtePosition, mvteCursorMode
      , mvteScreenMode, mvteCharMode
      )
) where

import qualified Data.Vector as V

import Minitel.MVTE.CharDesign (Font)
import Minitel.MVTE.G0 (g0)

data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           deriving (Eq, Show)

data CharSize = NormalSize
              | DoubleWidth
              | DoubleHeight
              | DoubleSize
              deriving (Eq, Show)

data ReverseVideo = Positive
                  | Negative
                  deriving (Eq, Show)

data Flash = NoFlash
           | Flash
           deriving (Eq, Show)

data Conceal = Reveal
             | Conceal
             deriving (Eq, Show)

data Underline = NoUnderline
               | Underline
               deriving (Eq, Show)

data Joint = Joint
           | Disjoint
           deriving (Eq, Show)

data Insert = Insert
            | Overwrite
            deriving (Eq, Show)

data CharPart = TopLeft
              | TopRight
              | BottomLeft
              | BottomRight
              deriving (Eq, Show)

data Accent = Tilde
            | Acute
            | Grave
            | Dot
            | Trema
            | Cedilla
            deriving (Eq, Show)

data CharType = Mosaics
              | Strokes
              | AlphaNumerics Underline
              | Accents Accent Underline
              | UserDefined Underline
              deriving (Eq, Show)

data Cell = Cell
    { cChar :: Int
    , cForeground :: Color
    , cBackground :: Color
    , cInsert :: Insert
    , cSize :: CharSize
    , cFlash :: Flash
    , cReverseVideo :: ReverseVideo
    , cType :: CharType
    , cPart :: CharPart
    }
    deriving (Eq, Show)

type Row = V.Vector Cell
type Buffer = V.Vector Row

data CursorMode = ShowCursor
                | HideCursor
                deriving (Eq, Show)

data ScreenMode = RollMode
                | PageMode
                deriving (Eq, Show)

data CharMode = CharNormal
              | Mosaic
              deriving (Eq, Show)

data MVTE = MVTE
    { mvteBuffer :: Buffer
    , mvteFonts :: (Font, Font, Font, Font)
    , mvtePosition :: (Int, Int)
    , mvteCursorMode :: CursorMode
    , mvteScreenMode :: ScreenMode
    , mvteCharMode :: CharMode
    , mvteForeground :: Color
    , mvteBackground :: Color
    }

initCell :: Cell
initCell = Cell
    { cChar = 32
    , cForeground = White
    , cBackground = Black
    , cInsert = Overwrite
    , cSize = NormalSize
    , cFlash = NoFlash
    , cReverseVideo = Positive
    , cType = AlphaNumerics NoUnderline
    , cPart = TopLeft
    }

initScreen :: MVTE
initScreen = MVTE
    { mvteBuffer = V.replicate 25 . V.replicate 40 $ initCell
    , mvteFonts = (g0, g0, g0, g0)
    , mvtePosition = (0, 1)
    , mvteCursorMode = ShowCursor
    , mvteScreenMode = RollMode
    , mvteCharMode = CharNormal
    , mvteLastCell = initCell
    }
