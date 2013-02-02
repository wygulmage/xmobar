module Types where
import Graphics.X11.Xlib

data Widget = Text String | Icon String

data Bitmap = Bitmap { width  :: Dimension
                     , height :: Dimension
                     , pixmap :: Pixmap
                     }
