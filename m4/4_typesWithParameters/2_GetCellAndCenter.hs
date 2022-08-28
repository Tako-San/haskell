module GetCellAndCenter where
import           GHC.Float                      ( int2Double )
import           GHC.Float.RealFracMethods      ( floorDoubleInt )

data Coord a = Coord a a
  deriving (Show, Read)

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x y) = Coord (calc x) (calc y)
  where calc z = w * (int2Double z + 0.5)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x y) = Coord (calc x) (calc y)
  where calc z = floorDoubleInt $ z / w
