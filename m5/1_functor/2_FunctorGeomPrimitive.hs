module FunctorGeomPrimitive where

data Point3D a = Point3D a a a deriving Show

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

instance Functor GeomPrimitive where
  fmap f (Point pt) = Point $ fmap f pt
  fmap f (LineSegment pt1 pt2) = LineSegment (fmap f pt1) (fmap f pt2)
