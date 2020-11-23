{-# LANGUAGE TupleSections #-}

import Control.Applicative ( Applicative(liftA2), liftA3 )
import Control.Arrow ( Arrow(second) )
import Control.Concurrent
    ( forkIO,
      getNumCapabilities,
      newEmptyMVar,
      putMVar,
      readMVar,
      MVar )
import Control.Monad ( replicateM, replicateM_ )
import Data.Foldable ( for_, maximumBy )
import Data.List ( sort )
import Data.Ord ( comparing )
import Data.Word ( Word8 )
import System.IO ( hClose, openFile, hGetLine, IOMode(ReadMode) )
import qualified Data.Colour.CIE as Colour
import Data.Colour.CIE.Illuminant (d65)
import qualified Data.Colour.SRGB as Colour
import System.Random ( randomRIO )

data Vector3 a = Vector3 a a a deriving (Eq, Ord, Show, Read)

instance Functor Vector3 where
    fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Applicative Vector3 where
    pure x = Vector3 x x x
    Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

instance Foldable Vector3 where
    foldMap f (Vector3 x y z) = f x <> f y <> f z

instance Traversable Vector3 where
    traverse f (Vector3 x y z) = liftA3 Vector3 (f x) (f y) (f z)

infixl 5 |+|, |-|
infixr 6 *|
infixl 6 |*, |/
infix 6 |.|
infixr 7 |*|

toVector3 :: (a,a,a) -> Vector3 a
toVector3 (x,y,z) = Vector3 x y z

zeroV :: Num a => Vector3 a
zeroV = Vector3 0 0 0

(|+|) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
(|+|) = liftA2 (+)

negateV :: Num a => Vector3 a -> Vector3 a
negateV = fmap negate

(|-|) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
(|-|) = liftA2 (-)

(*|) :: Num a => a -> Vector3 a -> Vector3 a
(*|) = fmap . (*)

(|*) :: Num a => Vector3 a -> a -> Vector3 a
(|*) = flip (*|)

(|/) :: Fractional a => Vector3 a -> a -> Vector3 a
v |/ k = fmap (/k) v

(|.|) :: Num a => Vector3 a -> Vector3 a -> a
Vector3 r1 g1 b1 |.| Vector3 r2 g2 b2 = r1*r2 + g1*g2 + b1*b2

(|*|) :: Num a => Vector3 a -> Vector3 a -> Vector3 a
Vector3 r1 g1 b1 |*| Vector3 r2 g2 b2 = Vector3 (g1*b2 - g2*b1) (b1*r2 - b2*r1) (r1*g2 - r2*g1)

normSqV :: Num a => Vector3 a -> a
normSqV v = v |.| v

normV :: Floating a => Vector3 a -> a
normV = sqrt . normSqV

unitV :: Floating a => Vector3 a -> Vector3 a
unitV v = v |/ normV v

compV :: Floating a => Vector3 a -> Vector3 a -> a -- project w onto v
compV v w = (v |.| w) / normV v

projV :: Fractional a => Vector3 a -> Vector3 a -> Vector3 a -- project w onto v
projV v w = (v |.| w) / normSqV v *| v

type RGBColour = Vector3 Word8
type CIEColour = Vector3 Double

adjacentColours :: RGBColour -> [RGBColour]
adjacentColours = traverse adjacentI where
    adjacentI 0x00 = [0x00 .. 0x01]
    adjacentI 0xFF = [0xFE .. 0xFF]
    adjacentI n = [n-1 .. n+1]

cieLUV :: (Ord a, Floating a) => Colour.Chromaticity a -- ^White point
    -> a              -- ^L* coordinate (lightness)
    -> a              -- ^U* coordinate
    -> a              -- ^V* coordinate
    -> Colour.Colour a
cieLUV white_ch l u v = Colour.cieXYZ x y z
  where
    white = Colour.chromaColour white_ch 1.0
    (xn,yn,zn) = Colour.cieXYZView white
    divisorWhite = xn + 15*yn + 3*zn
    u'n = 4*xn / divisorWhite
    u' = u'n + if 0 == l then 0 else u / (13*l)
    v'n = 9*yn / divisorWhite
    v' = v'n + if 0 == l then 0 else v / (13*l)
    y = yn * if l <= 8
        then (3/29)^3 * l
        else ((l + 16)/116)^3
    x = y * (9/4) * (u'/v')
    z = y * ((12 - 3*u') / (4*v') - 5)

cieLUVView :: (Ord a, Floating a) => Colour.Chromaticity a -- ^White point
    -> Colour.Colour a -> Vector3 a
cieLUVView white_ch c = Vector3 l u v
  where
    white = Colour.chromaColour white_ch 1.0
    (x,y,z) = Colour.cieXYZView c
    (xn,yn,zn) = Colour.cieXYZView white
    y' = y / yn
    l = if (6/29)^3 < y'
        then 116 * y' ** (1/3) - 16
        else y' / (6/29)^3
    divisor      = x + 15*y + 3*z
    divisorWhite = xn + 15*yn + 3*zn
    u'n = 4*xn / divisorWhite
    v'n = 9*yn / divisorWhite
    u' = if 0 == divisor then u'n else 4*x / divisor
    v' = if 0 == divisor then v'n else 9*y / divisor
    u = 13 * l * (u' - u'n)
    v = 13 * l * (v' - v'n)

rgbtoCIE :: RGBColour -> CIEColour
rgbtoCIE (Vector3 r g b) = cieLUVView d65 (Colour.sRGB24 r g b)

cietoRGB :: CIEColour -> RGBColour
cietoRGB (Vector3 l u v) = let
    Colour.RGB r g b = Colour.toSRGB24 (cieLUV d65 l u v)
    in Vector3 r g b

take1 :: [a] -> [(a,[a])]
take1 []     = []
take1 (x:xs) = (x,xs) : map (second (x:)) (take1 xs)

take2 :: [a] -> [(a,a)]
take2 []     = []
take2 (x:xs) = map (x,) xs ++ take2 xs

distGraph :: [RGBColour] -> [Double]
distGraph cs = take (coloursN - 1) . sort $ f <$> take2 cs where
    f (c,o) = normSqV (rgbtoCIE c |-| rgbtoCIE o)
{-
avgGraph :: [RGBColour] -> CIEColour
avgGraph cs = foldr (|+|) zeroV (rgbtoCIE <$> cs) |/ fromIntegral (length cs)

varGraph :: [RGBColour] -> Double
varGraph cs = let
    avgc = avgGraph cs
    in getSum (foldMap (Sum . normSqV . (|-|) avgc) (rgbtoCIE <$> cs)) / fromIntegral (length cs) 
-}
stepGraph :: [RGBColour] -> IO [RGBColour]
stepGraph cs = do
    index <- randomRIO (0, length cs - 1)
    let (o,os) = take1 cs !! index
    pure $ maximumBy (comparing distGraph) $ do
        o' <- adjacentColours o
        pure $ o' : os

coloursN :: Int
coloursN = 16

outputPath :: FilePath
outputPath = "./Sparsest" ++ show coloursN ++ "Colours.txt"

initialGraph :: IO ([RGBColour], [Double])
initialGraph = do
    h <- openFile outputPath ReadMode
    cs <- replicateM coloursN (hGetLine h)
    distSqs <- replicateM (coloursN - 1) (hGetLine h)
    hClose h
    pure ((\(Colour.RGB r g b) -> Vector3 r g b) . Colour.toSRGB24 . Colour.sRGB24read <$> cs, read <$> distSqs)

maxIter :: Int
maxIter = 256 * coloursN

iterGraph :: Int -> [Double] -> [RGBColour] -> IO [RGBColour]
iterGraph iter records cs
    | records < distGraph cs =  return cs
    | iter < maxIter = stepGraph cs >>= iterGraph (iter+1) records
    | otherwise = return []

findGraph :: MVar [RGBColour] -> IO ()
findGraph result = do
    (initGraph, distSqs) <- initialGraph
    resultGraph <- iterGraph 0 distSqs initGraph
    case resultGraph of
        [] -> findGraph result
        _  -> putMVar result resultGraph

main :: IO ()
main = do
    result <- newEmptyMVar
    threadN <- getNumCapabilities
    replicateM_ threadN (forkIO (findGraph result))
    cs <- readMVar result
    writeFile outputPath ""
    for_ cs (\c -> putStrLn (showColour c) >> appendFile outputPath (showColour c ++ "\n"))
    let distSqs = distGraph cs
    mapM_ print distSqs
    appendFile outputPath (concatMap ((++"\n") . show) distSqs)
  where
    showColour :: Vector3 Word8 -> String
    showColour (Vector3 r g b) = Colour.sRGB24show (Colour.sRGB24 r g b)
