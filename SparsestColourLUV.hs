import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Foldable
import Data.Maybe
import Data.Semigroup
import Data.Monoid hiding (First, Last)
import Data.Ord

import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as Graph

import qualified Data.Colour.CIE as Colour
import Data.Colour.CIE.Illuminant (d65)
import qualified Data.Colour.SRGB as Colour

import System.Random

data Vector3 a = Vector3 a a a deriving (Eq, Ord, Show, Read)

instance Functor Vector3 where
	fmap f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

instance Applicative Vector3 where
	pure x = Vector3 x x x
	Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

infixl 5 |+|, |-|
infixr 6 *|
infixl 6 |*, |/
infix 6 |.|
infixr 7 |*|

toVector3 :: (a,a,a) -> Vector3 a
toVector3 (x,y,z) = Vector3 x y z

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

type Colour = Colour.Colour Double
type CIEColour = Vector3 Double

clampColour :: Colour -> Colour
clampColour c = let
	Colour.RGB r g b = clamp <$> Colour.toSRGB c
	in Colour.sRGB r g b
  where
	clamp x = if x < 0
		then 0
		else if x > 1
			then 1
			else x

type ColourGraph = Graph.Gr Colour Double

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

toCIE :: Colour -> CIEColour
toCIE = cieLUVView d65

changeColour :: Colour -> Colour -> ColourGraph -> ColourGraph
changeColour oldc newc cs = Graph.gmap (\(_,n,x,xs) -> (,,,) [] n
	(if oldc == x then newc else x)
	(map (\b@(dist,n') -> let
		Just x' = Graph.lab cs n'
		in if oldc == x || oldc == x'
			then (normV (toCIE x |-| toCIE x'), n')
			else b
		) xs
	)
	) cs

step :: Double
step = 0.2

maxIter :: Int
maxIter = 10000

record :: Double
record = 70.536429053471

threads :: Int
threads = 4

outputPath :: FilePath
outputPath = "./Sparsest16Colours.txt"

stepGraph :: ColourGraph -> (Double, ColourGraph)
stepGraph cs = let
	(c1n,c2n,dist) = minimumBy (comparing (\(_,_,d) -> d)) $ Graph.labEdges cs
	Just c1 = Graph.lab cs c1n
	Just c2 = Graph.lab cs c2n
	v1 = toCIE c1
	v2 = toCIE c2
	Vector3 l1 a1 b1 = v1 |+| step *| unitV (v1 |-| v2)
	c1' = clampColour (cieLUV d65 l1 a1 b1)
	Vector3 l2 a2 b2 = v2 |+| step *| unitV (v2 |-| v1)
	c2' = clampColour (cieLUV d65 l2 a2 b2)
	in (
		dist,
		changeColour c2 c2' . changeColour c1 c1' $ cs
	)

take2 :: [a] -> [(a,a)]
take2 []     = []
take2 (x:xs) = map ((,) x) xs ++ take2 xs

randomColour :: IO Colour
randomColour = let
	gen = randomRIO (0,1)
	in liftA3 Colour.sRGB gen gen gen

initialGraph :: IO ColourGraph
initialGraph = do
	let vertexColours = liftA3 Colour.sRGB [0,1] [0,1] [0,1]
	internalColours <- sequenceA (replicate 8 randomColour)
	let colours' = zip [0..] (internalColours ++ vertexColours)
	let dists = map (\((n1,c1),(n2,c2)) -> (n1,n2, normV (toCIE c1 |-| toCIE c2))) (take2 colours')
	return $ Graph.mkGraph colours' dists
{-
iterGraph :: ColourGraph -> (Double, ColourGraph)
iterGraph cs = let
	(dist,cs') = stepGraph cs
	in if record < dist
		then (dist,cs)
		else iterGraph cs'
-}
output :: String -> IO ()
output str = do
	appendFile outputPath str
	appendFile outputPath "\n"
	putStrLn str

findGraph :: MVar (Double, ColourGraph) -> IO ()
findGraph result = do
	initG <- initialGraph
	let (g,(dist,_)) = appEndo (stimes maxIter (Endo $ \x@(g,(dist,g')) ->
		if record < dist
			then x
			else (g', stepGraph g')
		)) ((id &&& stepGraph) initG)
	if record < dist
		then putMVar result (dist, g)
		else findGraph result

main :: IO ()
main = do
	result <- newEmptyMVar
	sequenceA_ (replicate threads (forkIO (findGraph result)))
	(dist, g) <- readMVar result
	writeFile outputPath ""
	for_ (Graph.labNodes g) (output . Colour.sRGB24show . snd)
	output $ "Minimal distance: " ++ show dist