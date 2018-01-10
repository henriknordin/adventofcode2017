module Advent20
  ( parseInput
  , answer1
  , answer2
  ) where

import           Data.List       (elemIndex, elemIndices, groupBy, notElem,
                                  sortBy)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)

data Coordinate = Coordinate
  { x :: !Int
  , y :: !Int
  , z :: !Int
  } deriving (Show, Eq)

data Particle = Particle
  { pId :: !Int
  , pos :: !Coordinate
  , vel :: !Coordinate
  , acc :: !Coordinate
  } deriving (Show, Eq)

parseInput :: String -> [Particle]
parseInput = parse . lines

test1 :: [String]
test1 = ["p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>", "p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"]

test2 :: [String]
test2 =
  [ "p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>"
  , "p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"
  , "p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>"
  , "p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>"
  ]

answer1 :: [Particle] -> Int
answer1 xs =
  let byAccel = sortBy accel xs
      lowestAcc = manhattan $ acc $ head byAccel
      lowest = takeWhile (\(Particle _ _ _ a) -> manhattan a == lowestAcc) byAccel
  in pId $ head lowest

answer2 :: [Particle] -> Int
answer2 = length . go 0 . sortBy (flip accel)
  where
    go :: Int -> [Particle] -> [Particle]
    go 100 ps = ps
    go n ps =
      let pcs = sortBy coordinateOrdering $ map (\(Particle i p _ _) -> p) ps
          colliding =
            foldl
              (\cs p ->
                 if length (elemIndices (pos p) pcs) > 1
                   then p : cs
                   else cs)
              []
              ps
          noncol = filter (`notElem` colliding) ps
      in case noncol of
           [] -> []
           [x] -> [x]
                   -- the below is not right, but lets iterate 100 times
           (x:y:xs) ->
             if posEuc x >= posEuc y && velEuc x >= velEuc y && accEuc x >= accEuc y && escaping x
               then x : go (n + 1) (map step (y : xs))
               else go (n + 1) (map step (x : y : xs))

dot :: Particle -> Int
dot (Particle _ _ (Coordinate vx vy vz) (Coordinate ax ay az)) = vx * ax + vy * ay + vz * az

escaping :: Particle -> Bool
escaping p = dot p > 0

accel :: Particle -> Particle -> Ordering
accel (Particle _ _ _ a1) (Particle _ _ _ a2) = compare (manhattan a1) (manhattan a2)

posOrdering :: Particle -> Particle -> Ordering
posOrdering (Particle _ p1 _ _) (Particle _ p2 _ _) = compare (manhattan p1) (manhattan p2)

coordinateOrdering :: Coordinate -> Coordinate -> Ordering
coordinateOrdering (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) =
  let x' = compare x1 x2
      y' = compare y1 y2
      z' = compare z1 z2
  in if x' /= EQ
       then x'
       else if y' /= EQ
              then y'
              else z'

collide :: Particle -> Particle -> Bool
collide (Particle _ p1 _ _) (Particle _ p2 _ _) = p1 == p2

manhattan :: Coordinate -> Int
manhattan (Coordinate x' y' z') = abs x' + abs y' + abs z'

posEuc :: Particle -> Double
posEuc (Particle _ (Coordinate x' y' z') _ _) =
  sqrt . fromIntegral $ x' ^ 2 + abs y' ^ 2 + abs z' ^ 2

velEuc :: Particle -> Double
velEuc (Particle _ _ (Coordinate x' y' z') _) =
  sqrt . fromIntegral $ x' ^ 2 + abs y' ^ 2 + abs z' ^ 2

accEuc :: Particle -> Double
accEuc (Particle _ _ _ (Coordinate x' y' z')) =
  sqrt . fromIntegral $ x' ^ 2 + abs y' ^ 2 + abs z' ^ 2

step :: Particle -> Particle
step (Particle i p v a) = Particle i (add (add p v) a) (add v a) a
  where
    add :: Coordinate -> Coordinate -> Coordinate
    add (Coordinate x1 y1 z1) (Coordinate x2 y2 z2) = Coordinate (x1 + x2) (y1 + y2) (z1 + z2)

parse :: [String] -> [Particle]
parse = zipWith (curry parseLine) [0,1 ..]

parseLine :: (Int, String) -> Particle
parseLine (i, xs) =
  let [p, v, a] = splitOn ", " xs
  in Particle i (values p) (values v) (values a)
  where
    values :: String -> Coordinate
    values xs =
      let xs' = drop (1 + fromJust (elemIndex '<' xs)) xs
          [x', y', z'] = splitOn "," $ take (fromJust $ elemIndex '>' xs') xs'
      in Coordinate (read x' :: Int) (read y' :: Int) (read z' :: Int)

