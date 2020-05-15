{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module SA.Example.Ramp (
    rampProblem,
) where

import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as MV
import qualified System.Random.SplitMix as SM
import qualified Data.Set               as Set

import SA

rampProblem :: Int -> Problem Int (D 5) (V.Vector Int)
rampProblem size = Problem {..} where
    initial g0 = V.fromList $ go g0 $ Set.fromList [0..size] where
        go g rest | Set.null rest = []
                  | otherwise     =
            let (w, g') = SM.bitmaskWithRejection64 (fromIntegral (Set.size rest)) g
                c       = Set.elemAt (fromIntegral w) rest
            in c : go g' (Set.delete c rest)

    fitness = negate . length . filter (uncurry (<)) . pairs . V.toList

    acceptance _ _ fv fu (D t) = case compare fv fu of
        GT -> 1
        EQ -> 0.5
        LT -> t

    steps = succ size * succ size * 1000
    schedule = 
        [ D $ (1 - fromIntegral i / fromIntegral steps) * 0.25
        | i <- [0 .. steps]
        ]

    neighbor :: SM.SMGen -> V.Vector Int -> V.Vector Int
    neighbor g xs = 
        let (i', g') = SM.bitmaskWithRejection64' (fromIntegral size) g
            (j', _ ) = SM.bitmaskWithRejection64' (fromIntegral size) g'

            i        = fromIntegral i'
            j        = fromIntegral j'

            swap mv = do
                x <- MV.unsafeRead mv i
                y <- MV.unsafeRead mv j
                MV.unsafeWrite mv i y
                MV.unsafeWrite mv j x 

        in V.modify swap xs

pairs :: [a] -> [(a,a)]
pairs []         = []
pairs xs@(_:xs') = zip xs xs'
