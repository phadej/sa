{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module SA.Example.SplitMix16 where

import Control.Monad          (when)
import Control.Monad.ST       (ST, runST)
import Data.Bits
import Data.Foldable          (for_)
import Data.Word              (Word16, Word32, Word64)
import Distribution.Utils.MD5 (md5)
import GHC.Fingerprint        (Fingerprint (..))
import Numeric                (showHex)

import qualified Data.ByteString             as BS
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified System.Random.SplitMix      as SM

import SA

type Metric = D 6
type Beta   = D 5

data Params = Params !Word16 !Word16

instance Show Params where
    showsPrec d (Params p0 p1) = showParen (d > 10)
        $ showString "Params "
        . showHex16 p0
        . showChar ' '
        . showHex16 p1
      where
        showHex16 w = let s = showHex w ""
                      in showString $ "0x" ++ replicate (4 - length s) '0' ++ s

splitmixProblem
    :: Int -- ^ steps
    -> Problem Metric Beta Params
splitmixProblem steps = Problem {..} where
    fitness (Params p0 p1) = avalanche (paramHash p0 p1)
    acceptance _ _ = isingAcceptance

    initial g =
        let (w, _) = SM.nextWord32 g
        in Params (fromIntegral $ w `shiftR` 16) (fromIntegral w)

    change0, change1 :: Double
    change0 = 0.001
    change1 = 0.000001

    schedule =
        [ D (log 2 / delta)
        | step <- [0 .. steps]
        , let delta = fromIntegral step / fromIntegral steps * (change1 - change0) + change0
        ]

    neighbor g (Params p0 p1) =
        -- there are 32 possible bits we can flip
        let i = fromIntegral $ fst $ SM.bitmaskWithRejection64 32 g
        in if i < 16
           then Params (complementBit p0 i) p1
           else Params p0 (complementBit p1 (i - 16))


-------------------------------------------------------------------------------
-- parameterised hash
-------------------------------------------------------------------------------

-- parameterised hash, with murmur structure
paramHash :: Word16 -> Word16 -> Word16 -> Word16
paramHash p0 p1 x0 =
    let x1 = x0 `xor` (x0 `shiftR` 8)
        x2 = x1 * p0
        x3 = x2 `xor` (x1 `shiftR` 5)
        x4 = x3 * p1
        x5 = x4 `xor` (x4 `shiftR` 8)
    in x5

-------------------------------------------------------------------------------
-- Example hashes
-------------------------------------------------------------------------------

-- truncated murmurhash32
exampleHash :: Word16 -> Word16
exampleHash x0 =
    let x1 = x0 `xor` (x0 `shiftR` 8)
        x2 = x1 * 0x85eb
        x3 = x2 `xor` (x1 `shiftR` 5)
        x4 = x3 * 0xc2b2
        x5 = x4 `xor` (x4 `shiftR` 8)
    in x5

broken :: Word16 -> Word16
broken w
    | testBit w 0 = complement w
    | otherwise   = w

-------------------------------------------------------------------------------
-- Avalance Effect
-------------------------------------------------------------------------------

-- | Calculate avalance for a given function.
avalanche :: (Word16 -> Word16) -> D 6
avalanche = fst . avalancheTwo

avalancheTwo :: (Word16 -> Word16) -> (D 6, D 6)
avalancheTwo f = do
    -- for_ [0..15] $ \i -> do
    --     for_ [0..15] $ \j -> do
    --         putStr $ showFFloat (Just 2) (matrix2 UV.! (i * 16 + j)) " "
    --     putStrLn ""
    (D matrix3, D matrix5)
  where
    matrix1 :: UV.Vector Word32
    matrix1 = runST $ do
        vec <- MUV.replicate (16 * 16) (0 :: Word32)
        for_ [minBound .. maxBound] $ \u ->
            avalancheStep u f vec
        UV.freeze vec

    -- matrix of [-1 .. 1] uniformly distributed numbers
    matrix2 :: UV.Vector Double
    matrix2 = UV.map (\n -> (fromIntegral n - halfsize) / halfsize) matrix1

    -- sum of squares
    matrix3 :: Double
    matrix3 = UV.sum (UV.map sq matrix2) / 256.0

    matrix4 :: UV.Vector Double
    matrix4 = UV.map (\n -> fromIntegral n / halfsize) matrix1

    matrix5 :: Double
    matrix5 = UV.sum (UV.map sq matrix4) / 256.0

    halfsize :: Double
    halfsize = 32768.0 -- (2 ^ 16) / 2

    sq x = x * x

avalancheStep
    :: Word16
    -> (Word16 -> Word16)
    -> MUV.MVector s Word32
    -> ST s ()
avalancheStep u f vec = do
    for_ [0 .. 15 :: Int] $ \i -> do
        -- with a single bit flipped
        let v = complementBit u i

        -- apply function
        let u' = f u
            v' = f v

        -- for each output bit
        for_ [0 .. 15 :: Int] $ \j -> do
            let ne = u' `xor` v'

            -- check when they are not the same
            when (testBit ne j) $ MUV.unsafeModify vec succ (i * 16 + j)

-------------------------------------------------------------------------------
-- MD5
-------------------------------------------------------------------------------

truncated16MD5 :: Word16 -> Word16
truncated16MD5 w = fromIntegral res
  where
    Fingerprint _ res = md5 (BS.pack [lo,hi])
    lo = fromIntegral w
    hi = fromIntegral (w `shiftR` 8)

-------------------------------------------------------------------------------
-- 32bit sanity check
-------------------------------------------------------------------------------

murmurmix32 :: Word32 -> Word32
murmurmix32 x0 =
    let x1 = x0 `xor` (x0 `shiftR` 16)
        x2 = x1 * 0x85ebca6b
        x3 = x2 `xor` (x1 `shiftR` 13)
        x4 = x3 * 0xc2b2ae35
        x5 = x4 `xor` (x4 `shiftR` 16)
    in x5

-- | Calculate avalance for a given function.
avalanche32 :: (Word32 -> Word32) -> D 6
avalanche32 f = do
    -- for_ [0..15] $ \i -> do
    --     for_ [0..15] $ \j -> do
    --         putStr $ showFFloat (Just 2) (matrix2 UV.! (i * 16 + j)) " "
    --     putStrLn ""
    D matrix3
  where
    matrix1 :: UV.Vector Word64
    matrix1 = runST $ do
        vec <- MUV.replicate (32 * 32) (0 :: Word64)
        for_ [0 .. size - 1] $ \u ->
            avalancheStep32 (fromIntegral u) f vec
        UV.freeze vec

    -- matrix of [-1 .. 1] uniformly distributed numbers
    matrix2 :: UV.Vector Double
    matrix2 = UV.map (\n -> (fromIntegral n - halfsize) / halfsize ) matrix1

    -- sum of squares
    matrix3 :: Double
    matrix3 = UV.sum (UV.map sq matrix2) / (32.0 * 32.0)

    -- here 10^7; whole range is:
    --
    -- >>> logBase 10 (2 ^ 32)
    -- 9.632959861247397
    --
    size :: Int
    size = 10000000

    halfsize :: Double
    halfsize = 0.5 * fromIntegral size

    sq x = x * x

avalancheStep32
    :: Word32
    -> (Word32 -> Word32)
    -> MUV.MVector s Word64
    -> ST s ()
avalancheStep32 u f vec = do
    for_ [0 .. 31 :: Int] $ \i -> do
        -- with a single bit flipped
        let v = complementBit u i

        -- apply function
        let u' = f u
            v' = f v

        -- for each output bit
        for_ [0 .. 31 :: Int] $ \j -> do
            let ne = u' `xor` v'

            -- check when they are not the same
            when (testBit ne j) $ MUV.unsafeModify vec succ (i * 32 + j)
