{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SA (
    -- * Problems
    Problem (..),
    Probability,
    isingAcceptance,
    -- * Config
    Config (..),
    -- * Search
    simulatedAnnealing,
    -- * Helpers
    D (..),
) where

import Data.Proxy   (Proxy (..))
import Data.Word    (Word64)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Numeric      (showFFloat)

import qualified System.Random.SplitMix as SM

-------------------------------------------------------------------------------
-- Problem
-------------------------------------------------------------------------------

-- | Probabilities should be [0,1] distributed 'Double's.
type Probability = Double

data Problem metric beta solution = Problem
    { initial :: SM.SMGen -> solution
      -- ^ used to generate initial solution

    , neighbor :: SM.SMGen -> solution -> solution
      -- ^ randomly select a neighbor to the current solution

    , acceptance :: solution -> solution -> metric -> metric -> beta -> Probability
      -- ^ an acceptance probability of new solution at given temperature

    , schedule :: [beta]
      -- ^ temperature schedule

    , fitness :: solution -> metric
      -- ^ fitness of solution.
      --   we only use it to report progress
    }

-- |
--
-- \[
-- A(\mu, \nu) = \begin{cases}
-- e^{-\beta (H_\nu - H_\mu)}, &\text{if } H_\nu - H_\mu > 0 \\
-- 1 & \text{otherwise}
-- \end{cases}
-- \]
--
isingAcceptance
    :: D metric -- ^ H(u)
    -> D metric -- ^ H(v)
    -> D beta   -- ^ inverse temperature, \( \beta \).
    -> Probability
isingAcceptance (D hu) (D hv) (D beta)
    | di > 0    = exp (negate (beta * di))
    | otherwise = 1
  where
    di = hv - hu

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

-- | Config, except we have no...
data Config = Config

-------------------------------------------------------------------------------
-- D
-------------------------------------------------------------------------------

-- | Double which 'Show' is prettier.
newtype D (n :: Nat) = D Double
  deriving (Eq, Ord)

instance KnownNat n => Show (D n) where
    showsPrec _ (D d) =
        showFFloat (Just (fromIntegral (natVal (Proxy :: Proxy n)))) d

-------------------------------------------------------------------------------
-- Main loop
-------------------------------------------------------------------------------

simulatedAnnealing
    :: (Ord metric, Show metric, Show solution, Show beta)
    => Config
    -> Word64                               -- ^ seed
    -> Problem metric beta solution  -- ^ problem
    -> IO solution
simulatedAnnealing _cfg seed Problem {..} = do
    let g0 = SM.mkSMGen seed

    let (g1, g1') = SM.splitSMGen g0
    let s0 = initial g1'
    let f0 = fitness s0

    putStrLn $ "Initial solution, fitness = " ++ show f0
    print s0

    (S _ _ _ _ sN fN) <- foldingM (S 1 g1 s0 f0 s0 f0) schedule $ \(S step g2 su fu sN fN) beta -> do
        let (g3, g3') = SM.splitSMGen g2
        let sv        = neighbor g3' su

        let fv = fitness sv

        let prob      = acceptance su sv fu fv beta
        let (x, g4)   = SM.nextDouble g3

        let (next, fnext) | x < prob  = (sv, fv)
                          | otherwise = (su, fu)

        let (sN', fN') | fN < fnext = (sN, fN)
                       | otherwise  = (next, fnext)

        putStrLn $ concat
            [ "Step "
            , show step
            , "; beta = ", show beta
            , "; H(u) = ", show fu
            , "; H(v) = ", show fv
            , "; A(u,v) = ", showFFloat (Just 5) prob ""
            , "; ", if x < prob then "accept" else "reject"
            , " -- " ++ show next
            ]

        return $ S (step + 1) g4 next fnext sN' fN'

    putStrLn $ "Final solution, fitness = " ++ show fN

    return sN

-- | State of simulation
data S metric solution = S !Int !SM.SMGen !metric !solution !metric !solution

foldingM :: s -> [a] -> (s -> a -> IO s) -> IO s
foldingM z xs0 f = go z xs0 where
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs
