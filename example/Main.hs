{-# LANGUAGE DataKinds #-}
module Main (
    main,
    -- * Finnish Randonneur results
    route1, route2, route3,
    route4, route5, route6, route7,
    openRouteService, noNorth,
) where

import Data.List          (intercalate)
import Data.Maybe         (fromMaybe)
import Data.Traversable   (for)
import Data.Word          (Word64)
import Numeric            (showFFloat)
import System.Environment (getArgs)
import Text.Read          (readMaybe)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import SA
import SA.Example.FinnishRandonneur
import SA.Example.Ramp
import SA.Example.SplitMix16

main :: IO ()
main = do
    args <- getArgs
    case args of
        "ramp9"       : args' -> ramp9      (seed args')
        "ramp9stats"  : _     -> ramp9stats
        "ramp19stats" : _     -> ramp19stats
        "ramp19"      : args' -> ramp19     (seed args')
        "frando5"     : args' -> frando5    (seed args')
        "frando9"     : args' -> frando9    (seed args')
        "frando-all"  : args' -> frandoAll  (seed args')
        "splitmix"    : args' -> splitmix   (seed args')
        "avalanche"   : _     -> avalanceMurmurmix32

        _ -> ramp9 42
  where
    seed :: [String] -> Word64
    seed []    = 42
    seed (s:_) = fromMaybe 42 (readMaybe s)

-------------------------------------------------------------------------------
-- Ramp
-------------------------------------------------------------------------------

ramp9 :: Word64 -> IO ()
ramp9 seed = print =<< simulatedAnnealing
    Config
    seed
    (rampProblem 9)

ramp19 :: Word64 -> IO ()
ramp19 seed = print =<< simulatedAnnealing
    Config
    seed
    (rampProblem 19)

ramp9stats :: IO ()
ramp9stats = do
    let problem = rampProblem 9
    res <- for [1..1000] $ \seed -> do
        res <- simulatedAnnealing Config seed problem
        print (seed, res)
        return res
    let counts = Map.fromListWith (+) [ (fitness problem c, 1 :: Int) | c <- res]
    print counts

ramp19stats :: IO ()
ramp19stats = do
    let problem = rampProblem 19
    res <- for [1..1000] $ \seed -> do
        res <- simulatedAnnealing Config seed problem
        print (seed, res)
        return res
    let counts = Map.fromListWith (+) [ (fitness problem c, 1 :: Int) | c <- res]
    print counts



-------------------------------------------------------------------------------
-- splitmix
-------------------------------------------------------------------------------

-- Params 0x906b 0x3cb3; fitness = 0.000716
-- avalancheTwo of result      = (0.000716,1.012189)
--
-- Params 0xd255 0xed45; fitness = 0.000366
-- avalancheTwo of result      = (0.000366,1.008126)
--
-- avalancheTwo of exampleHash = (0.006548,0.991778)
splitmix :: Word64 -> IO ()
splitmix seed = do
    let problem = splitmixProblem 1000
    solution <- simulatedAnnealing Config seed problem
    putStrLn $ show solution ++ "; fitness = " ++ show (fitness problem solution)
    let Params p0 p1 = solution
    putStrLn $ "avalancheTwo of result      = " ++ show (avalancheTwo (paramHash p0 p1))
    putStrLn $ "avalancheTwo of exampleHash = " ++ show (avalancheTwo exampleHash)
    putStrLn $ "avalancheTwo of exampleHash = " ++ show (avalancheTwo id)

avalanceMurmurmix32 :: IO ()
avalanceMurmurmix32 = do
    putStrLn $ "avalanche of murmummix32 " ++ show (avalanche32 murmurmix32)
    putStrLn $ "avalanche of id @Word32  " ++ show (avalanche32 id)

-------------------------------------------------------------------------------
-- Finnish Travelling Salesman
-------------------------------------------------------------------------------

-- [Helsinki,Turku,Tampere,Oulu,Jyvaskyla]
-- 1238 vs 1431km
frando5 :: Word64 -> IO ()
frando5 seed = print =<< simulatedAnnealing
    (Config)
    seed
    (finnishRandonneurProblem 1000 $ Set.fromList [Helsinki, Tampere, Turku, Oulu, Jyvaskyla])

-- [Helsinki,Turku,Pori,Tampere,Jyvaskyla,Kuopio,Joensuu,Kouvola,Lahti]
-- 1150 vs 1362km
frando9 :: Word64 -> IO ()
frando9 seed = print =<< simulatedAnnealing
    (Config)
    seed
    (finnishRandonneurProblem 1000
    $ Set.fromList
        [ Helsinki, Tampere, Turku, Jyvaskyla
        , Lahti, Kuopio, Pori, Kouvola, Joensuu
        ])

frandoAll :: Word64 -> IO ()
frandoAll seed = print =<< simulatedAnnealing
    (Config)
    seed
    (finnishRandonneurProblem 100000 $ Set.fromList [minBound .. maxBound])

-------------------------------------------------------------------------------
-- Finnish Travelling Salesman results
-------------------------------------------------------------------------------

-- fitness 2446
--
-- OSM: 2375km (without Oulu and Rovaniemi)
route1 :: [City]
route1 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Lahti,Jyvaskyla,Seinajoki,Vaasa,Kokkola
    ,Oulu,Rovaniemi,Kajaani,Kuopio,Joensuu
    ,Savonlinna,Mikkeli,Lappeenranta,Kouvola,Kotka
    ,Porvoo,Hyvinkaa,Jarvenpaa,Kerava,Vantaa
    ,Helsinki]

-- fitness 2449
--
-- OSM: 2376km (without Oulu and Rovaniemi)
route2 :: [City]
route2 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Lahti,Mikkeli,Jyvaskyla,Seinajoki,Vaasa
    ,Kokkola,Oulu,Rovaniemi,Kajaani,Kuopio
    ,Joensuu,Savonlinna,Lappeenranta,Kouvola,Kotka
    ,Porvoo,Hyvinkaa,Jarvenpaa,Kerava,Vantaa
    ]

-- fitness 2460
--
-- OSM: 2363km (without Oulu and Rovaniemi)
route3 :: [City]
route3 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Seinajoki
    ,Vaasa,Kokkola,Oulu,Rovaniemi,Kajaani
    ,Kuopio,Joensuu,Savonlinna,Jyvaskyla,Mikkeli
    ,Lappeenranta,Kotka,Kouvola,Lahti,Hameenlinna
    ,Hyvinkaa,Jarvenpaa,Porvoo,Kerava,Vantaa]

openRouteService :: [City] -> String
openRouteService cs = concat $
    [ "https://maps.openrouteservice.org/directions?n1=62.410729&n2=26.438599&n3=7&"
    , "a="
    ] ++
    [ intercalate ","
        [ showFFloat (Just 6) x "" ++ "," ++ showFFloat (Just 6) y ""
        | c <- cs
        , let (x,y) = cityWGS84 c
        ]
    ] ++
    [ "&b=0&c=1&k1=en-US&k2=km"
    ]

noNorth :: [City] -> [City]
noNorth = filter (`notElem` [Oulu, Rovaniemi])

-- fitness 2422
-- OSM: 2317
route4 :: [City]
route4 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Jyvaskyla
    ,Seinajoki,Vaasa,Kokkola,Oulu,Rovaniemi
    ,Kajaani,Kuopio,Joensuu,Savonlinna,Mikkeli
    ,Lappeenranta,Kotka,Kouvola,Lahti,Hameenlinna
    ,Hyvinkaa,Jarvenpaa,Kerava,Porvoo,Vantaa]

-- fitness 2470
route5 :: [City]
route5 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Lahti,Kouvola,Mikkeli,Jyvaskyla,Seinajoki
    ,Vaasa,Kokkola,Rovaniemi,Oulu,Kajaani
    ,Kuopio,Joensuu,Savonlinna,Lappeenranta,Kotka
    ,Porvoo,Jarvenpaa,Hyvinkaa,Kerava,Vantaa]

-- fitness 2442
-- OSM: 2370
route6 :: [City]
route6 =
    [Helsinki,Vantaa,Jarvenpaa,Kerava,Porvoo
    ,Kotka,Kouvola,Lahti,Hyvinkaa,Hameenlinna
    ,Nokia,Tampere,Jyvaskyla,Mikkeli,Lappeenranta
    ,Savonlinna,Joensuu,Kuopio,Kajaani,Rovaniemi
    ,Oulu,Kokkola,Vaasa,Seinajoki,Pori
    ,Rauma,Turku,Salo,Lohja,Espoo]

--   [Oulu,Rovaniemi,Kajaani,Kuopio,Joensuu,Savonlinna
-- ,Lappeenranta,Mikkeli,Jyvaskyla,Nokia,Tampere,
-- Hameenlinna,Lahti,Kouvola,Kotka,Porvoo,
-- Hyvinkaa,Jarvenpaa,Kerava,Vantaa,Helsinki
-- ,Espoo,Lohja,Salo,Turku,Rauma,Pori,Seinajoki,Vaasa,Kokkola]

-- fitness 2451
-- OSM:
route7 :: [City]
route7 =
    [Helsinki,Espoo,Lohja,Salo,Turku
    ,Rauma,Pori,Nokia,Tampere,Hameenlinna
    ,Hyvinkaa,Lahti,Kouvola,Mikkeli,Jyvaskyla
    ,Seinajoki,Vaasa,Kokkola,Oulu,Rovaniemi
    ,Kajaani,Kuopio,Joensuu,Savonlinna,Lappeenranta
    ,Kotka,Porvoo,Jarvenpaa,Kerava,Vantaa]

-- fitness 2505
-- [Helsinki,Vantaa,Kerava,Jarvenpaa,Hyvinkaa,Porvoo,Kotka,Lappeenranta,Mikkeli,Jyvaskyla,Seinajoki,Vaasa,Kokkola,Oulu,Rovaniemi,Kajaani,Kuopio,Joensuu,Savonlinna,Kouvola,Lahti,Hameenlinna,Tampere,Nokia,Pori,Rauma,Turku,Salo,Lohja,Espoo]

-- fitness 2435
-- [Oulu,Rovaniemi,Kajaani,Kuopio,Joensuu,Savonlinna,Lappeenranta,Mikkeli,Jyvaskyla,Tampere,Nokia,Hameenlinna,Hyvinkaa,Lahti,Kouvola,Kotka,Porvoo,Jarvenpaa,Kerava,Vantaa,Helsinki,Espoo,Lohja,Salo,Turku,Rauma,Pori,Seinajoki,Vaasa,Kokkola]
