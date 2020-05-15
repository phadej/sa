{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module SA.Example.FinnishRandonneur (
    -- * Finnish Randonneur
    finnishRandonneurProblem,
    -- * Data
    routeDistance,
    City (..),
    cityWGS84,
    cityDistance,
) where

import Control.DeepSeq (NFData (..))
import Data.Foldable   (toList)

import qualified Data.List              as L
import qualified Data.Set               as Set
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as MV
import qualified System.Random.SplitMix as SM

import SA

newtype FinnRandoSolution = FinnRandoSolution (V.Vector City)
  deriving (Eq, Ord)

instance NFData FinnRandoSolution where
    rnf (FinnRandoSolution xs) = rnf xs

solutionToList :: FinnRandoSolution -> [City]
solutionToList (FinnRandoSolution xs) = V.toList xs

instance Show FinnRandoSolution where
    showsPrec d (FinnRandoSolution xs) =
        let (pfx, sfx) = V.span (/= Helsinki) xs
        in showsPrec d (V.toList (sfx <> pfx))

type Metric = D 2
type Beta   = D 5

routeDistance :: Foldable f => f City -> Metric
routeDistance = D . L.foldl' (+) 0 . map (uncurry cityDistance) . pairs . toList

finnishRandonneurProblem
    :: Int              -- ^ steps
    -> Set.Set City
    -> Problem Metric Beta FinnRandoSolution
finnishRandonneurProblem steps cities = Problem {..} where
    -- start and end kilometer changes
    km0, km1 :: Double
    km0 = 200
    km1 = 3

    schedule =
        [ D (log 2 / delta)
        | step <- [0 .. steps]
        , let delta = fromIntegral step / fromIntegral steps * (km1 - km0) + km0
        ]

    initial g0 = FinnRandoSolution $ V.fromList $ go g0 cities where
        go g rest | Set.null rest = []
                  | otherwise     =
            let (w, g') = SM.bitmaskWithRejection64 (fromIntegral (Set.size rest)) g
                c       = Set.elemAt (fromIntegral w) rest
            in c : go g' (Set.delete c rest)

    acceptance _ _ = isingAcceptance

    fitness :: FinnRandoSolution -> Metric
    fitness = routeDistance . solutionToList

    neighbor :: SM.SMGen -> FinnRandoSolution -> FinnRandoSolution
    neighbor g (FinnRandoSolution s) = FinnRandoSolution $
        let size = V.length s
            i, j :: Int
            (i', g') = SM.bitmaskWithRejection64 (fromIntegral size) g
            (j', _ ) = SM.bitmaskWithRejection64 (fromIntegral size) g'
            -- j'       = mod (i' + 1) (fromIntegral $ V.length s)

            i        = fromIntegral i'
            j        = fromIntegral j'

            f mv = do
                x <- MV.unsafeRead mv i
                y <- MV.unsafeRead mv j
                MV.unsafeWrite mv i y
                MV.unsafeWrite mv j x

        in V.modify f s

-- loop pairs.
pairs :: [a] -> [(a,a)]
pairs []         = []
pairs xs@(x:xs') = zip xs (xs' ++ [x])

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | One of 30 biggest cities in Finland. (they are quite small).
data City
    = Helsinki
    | Espoo
    | Tampere
    | Vantaa
    | Oulu

    | Turku
    | Jyvaskyla
    | Lahti
    | Kuopio
    | Pori

    | Kouvola
    | Joensuu
    | Lappeenranta
    | Hameenlinna
    | Vaasa

    | Seinajoki
    | Rovaniemi
    | Mikkeli
    | Kotka
    | Salo

    | Porvoo
    | Kokkola
    | Hyvinkaa
    | Lohja
    | Jarvenpaa

    | Rauma
    | Kajaani
    | Kerava
    | Savonlinna
    | Nokia
  deriving (Eq, Ord, Show, Enum, Bounded)

instance NFData City where
    rnf x = x `seq` ()

-- | WGS84 coordinates of 'City'.
cityWGS84 :: City -> (Double, Double)
cityWGS84 Helsinki     = (60.166641, 24.943537)
cityWGS84 Espoo        = (60.206376, 24.656729)
cityWGS84 Tampere      = (61.497743, 23.76129)
cityWGS84 Vantaa       = (60.298134, 25.006641)
cityWGS84 Oulu         = (65.013785, 25.472099)

cityWGS84 Turku        = (60.45169, 22.266867)
cityWGS84 Jyvaskyla    = (62.241678, 25.749498)
cityWGS84 Lahti        = (60.980381, 25.654988)
cityWGS84 Kuopio       = (62.892983, 27.688935)
cityWGS84 Pori         = (61.483726, 21.7959)

cityWGS84 Kouvola      = (60.866825, 26.705598)
cityWGS84 Lappeenranta = (61.05875, 28.18769)
cityWGS84 Joensuu      = (62.602079, 29.759679)
cityWGS84 Hameenlinna  = (60.996174, 24.464425)
cityWGS84 Vaasa        = (63.092589, 21.615874)

cityWGS84 Seinajoki    = (62.786663, 22.84228)
cityWGS84 Rovaniemi    = (66.50279, 25.728479)
cityWGS84 Mikkeli      = (61.687727, 27.273224)
cityWGS84 Kotka        = (60.465521, 26.941153)
cityWGS84 Salo         = (60.384374, 23.126727)

cityWGS84 Porvoo       = (60.395372, 25.66656)
cityWGS84 Kokkola      = (63.837583, 23.131962)
cityWGS84 Hyvinkaa     = (60.631017, 24.861124)
cityWGS84 Lohja        = (60.250916, 24.065782)
cityWGS84 Jarvenpaa    = (60.481098, 25.100747)

cityWGS84 Rauma        = (61.128738, 21.511127)
cityWGS84 Kajaani      = (64.226734, 27.728047)
cityWGS84 Kerava       = (60.404869, 25.103549)
cityWGS84 Savonlinna   = (61.869803, 28.878498)
cityWGS84 Nokia        = (61.478774, 23.508499)

-- | Distance between cities.
--
-- Computed using WGS84 model, assuming height 0.
cityDistance :: City -> City -> Double
cityDistance Helsinki    Helsinki    = 0
cityDistance Helsinki    Espoo       = 16.518025075
cityDistance Helsinki    Tampere     = 161.653533078
cityDistance Helsinki    Vantaa      = 15.061909302
cityDistance Helsinki    Oulu        = 540.914394989
cityDistance Helsinki    Turku       = 151.321102887
cityDistance Helsinki    Jyvaskyla   = 235.246121242
cityDistance Helsinki    Lahti       = 98.702916264
cityDistance Helsinki    Kuopio      = 337.034160143
cityDistance Helsinki    Pori        = 225.485300235
cityDistance Helsinki    Kouvola     = 124.306347217
cityDistance Helsinki    Joensuu     = 373.880413661
cityDistance Helsinki    Lappeenranta = 203.550333812
cityDistance Helsinki    Hameenlinna = 96.08672963
cityDistance Helsinki    Vaasa       = 370.647685302
cityDistance Helsinki    Seinajoki   = 312.666265652
cityDistance Helsinki    Rovaniemi   = 707.346163376
cityDistance Helsinki    Mikkeli     = 211.371419578
cityDistance Helsinki    Kotka       = 115.309601383
cityDistance Helsinki    Salo        = 103.415707536
cityDistance Helsinki    Porvoo      = 47.429818328
cityDistance Helsinki    Kokkola     = 419.932633537
cityDistance Helsinki    Hyvinkaa    = 51.939394229
cityDistance Helsinki    Lohja       = 49.566864518
cityDistance Helsinki    Jarvenpaa   = 36.096819439
cityDistance Helsinki    Rauma       = 216.179544936
cityDistance Helsinki    Kajaani     = 475.0318856
cityDistance Helsinki    Kerava      = 27.979684501
cityDistance Helsinki    Savonlinna  = 285.038250646
cityDistance Helsinki    Nokia       = 165.73885202
cityDistance Espoo       Helsinki    = 16.518025075
cityDistance Espoo       Espoo       = 0
cityDistance Espoo       Tampere     = 151.898840494
cityDistance Espoo       Vantaa      = 21.907869983
cityDistance Espoo       Oulu        = 537.431575164
cityDistance Espoo       Turku       = 134.81928177
cityDistance Espoo       Jyvaskyla   = 234.266433126
cityDistance Espoo       Lahti       = 102.124408483
cityDistance Espoo       Kuopio      = 339.954477838
cityDistance Espoo       Pori        = 210.803131149
cityDistance Espoo       Kouvola     = 134.396149256
cityDistance Espoo       Joensuu     = 381.339340699
cityDistance Espoo       Lappeenranta = 215.292980963
cityDistance Espoo       Hameenlinna = 88.629493837
cityDistance Espoo       Vaasa       = 359.663879421
cityDistance Espoo       Seinajoki   = 303.315228852
cityDistance Espoo       Rovaniemi   = 703.860640498
cityDistance Espoo       Mikkeli     = 217.57962845
cityDistance Espoo       Kotka       = 129.430497829
cityDistance Espoo       Salo        = 86.902871307
cityDistance Espoo       Porvoo      = 59.674114054
cityDistance Espoo       Kokkola     = 412.452187629
cityDistance Espoo       Hyvinkaa    = 48.634781382
cityDistance Espoo       Lohja       = 33.12079575
cityDistance Espoo       Jarvenpaa   = 39.218177498
cityDistance Espoo       Rauma       = 200.314226275
cityDistance Espoo       Kajaani     = 475.568245259
cityDistance Espoo       Kerava      = 33.155557676
cityDistance Espoo       Savonlinna  = 293.860551654
cityDistance Espoo       Nokia       = 154.911459756
cityDistance Tampere     Helsinki    = 161.653533078
cityDistance Tampere     Espoo       = 151.898840494
cityDistance Tampere     Tampere     = 0
cityDistance Tampere     Vantaa      = 149.783738114
cityDistance Tampere     Oulu        = 401.189985463
cityDistance Tampere     Turku       = 141.889784956
cityDistance Tampere     Jyvaskyla   = 133.480002545
cityDistance Tampere     Lahti       = 116.890931373
cityDistance Tampere     Kuopio      = 256.818060458
cityDistance Tampere     Pori        = 104.705493025
cityDistance Tampere     Kouvola     = 173.276684673
cityDistance Tampere     Joensuu     = 336.906785549
cityDistance Tampere     Lappeenranta = 242.340411938
cityDistance Tampere     Hameenlinna = 67.444326641
cityDistance Tampere     Vaasa       = 209.706262132
cityDistance Tampere     Seinajoki   = 151.428755069
cityDistance Tampere     Rovaniemi   = 566.117846331
cityDistance Tampere     Mikkeli     = 187.645489662
cityDistance Tampere     Kotka       = 207.006963719
cityDistance Tampere     Salo        = 128.73950839
cityDistance Tampere     Porvoo      = 160.459820913
cityDistance Tampere     Kokkola     = 262.773165653
cityDistance Tampere     Hyvinkaa    = 113.374598303
cityDistance Tampere     Lohja       = 139.910910118
cityDistance Tampere     Jarvenpaa   = 134.490094885
cityDistance Tampere     Rauma       = 127.362179743
cityDistance Tampere     Kajaani     = 364.961284823
cityDistance Tampere     Kerava      = 141.839616228
cityDistance Tampere     Savonlinna  = 273.987919422
cityDistance Tampere     Nokia       = 13.632313971
cityDistance Vantaa      Helsinki    = 15.061909302
cityDistance Vantaa      Espoo       = 21.907869983
cityDistance Vantaa      Tampere     = 149.783738114
cityDistance Vantaa      Vantaa      = 0
cityDistance Vantaa      Oulu        = 526.124390414
cityDistance Vantaa      Turku       = 152.10030037
cityDistance Vantaa      Jyvaskyla   = 220.208291967
cityDistance Vantaa      Lahti       = 83.888230746
cityDistance Vantaa      Kuopio      = 322.266010088
cityDistance Vantaa      Pori        = 218.683087488
cityDistance Vantaa      Kouvola     = 112.637172742
cityDistance Vantaa      Joensuu     = 360.650325732
cityDistance Vantaa      Lappeenranta = 193.385373771
cityDistance Vantaa      Hameenlinna = 83.241559948
cityDistance Vantaa      Vaasa       = 359.303622992
cityDistance Vantaa      Seinajoki   = 300.215458356
cityDistance Vantaa      Rovaniemi   = 692.544776306
cityDistance Vantaa      Mikkeli     = 197.5178303
cityDistance Vantaa      Kotka       = 108.312974132
cityDistance Vantaa      Salo        = 104.25710626
cityDistance Vantaa      Porvoo      = 38.013825216
cityDistance Vantaa      Kokkola     = 406.40847544
cityDistance Vantaa      Hyvinkaa    = 37.944018092
cityDistance Vantaa      Lohja       = 52.329032656
cityDistance Vantaa      Jarvenpaa   = 21.035744768
cityDistance Vantaa      Rauma       = 212.056683337
cityDistance Vantaa      Kajaani     = 459.993358862
cityDistance Vantaa      Kerava      = 13.040224679
cityDistance Vantaa      Savonlinna  = 272.554567341
cityDistance Vantaa      Nokia       = 154.664211262
cityDistance Oulu        Helsinki    = 540.914394989
cityDistance Oulu        Espoo       = 537.431575164
cityDistance Oulu        Tampere     = 401.189985463
cityDistance Oulu        Vantaa      = 526.124390414
cityDistance Oulu        Oulu        = 0
cityDistance Oulu        Turku       = 534.071986532
cityDistance Oulu        Jyvaskyla   = 309.315855499
cityDistance Oulu        Lahti       = 449.66365557
cityDistance Oulu        Kuopio      = 260.154837301
cityDistance Oulu        Pori        = 434.501022626
cityDistance Oulu        Kouvola     = 466.422960633
cityDistance Oulu        Joensuu     = 341.749703424
cityDistance Oulu        Lappeenranta = 461.657487481
cityDistance Oulu        Hameenlinna = 450.694645432
cityDistance Oulu        Vaasa       = 285.100645398
cityDistance Oulu        Seinajoki   = 279.793617672
cityDistance Oulu        Rovaniemi   = 166.44475136
cityDistance Oulu        Mikkeli     = 381.514398216
cityDistance Oulu        Kotka       = 512.433910999
cityDistance Oulu        Salo        = 529.671384623
cityDistance Oulu        Porvoo      = 514.848181561
cityDistance Oulu        Kokkola     = 172.921690639
cityDistance Oulu        Hyvinkaa    = 489.482830259
cityDistance Oulu        Lohja       = 535.694781713
cityDistance Oulu        Jarvenpaa   = 505.555143962
cityDistance Oulu        Rauma       = 476.869020687
cityDistance Oulu        Kajaani     = 139.087102288
cityDistance Oulu        Kerava      = 514.038294119
cityDistance Oulu        Savonlinna  = 389.379934342
cityDistance Oulu        Nokia       = 406.145848798
cityDistance Turku       Helsinki    = 151.321102887
cityDistance Turku       Espoo       = 134.81928177
cityDistance Turku       Tampere     = 141.889784956
cityDistance Turku       Vantaa      = 152.10030037
cityDistance Turku       Oulu        = 534.071986532
cityDistance Turku       Turku       = 0
cityDistance Turku       Jyvaskyla   = 272.919024915
cityDistance Turku       Lahti       = 194.085518263
cityDistance Turku       Kuopio      = 395.327077674
cityDistance Turku       Pori        = 117.792642859
cityDistance Turku       Kouvola     = 247.054004847
cityDistance Turku       Joensuu     = 464.747371924
cityDistance Turku       Lappeenranta = 329.710769413
cityDistance Turku       Hameenlinna = 134.397150577
cityDistance Turku       Vaasa       = 296.301931694
cityDistance Turku       Seinajoki   = 261.989358652
cityDistance Turku       Rovaniemi   = 695.960875993
cityDistance Turku       Mikkeli     = 303.247658766
cityDistance Turku       Kotka       = 257.157451597
cityDistance Turku       Salo        = 47.963288854
cityDistance Turku       Porvoo      = 187.357149957
cityDistance Turku       Kokkola     = 380.028203856
cityDistance Turku       Hyvinkaa    = 143.773371584
cityDistance Turku       Lohja       = 101.798003088
cityDistance Turku       Jarvenpaa   = 155.92186292
cityDistance Turku       Rauma       = 85.937373164
cityDistance Turku       Kajaani     = 506.649682853
cityDistance Turku       Kerava      = 156.311484292
cityDistance Turku       Savonlinna  = 389.202555712
cityDistance Turku       Nokia       = 132.739540241
cityDistance Jyvaskyla   Helsinki    = 235.246121242
cityDistance Jyvaskyla   Espoo       = 234.266433126
cityDistance Jyvaskyla   Tampere     = 133.480002545
cityDistance Jyvaskyla   Vantaa      = 220.208291967
cityDistance Jyvaskyla   Oulu        = 309.315855499
cityDistance Jyvaskyla   Turku       = 272.919024915
cityDistance Jyvaskyla   Jyvaskyla   = 0
cityDistance Jyvaskyla   Lahti       = 140.647318044
cityDistance Jyvaskyla   Kuopio      = 123.341647938
cityDistance Jyvaskyla   Pori        = 224.537599562
cityDistance Jyvaskyla   Kouvola     = 161.417959558
cityDistance Jyvaskyla   Joensuu     = 211.035739508
cityDistance Jyvaskyla   Lappeenranta = 184.57209839
cityDistance Jyvaskyla   Hameenlinna = 154.63034087
cityDistance Jyvaskyla   Vaasa       = 232.049230943
cityDistance Jyvaskyla   Seinajoki   = 161.590772566
cityDistance Jyvaskyla   Rovaniemi   = 475.041581121
cityDistance Jyvaskyla   Mikkeli     = 100.993168165
cityDistance Jyvaskyla   Kotka       = 207.933223736
cityDistance Jyvaskyla   Salo        = 250.115175957
cityDistance Jyvaskyla   Porvoo      = 205.789807298
cityDistance Jyvaskyla   Kokkola     = 221.744984632
cityDistance Jyvaskyla   Hyvinkaa    = 185.636722405
cityDistance Jyvaskyla   Lohja       = 239.527539776
cityDistance Jyvaskyla   Jarvenpaa   = 199.233203052
cityDistance Jyvaskyla   Rauma       = 256.299128962
cityDistance Jyvaskyla   Kajaani     = 242.561138089
cityDistance Jyvaskyla   Kerava      = 207.583966974
cityDistance Jyvaskyla   Savonlinna  = 168.802011579
cityDistance Jyvaskyla   Nokia       = 145.395263603
cityDistance Lahti       Helsinki    = 98.702916264
cityDistance Lahti       Espoo       = 102.124408483
cityDistance Lahti       Tampere     = 116.890931373
cityDistance Lahti       Vantaa      = 83.888230746
cityDistance Lahti       Oulu        = 449.66365557
cityDistance Lahti       Turku       = 194.085518263
cityDistance Lahti       Jyvaskyla   = 140.647318044
cityDistance Lahti       Lahti       = 0
cityDistance Lahti       Kuopio      = 238.379425499
cityDistance Lahti       Pori        = 214.699130146
cityDistance Lahti       Kouvola     = 58.369608795
cityDistance Lahti       Joensuu     = 281.964047545
cityDistance Lahti       Lappeenranta = 137.223317836
cityDistance Lahti       Hameenlinna = 64.465234913
cityDistance Lahti       Vaasa       = 316.26561059
cityDistance Lahti       Seinajoki   = 249.76799141
cityDistance Lahti       Rovaniemi   = 615.609006717
cityDistance Lahti       Mikkeli     = 117.122535055
cityDistance Lahti       Kotka       = 90.653840075
cityDistance Lahti       Salo        = 153.279712189
cityDistance Lahti       Porvoo      = 65.187045653
cityDistance Lahti       Kokkola     = 344.05136877
cityDistance Lahti       Hyvinkaa    = 58.163138969
cityDistance Lahti       Lohja       = 119.072101575
cityDistance Lahti       Jarvenpaa   = 63.320478107
cityDistance Lahti       Rauma       = 224.400920133
cityDistance Lahti       Kajaani     = 377.112187213
cityDistance Lahti       Kerava      = 70.848734612
cityDistance Lahti       Savonlinna  = 198.551102745
cityDistance Lahti       Nokia       = 127.971980834
cityDistance Kuopio      Helsinki    = 337.034160143
cityDistance Kuopio      Espoo       = 339.954477838
cityDistance Kuopio      Tampere     = 256.818060458
cityDistance Kuopio      Vantaa      = 322.266010088
cityDistance Kuopio      Oulu        = 260.154837301
cityDistance Kuopio      Turku       = 395.327077674
cityDistance Kuopio      Jyvaskyla   = 123.341647938
cityDistance Kuopio      Lahti       = 238.379425499
cityDistance Kuopio      Kuopio      = 0
cityDistance Kuopio      Pori        = 344.557904966
cityDistance Kuopio      Kouvola     = 231.644467137
cityDistance Kuopio      Joensuu     = 110.685117157
cityDistance Kuopio      Lappeenranta = 206.081090827
cityDistance Kuopio      Hameenlinna = 270.739731798
cityDistance Kuopio      Vaasa       = 308.50203306
cityDistance Kuopio      Seinajoki   = 247.163871927
cityDistance Kuopio      Rovaniemi   = 413.123136499
cityDistance Kuopio      Mikkeli     = 136.04694985
cityDistance Kuopio      Kotka       = 273.39423929
cityDistance Kuopio      Salo        = 369.497151216
cityDistance Kuopio      Porvoo      = 298.228297579
cityDistance Kuopio      Kokkola     = 251.086717089
cityDistance Kuopio      Hyvinkaa    = 292.924606308
cityDistance Kuopio      Lohja       = 351.656817732
cityDistance Kuopio      Jarvenpaa   = 301.628917872
cityDistance Kuopio      Rauma       = 378.410240473
cityDistance Kuopio      Kajaani     = 148.686537378
cityDistance Kuopio      Kerava      = 309.227335293
cityDistance Kuopio      Savonlinna  = 129.580743204
cityDistance Kuopio      Nokia       = 268.691306256
cityDistance Pori        Helsinki    = 225.485300235
cityDistance Pori        Espoo       = 210.803131149
cityDistance Pori        Tampere     = 104.705493025
cityDistance Pori        Vantaa      = 218.683087488
cityDistance Pori        Oulu        = 434.501022626
cityDistance Pori        Turku       = 117.792642859
cityDistance Pori        Jyvaskyla   = 224.537599562
cityDistance Pori        Lahti       = 214.699130146
cityDistance Pori        Kuopio      = 344.557904966
cityDistance Pori        Pori        = 0
cityDistance Pori        Kouvola     = 272.909920309
cityDistance Pori        Joensuu     = 434.616727038
cityDistance Pori        Lappeenranta = 346.001500438
cityDistance Pori        Hameenlinna = 153.236506708
cityDistance Pori        Vaasa       = 179.551215325
cityDistance Pori        Seinajoki   = 155.124968848
cityDistance Pori        Rovaniemi   = 591.407147383
cityDistance Pori        Mikkeli     = 291.691346149
cityDistance Pori        Kotka       = 300.730916026
cityDistance Pori        Salo        = 142.164113715
cityDistance Pori        Porvoo      = 242.298633188
cityDistance Pori        Kokkola     = 271.128526018
cityDistance Pori        Hyvinkaa    = 190.854398717
cityDistance Pori        Lohja       = 184.587123052
cityDistance Pori        Jarvenpaa   = 210.895730482
cityDistance Pori        Rauma       = 42.39896432
cityDistance Pori        Kajaani     = 429.474153207
cityDistance Pori        Kerava      = 215.816195089
cityDistance Pori        Savonlinna  = 377.309403328
cityDistance Pori        Nokia       = 91.258245785
cityDistance Kouvola     Helsinki    = 124.306347217
cityDistance Kouvola     Espoo       = 134.396149256
cityDistance Kouvola     Tampere     = 173.276684673
cityDistance Kouvola     Vantaa      = 112.637172742
cityDistance Kouvola     Oulu        = 466.422960633
cityDistance Kouvola     Turku       = 247.054004847
cityDistance Kouvola     Jyvaskyla   = 161.417959558
cityDistance Kouvola     Lahti       = 58.369608795
cityDistance Kouvola     Kuopio      = 231.644467137
cityDistance Kouvola     Pori        = 272.909920309
cityDistance Kouvola     Kouvola     = 0
cityDistance Kouvola     Joensuu     = 251.841526841
cityDistance Kouvola     Lappeenranta = 83.083503032
cityDistance Kouvola     Hameenlinna = 122.37083728
cityDistance Kouvola     Vaasa       = 364.154185919
cityDistance Kouvola     Seinajoki   = 295.234527958
cityDistance Kouvola     Rovaniemi   = 630.089553785
cityDistance Kouvola     Mikkeli     = 96.40844018
cityDistance Kouvola     Kotka       = 46.532374829
cityDistance Kouvola     Salo        = 203.130447426
cityDistance Kouvola     Porvoo      = 77.417505855
cityDistance Kouvola     Kokkola     = 379.183727755
cityDistance Kouvola     Hyvinkaa    = 103.957762685
cityDistance Kouvola     Lohja       = 160.232855638
cityDistance Kouvola     Jarvenpaa   = 97.682372352
cityDistance Kouvola     Rauma       = 282.514533599
cityDistance Kouvola     Kajaani     = 378.141073486
cityDistance Kouvola     Kerava      = 101.662311373
cityDistance Kouvola     Savonlinna  = 161.216413289
cityDistance Kouvola     Nokia       = 185.043203433
cityDistance Joensuu     Helsinki    = 373.880413661
cityDistance Joensuu     Espoo       = 381.339340699
cityDistance Joensuu     Tampere     = 336.906785549
cityDistance Joensuu     Vantaa      = 360.650325732
cityDistance Joensuu     Oulu        = 341.749703424
cityDistance Joensuu     Turku       = 464.747371924
cityDistance Joensuu     Jyvaskyla   = 211.035739508
cityDistance Joensuu     Lahti       = 281.964047545
cityDistance Joensuu     Kuopio      = 110.685117157
cityDistance Joensuu     Pori        = 434.616727038
cityDistance Joensuu     Kouvola     = 251.841526841
cityDistance Joensuu     Joensuu     = 0
cityDistance Joensuu     Lappeenranta = 190.884130041
cityDistance Joensuu     Hameenlinna = 331.554544405
cityDistance Joensuu     Vaasa       = 418.120638298
cityDistance Joensuu     Seinajoki   = 354.609163714
cityDistance Joensuu     Rovaniemi   = 475.689128011
cityDistance Joensuu     Mikkeli     = 164.897955787
cityDistance Joensuu     Kotka       = 281.313051316
cityDistance Joensuu     Salo        = 430.841744861
cityDistance Joensuu     Porvoo      = 328.494662815
cityDistance Joensuu     Kokkola     = 360.433320189
cityDistance Joensuu     Hyvinkaa    = 340.127183124
cityDistance Joensuu     Lohja       = 401.029776739
cityDistance Joensuu     Jarvenpaa   = 342.279189914
cityDistance Joensuu     Rauma       = 463.727422417
cityDistance Joensuu     Kajaani     = 207.571458479
cityDistance Joensuu     Kerava      = 348.300274046
cityDistance Joensuu     Savonlinna  = 93.590090222
cityDistance Joensuu     Nokia       = 350.078102254
cityDistance Lappeenranta Helsinki    = 203.550333812
cityDistance Lappeenranta Espoo       = 215.292980963
cityDistance Lappeenranta Tampere     = 242.340411938
cityDistance Lappeenranta Vantaa      = 193.385373771
cityDistance Lappeenranta Oulu        = 461.657487481
cityDistance Lappeenranta Turku       = 329.710769413
cityDistance Lappeenranta Jyvaskyla   = 184.57209839
cityDistance Lappeenranta Lahti       = 137.223317836
cityDistance Lappeenranta Kuopio      = 206.081090827
cityDistance Lappeenranta Pori        = 346.001500438
cityDistance Lappeenranta Kouvola     = 83.083503032
cityDistance Lappeenranta Joensuu     = 190.884130041
cityDistance Lappeenranta Lappeenranta = 0
cityDistance Lappeenranta Hameenlinna = 201.376075926
cityDistance Lappeenranta Vaasa       = 411.243837274
cityDistance Lappeenranta Seinajoki   = 340.318114652
cityDistance Lappeenranta Rovaniemi   = 618.743742144
cityDistance Lappeenranta Mikkeli     = 85.459541843
cityDistance Lappeenranta Kotka       = 94.7946123
cityDistance Lappeenranta Salo        = 286.185369024
cityDistance Lappeenranta Porvoo      = 156.165303775
cityDistance Lappeenranta Kokkola     = 404.785156838
cityDistance Lappeenranta Hyvinkaa    = 187.020236061
cityDistance Lappeenranta Lohja       = 242.694591519
cityDistance Lappeenranta Jarvenpaa   = 180.10436995
cityDistance Lappeenranta Rauma       = 360.114897574
cityDistance Lappeenranta Kajaani     = 353.873907407
cityDistance Lappeenranta Kerava      = 183.352439235
cityDistance Lappeenranta Savonlinna  = 97.596184248
cityDistance Lappeenranta Nokia       = 255.304476235
cityDistance Hameenlinna Helsinki    = 96.08672963
cityDistance Hameenlinna Espoo       = 88.629493837
cityDistance Hameenlinna Tampere     = 67.444326641
cityDistance Hameenlinna Vantaa      = 83.241559948
cityDistance Hameenlinna Oulu        = 450.694645432
cityDistance Hameenlinna Turku       = 134.397150577
cityDistance Hameenlinna Jyvaskyla   = 154.63034087
cityDistance Hameenlinna Lahti       = 64.465234913
cityDistance Hameenlinna Kuopio      = 270.739731798
cityDistance Hameenlinna Pori        = 153.236506708
cityDistance Hameenlinna Kouvola     = 122.37083728
cityDistance Hameenlinna Joensuu     = 331.554544405
cityDistance Hameenlinna Lappeenranta = 201.376075926
cityDistance Hameenlinna Hameenlinna = 0
cityDistance Hameenlinna Vaasa       = 277.069506645
cityDistance Hameenlinna Seinajoki   = 216.989874475
cityDistance Hameenlinna Rovaniemi   = 616.970121099
cityDistance Hameenlinna Mikkeli     = 168.921525795
cityDistance Hameenlinna Kotka       = 147.498766851
cityDistance Hameenlinna Salo        = 99.937280032
cityDistance Hameenlinna Porvoo      = 93.770512411
cityDistance Hameenlinna Kokkola     = 324.063705994
cityDistance Hameenlinna Hyvinkaa    = 46.061089451
cityDistance Hameenlinna Lohja       = 85.858390713
cityDistance Hameenlinna Jarvenpaa   = 67.072459277
cityDistance Hameenlinna Rauma       = 160.150451109
cityDistance Hameenlinna Kajaani     = 397.024849274
cityDistance Hameenlinna Kerava      = 74.560410763
cityDistance Hameenlinna Savonlinna  = 254.835142796
cityDistance Hameenlinna Nokia       = 74.345228389
cityDistance Vaasa       Helsinki    = 370.647685302
cityDistance Vaasa       Espoo       = 359.663879421
cityDistance Vaasa       Tampere     = 209.706262132
cityDistance Vaasa       Vantaa      = 359.303622992
cityDistance Vaasa       Oulu        = 285.100645398
cityDistance Vaasa       Turku       = 296.301931694
cityDistance Vaasa       Jyvaskyla   = 232.049230943
cityDistance Vaasa       Lahti       = 316.26561059
cityDistance Vaasa       Kuopio      = 308.50203306
cityDistance Vaasa       Pori        = 179.551215325
cityDistance Vaasa       Kouvola     = 364.154185919
cityDistance Vaasa       Joensuu     = 418.120638298
cityDistance Vaasa       Lappeenranta = 411.243837274
cityDistance Vaasa       Hameenlinna = 277.069506645
cityDistance Vaasa       Vaasa       = 0
cityDistance Vaasa       Seinajoki   = 70.996562114
cityDistance Vaasa       Rovaniemi   = 427.302914886
cityDistance Vaasa       Mikkeli     = 331.732205397
cityDistance Vaasa       Kotka       = 405.586652245
cityDistance Vaasa       Salo        = 312.167426209
cityDistance Vaasa       Porvoo      = 368.843823274
cityDistance Vaasa       Kokkola     = 112.295413836
cityDistance Vaasa       Hyvinkaa    = 323.070601586
cityDistance Vaasa       Lohja       = 342.166293811
cityDistance Vaasa       Jarvenpaa   = 344.141751176
cityDistance Vaasa       Rauma       = 218.934145853
cityDistance Vaasa       Kajaani     = 327.884589159
cityDistance Vaasa       Kerava      = 351.544523731
cityDistance Vaasa       Savonlinna  = 398.291958134
cityDistance Vaasa       Nokia       = 204.920732067
cityDistance Seinajoki   Helsinki    = 312.666265652
cityDistance Seinajoki   Espoo       = 303.315228852
cityDistance Seinajoki   Tampere     = 151.428755069
cityDistance Seinajoki   Vantaa      = 300.215458356
cityDistance Seinajoki   Oulu        = 279.793617672
cityDistance Seinajoki   Turku       = 261.989358652
cityDistance Seinajoki   Jyvaskyla   = 161.590772566
cityDistance Seinajoki   Lahti       = 249.76799141
cityDistance Seinajoki   Kuopio      = 247.163871927
cityDistance Seinajoki   Pori        = 155.124968848
cityDistance Seinajoki   Kouvola     = 295.234527958
cityDistance Seinajoki   Joensuu     = 354.609163714
cityDistance Seinajoki   Lappeenranta = 340.318114652
cityDistance Seinajoki   Hameenlinna = 216.989874475
cityDistance Seinajoki   Vaasa       = 70.996562114
cityDistance Seinajoki   Seinajoki   = 0
cityDistance Seinajoki   Rovaniemi   = 436.553647648
cityDistance Seinajoki   Mikkeli     = 260.82398641
cityDistance Seinajoki   Kotka       = 337.76186791
cityDistance Seinajoki   Salo        = 268.133163755
cityDistance Seinajoki   Porvoo      = 305.716915436
cityDistance Seinajoki   Kokkola     = 118.038845846
cityDistance Seinajoki   Hyvinkaa    = 262.863531301
cityDistance Seinajoki   Lohja       = 289.9699529
cityDistance Seinajoki   Jarvenpaa   = 283.429097107
cityDistance Seinajoki   Rauma       = 197.518022221
cityDistance Seinajoki   Kajaani     = 291.343660633
cityDistance Seinajoki   Kerava      = 291.269816808
cityDistance Seinajoki   Savonlinna  = 328.997401442
cityDistance Seinajoki   Nokia       = 149.84572218
cityDistance Rovaniemi   Helsinki    = 707.346163376
cityDistance Rovaniemi   Espoo       = 703.860640498
cityDistance Rovaniemi   Tampere     = 566.117846331
cityDistance Rovaniemi   Vantaa      = 692.544776306
cityDistance Rovaniemi   Oulu        = 166.44475136
cityDistance Rovaniemi   Turku       = 695.960875993
cityDistance Rovaniemi   Jyvaskyla   = 475.041581121
cityDistance Rovaniemi   Lahti       = 615.609006717
cityDistance Rovaniemi   Kuopio      = 413.123136499
cityDistance Rovaniemi   Pori        = 591.407147383
cityDistance Rovaniemi   Kouvola     = 630.089553785
cityDistance Rovaniemi   Joensuu     = 475.689128011
cityDistance Rovaniemi   Lappeenranta = 618.743742144
cityDistance Rovaniemi   Hameenlinna = 616.970121099
cityDistance Rovaniemi   Vaasa       = 427.302914886
cityDistance Rovaniemi   Seinajoki   = 436.553647648
cityDistance Rovaniemi   Rovaniemi   = 0
cityDistance Rovaniemi   Mikkeli     = 541.992988527
cityDistance Rovaniemi   Kotka       = 675.641376369
cityDistance Rovaniemi   Salo        = 694.103958151
cityDistance Rovaniemi   Porvoo      = 680.789321992
cityDistance Rovaniemi   Kokkola     = 321.051579194
cityDistance Rovaniemi   Hyvinkaa    = 655.927579564
cityDistance Rovaniemi   Lohja       = 701.758659185
cityDistance Rovaniemi   Jarvenpaa   = 671.950157221
cityDistance Rovaniemi   Rauma       = 633.71005899
cityDistance Rovaniemi   Kajaani     = 270.260701437
cityDistance Rovaniemi   Kerava      = 680.429956142
cityDistance Rovaniemi   Savonlinna  = 538.532403189
cityDistance Rovaniemi   Nokia       = 570.413717541
cityDistance Mikkeli     Helsinki    = 211.371419578
cityDistance Mikkeli     Espoo       = 217.57962845
cityDistance Mikkeli     Tampere     = 187.645489662
cityDistance Mikkeli     Vantaa      = 197.5178303
cityDistance Mikkeli     Oulu        = 381.514398216
cityDistance Mikkeli     Turku       = 303.247658766
cityDistance Mikkeli     Jyvaskyla   = 100.993168165
cityDistance Mikkeli     Lahti       = 117.122535055
cityDistance Mikkeli     Kuopio      = 136.04694985
cityDistance Mikkeli     Pori        = 291.691346149
cityDistance Mikkeli     Kouvola     = 96.40844018
cityDistance Mikkeli     Joensuu     = 164.897955787
cityDistance Mikkeli     Lappeenranta = 85.459541843
cityDistance Mikkeli     Hameenlinna = 168.921525795
cityDistance Mikkeli     Vaasa       = 331.732205397
cityDistance Mikkeli     Seinajoki   = 260.82398641
cityDistance Mikkeli     Rovaniemi   = 541.992988527
cityDistance Mikkeli     Mikkeli     = 0
cityDistance Mikkeli     Kotka       = 137.364823743
cityDistance Mikkeli     Salo        = 266.971922867
cityDistance Mikkeli     Porvoo      = 168.141804126
cityDistance Mikkeli     Kokkola     = 319.516330602
cityDistance Mikkeli     Hyvinkaa    = 175.275062158
cityDistance Mikkeli     Lohja       = 236.190794923
cityDistance Mikkeli     Jarvenpaa   = 178.369104501
cityDistance Mikkeli     Rauma       = 313.892733803
cityDistance Mikkeli     Kajaani     = 283.937659827
cityDistance Mikkeli     Kerava      = 184.847232899
cityDistance Mikkeli     Savonlinna  = 87.11627473
cityDistance Mikkeli     Nokia       = 201.276598192
cityDistance Kotka       Helsinki    = 115.309601383
cityDistance Kotka       Espoo       = 129.430497829
cityDistance Kotka       Tampere     = 207.006963719
cityDistance Kotka       Vantaa      = 108.312974132
cityDistance Kotka       Oulu        = 512.433910999
cityDistance Kotka       Turku       = 257.157451597
cityDistance Kotka       Jyvaskyla   = 207.933223736
cityDistance Kotka       Lahti       = 90.653840075
cityDistance Kotka       Kuopio      = 273.39423929
cityDistance Kotka       Pori        = 300.730916026
cityDistance Kotka       Kouvola     = 46.532374829
cityDistance Kotka       Joensuu     = 281.313051316
cityDistance Kotka       Lappeenranta = 94.7946123
cityDistance Kotka       Hameenlinna = 147.498766851
cityDistance Kotka       Vaasa       = 405.586652245
cityDistance Kotka       Seinajoki   = 337.76186791
cityDistance Kotka       Rovaniemi   = 675.641376369
cityDistance Kotka       Mikkeli     = 137.364823743
cityDistance Kotka       Kotka       = 0
cityDistance Kotka       Salo        = 210.274365051
cityDistance Kotka       Porvoo      = 70.629010274
cityDistance Kotka       Kokkola     = 424.888509505
cityDistance Kotka       Hyvinkaa    = 115.614635817
cityDistance Kotka       Lohja       = 160.485724485
cityDistance Kotka       Jarvenpaa   = 101.23580588
cityDistance Kotka       Rauma       = 304.673177763
cityDistance Kotka       Kajaani     = 421.159614294
cityDistance Kotka       Kerava      = 101.410687855
cityDistance Kotka       Savonlinna  = 188.022432443
cityDistance Kotka       Nokia       = 217.455599199
cityDistance Salo        Helsinki    = 103.415707536
cityDistance Salo        Espoo       = 86.902871307
cityDistance Salo        Tampere     = 128.73950839
cityDistance Salo        Vantaa      = 104.25710626
cityDistance Salo        Oulu        = 529.671384623
cityDistance Salo        Turku       = 47.963288854
cityDistance Salo        Jyvaskyla   = 250.115175957
cityDistance Salo        Lahti       = 153.279712189
cityDistance Salo        Kuopio      = 369.497151216
cityDistance Salo        Pori        = 142.164113715
cityDistance Salo        Kouvola     = 203.130447426
cityDistance Salo        Joensuu     = 430.841744861
cityDistance Salo        Lappeenranta = 286.185369024
cityDistance Salo        Hameenlinna = 99.937280032
cityDistance Salo        Vaasa       = 312.167426209
cityDistance Salo        Seinajoki   = 268.133163755
cityDistance Salo        Rovaniemi   = 694.103958151
cityDistance Salo        Mikkeli     = 266.971922867
cityDistance Salo        Kotka       = 210.274365051
cityDistance Salo        Salo        = 0
cityDistance Salo        Porvoo      = 140.048550573
cityDistance Salo        Kokkola     = 384.8505313
cityDistance Salo        Hyvinkaa    = 99.172781841
cityDistance Salo        Lohja       = 53.983851256
cityDistance Salo        Jarvenpaa   = 109.237335637
cityDistance Salo        Rauma       = 120.981245222
cityDistance Salo        Kajaani     = 489.973674935
cityDistance Salo        Kerava      = 109.010177841
cityDistance Salo        Savonlinna  = 351.224565614
cityDistance Salo        Nokia       = 123.69076917
cityDistance Porvoo      Helsinki    = 47.429818328
cityDistance Porvoo      Espoo       = 59.674114054
cityDistance Porvoo      Tampere     = 160.459820913
cityDistance Porvoo      Vantaa      = 38.013825216
cityDistance Porvoo      Oulu        = 514.848181561
cityDistance Porvoo      Turku       = 187.357149957
cityDistance Porvoo      Jyvaskyla   = 205.789807298
cityDistance Porvoo      Lahti       = 65.187045653
cityDistance Porvoo      Kuopio      = 298.228297579
cityDistance Porvoo      Pori        = 242.298633188
cityDistance Porvoo      Kouvola     = 77.417505855
cityDistance Porvoo      Joensuu     = 328.494662815
cityDistance Porvoo      Lappeenranta = 156.165303775
cityDistance Porvoo      Hameenlinna = 93.770512411
cityDistance Porvoo      Vaasa       = 368.843823274
cityDistance Porvoo      Seinajoki   = 305.716915436
cityDistance Porvoo      Rovaniemi   = 680.789321992
cityDistance Porvoo      Mikkeli     = 168.141804126
cityDistance Porvoo      Kotka       = 70.629010274
cityDistance Porvoo      Salo        = 140.048550573
cityDistance Porvoo      Porvoo      = 0
cityDistance Porvoo      Kokkola     = 405.718849063
cityDistance Porvoo      Hyvinkaa    = 51.448737078
cityDistance Porvoo      Lohja       = 89.900941422
cityDistance Porvoo      Jarvenpaa   = 32.585093591
cityDistance Porvoo      Rauma       = 240.75739467
cityDistance Porvoo      Kajaani     = 440.129502867
cityDistance Porvoo      Kerava      = 31.053822284
cityDistance Porvoo      Savonlinna  = 238.581727276
cityDistance Porvoo      Nokia       = 168.097013488
cityDistance Kokkola     Helsinki    = 419.932633537
cityDistance Kokkola     Espoo       = 412.452187629
cityDistance Kokkola     Tampere     = 262.773165653
cityDistance Kokkola     Vantaa      = 406.40847544
cityDistance Kokkola     Oulu        = 172.921690639
cityDistance Kokkola     Turku       = 380.028203856
cityDistance Kokkola     Jyvaskyla   = 221.744984632
cityDistance Kokkola     Lahti       = 344.05136877
cityDistance Kokkola     Kuopio      = 251.086717089
cityDistance Kokkola     Pori        = 271.128526018
cityDistance Kokkola     Kouvola     = 379.183727755
cityDistance Kokkola     Joensuu     = 360.433320189
cityDistance Kokkola     Lappeenranta = 404.785156838
cityDistance Kokkola     Hameenlinna = 324.063705994
cityDistance Kokkola     Vaasa       = 112.295413836
cityDistance Kokkola     Seinajoki   = 118.038845846
cityDistance Kokkola     Rovaniemi   = 321.051579194
cityDistance Kokkola     Mikkeli     = 319.516330602
cityDistance Kokkola     Kotka       = 424.888509505
cityDistance Kokkola     Salo        = 384.8505313
cityDistance Kokkola     Porvoo      = 405.718849063
cityDistance Kokkola     Kokkola     = 0
cityDistance Kokkola     Hyvinkaa    = 368.470977037
cityDistance Kokkola     Lohja       = 402.683755896
cityDistance Kokkola     Jarvenpaa   = 387.847809258
cityDistance Kokkola     Rauma       = 313.239734366
cityDistance Kokkola     Kajaani     = 228.732943254
cityDistance Kokkola     Kerava      = 396.115181462
cityDistance Kokkola     Savonlinna  = 365.495330028
cityDistance Kokkola     Nokia       = 263.61013888
cityDistance Hyvinkaa    Helsinki    = 51.939394229
cityDistance Hyvinkaa    Espoo       = 48.634781382
cityDistance Hyvinkaa    Tampere     = 113.374598303
cityDistance Hyvinkaa    Vantaa      = 37.944018092
cityDistance Hyvinkaa    Oulu        = 489.482830259
cityDistance Hyvinkaa    Turku       = 143.773371584
cityDistance Hyvinkaa    Jyvaskyla   = 185.636722405
cityDistance Hyvinkaa    Lahti       = 58.163138969
cityDistance Hyvinkaa    Kuopio      = 292.924606308
cityDistance Hyvinkaa    Pori        = 190.854398717
cityDistance Hyvinkaa    Kouvola     = 103.957762685
cityDistance Hyvinkaa    Joensuu     = 340.127183124
cityDistance Hyvinkaa    Lappeenranta = 187.020236061
cityDistance Hyvinkaa    Hameenlinna = 46.061089451
cityDistance Hyvinkaa    Vaasa       = 323.070601586
cityDistance Hyvinkaa    Seinajoki   = 262.863531301
cityDistance Hyvinkaa    Rovaniemi   = 655.927579564
cityDistance Hyvinkaa    Mikkeli     = 175.275062158
cityDistance Hyvinkaa    Kotka       = 115.614635817
cityDistance Hyvinkaa    Salo        = 99.172781841
cityDistance Hyvinkaa    Porvoo      = 51.448737078
cityDistance Hyvinkaa    Kokkola     = 368.470977037
cityDistance Hyvinkaa    Hyvinkaa    = 0
cityDistance Hyvinkaa    Lohja       = 60.917096098
cityDistance Hyvinkaa    Jarvenpaa   = 21.256667366
cityDistance Hyvinkaa    Rauma       = 190.184006344
cityDistance Hyvinkaa    Kajaani     = 427.13761754
cityDistance Hyvinkaa    Kerava      = 28.499408782
cityDistance Hyvinkaa    Savonlinna  = 255.986058784
cityDistance Hyvinkaa    Nokia       = 119.416833568
cityDistance Lohja       Helsinki    = 49.566864518
cityDistance Lohja       Espoo       = 33.12079575
cityDistance Lohja       Tampere     = 139.910910118
cityDistance Lohja       Vantaa      = 52.329032656
cityDistance Lohja       Oulu        = 535.694781713
cityDistance Lohja       Turku       = 101.798003088
cityDistance Lohja       Jyvaskyla   = 239.527539776
cityDistance Lohja       Lahti       = 119.072101575
cityDistance Lohja       Kuopio      = 351.656817732
cityDistance Lohja       Pori        = 184.587123052
cityDistance Lohja       Kouvola     = 160.232855638
cityDistance Lohja       Joensuu     = 401.029776739
cityDistance Lohja       Lappeenranta = 242.694591519
cityDistance Lohja       Hameenlinna = 85.858390713
cityDistance Lohja       Vaasa       = 342.166293811
cityDistance Lohja       Seinajoki   = 289.9699529
cityDistance Lohja       Rovaniemi   = 701.758659185
cityDistance Lohja       Mikkeli     = 236.190794923
cityDistance Lohja       Kotka       = 160.485724485
cityDistance Lohja       Salo        = 53.983851256
cityDistance Lohja       Porvoo      = 89.900941422
cityDistance Lohja       Kokkola     = 402.683755896
cityDistance Lohja       Hyvinkaa    = 60.917096098
cityDistance Lohja       Lohja       = 0
cityDistance Lohja       Jarvenpaa   = 62.605172319
cityDistance Lohja       Rauma       = 170.413203953
cityDistance Lohja       Kajaani     = 482.085560521
cityDistance Lohja       Kerava      = 59.843646375
cityDistance Lohja       Savonlinna  = 316.25226802
cityDistance Lohja       Nokia       = 140.125942946
cityDistance Jarvenpaa   Helsinki    = 36.096819439
cityDistance Jarvenpaa   Espoo       = 39.218177498
cityDistance Jarvenpaa   Tampere     = 134.490094885
cityDistance Jarvenpaa   Vantaa      = 21.035744768
cityDistance Jarvenpaa   Oulu        = 505.555143962
cityDistance Jarvenpaa   Turku       = 155.92186292
cityDistance Jarvenpaa   Jyvaskyla   = 199.233203052
cityDistance Jarvenpaa   Lahti       = 63.320478107
cityDistance Jarvenpaa   Kuopio      = 301.628917872
cityDistance Jarvenpaa   Pori        = 210.895730482
cityDistance Jarvenpaa   Kouvola     = 97.682372352
cityDistance Jarvenpaa   Joensuu     = 342.279189914
cityDistance Jarvenpaa   Lappeenranta = 180.10436995
cityDistance Jarvenpaa   Hameenlinna = 67.072459277
cityDistance Jarvenpaa   Vaasa       = 344.141751176
cityDistance Jarvenpaa   Seinajoki   = 283.429097107
cityDistance Jarvenpaa   Rovaniemi   = 671.950157221
cityDistance Jarvenpaa   Mikkeli     = 178.369104501
cityDistance Jarvenpaa   Kotka       = 101.23580588
cityDistance Jarvenpaa   Salo        = 109.237335637
cityDistance Jarvenpaa   Porvoo      = 32.585093591
cityDistance Jarvenpaa   Kokkola     = 387.847809258
cityDistance Jarvenpaa   Hyvinkaa    = 21.256667366
cityDistance Jarvenpaa   Lohja       = 62.605172319
cityDistance Jarvenpaa   Jarvenpaa   = 0
cityDistance Jarvenpaa   Rauma       = 208.280851388
cityDistance Jarvenpaa   Kajaani     = 438.977837871
cityDistance Jarvenpaa   Kerava      = 8.494819281
cityDistance Jarvenpaa   Savonlinna  = 255.409501001
cityDistance Jarvenpaa   Nokia       = 140.669686641
cityDistance Rauma       Helsinki    = 216.179544936
cityDistance Rauma       Espoo       = 200.314226275
cityDistance Rauma       Tampere     = 127.362179743
cityDistance Rauma       Vantaa      = 212.056683337
cityDistance Rauma       Oulu        = 476.869020687
cityDistance Rauma       Turku       = 85.937373164
cityDistance Rauma       Jyvaskyla   = 256.299128962
cityDistance Rauma       Lahti       = 224.400920133
cityDistance Rauma       Kuopio      = 378.410240473
cityDistance Rauma       Pori        = 42.39896432
cityDistance Rauma       Kouvola     = 282.514533599
cityDistance Rauma       Joensuu     = 463.727422417
cityDistance Rauma       Lappeenranta = 360.114897574
cityDistance Rauma       Hameenlinna = 160.150451109
cityDistance Rauma       Vaasa       = 218.934145853
cityDistance Rauma       Seinajoki   = 197.518022221
cityDistance Rauma       Rovaniemi   = 633.71005899
cityDistance Rauma       Mikkeli     = 313.892733803
cityDistance Rauma       Kotka       = 304.673177763
cityDistance Rauma       Salo        = 120.981245222
cityDistance Rauma       Porvoo      = 240.75739467
cityDistance Rauma       Kokkola     = 313.239734366
cityDistance Rauma       Hyvinkaa    = 190.184006344
cityDistance Rauma       Lohja       = 170.413203953
cityDistance Rauma       Jarvenpaa   = 208.280851388
cityDistance Rauma       Rauma       = 0
cityDistance Rauma       Kajaani     = 469.345154826
cityDistance Rauma       Kerava      = 211.727080455
cityDistance Rauma       Savonlinna  = 400.720325219
cityDistance Rauma       Nokia       = 113.91916397
cityDistance Kajaani     Helsinki    = 475.0318856
cityDistance Kajaani     Espoo       = 475.568245259
cityDistance Kajaani     Tampere     = 364.961284823
cityDistance Kajaani     Vantaa      = 459.993358862
cityDistance Kajaani     Oulu        = 139.087102288
cityDistance Kajaani     Turku       = 506.649682853
cityDistance Kajaani     Jyvaskyla   = 242.561138089
cityDistance Kajaani     Lahti       = 377.112187213
cityDistance Kajaani     Kuopio      = 148.686537378
cityDistance Kajaani     Pori        = 429.474153207
cityDistance Kajaani     Kouvola     = 378.141073486
cityDistance Kajaani     Joensuu     = 207.571458479
cityDistance Kajaani     Lappeenranta = 353.873907407
cityDistance Kajaani     Hameenlinna = 397.024849274
cityDistance Kajaani     Vaasa       = 327.884589159
cityDistance Kajaani     Seinajoki   = 291.343660633
cityDistance Kajaani     Rovaniemi   = 270.260701437
cityDistance Kajaani     Mikkeli     = 283.937659827
cityDistance Kajaani     Kotka       = 421.159614294
cityDistance Kajaani     Salo        = 489.973674935
cityDistance Kajaani     Porvoo      = 440.129502867
cityDistance Kajaani     Kokkola     = 228.732943254
cityDistance Kajaani     Hyvinkaa    = 427.13761754
cityDistance Kajaani     Lohja       = 482.085560521
cityDistance Kajaani     Jarvenpaa   = 438.977837871
cityDistance Kajaani     Rauma       = 469.345154826
cityDistance Kajaani     Kajaani     = 0
cityDistance Kajaani     Kerava      = 447.06745252
cityDistance Kajaani     Savonlinna  = 269.067263442
cityDistance Kajaani     Nokia       = 373.982747084
cityDistance Kerava      Helsinki    = 27.979684501
cityDistance Kerava      Espoo       = 33.155557676
cityDistance Kerava      Tampere     = 141.839616228
cityDistance Kerava      Vantaa      = 13.040224679
cityDistance Kerava      Oulu        = 514.038294119
cityDistance Kerava      Turku       = 156.311484292
cityDistance Kerava      Jyvaskyla   = 207.583966974
cityDistance Kerava      Lahti       = 70.848734612
cityDistance Kerava      Kuopio      = 309.227335293
cityDistance Kerava      Pori        = 215.816195089
cityDistance Kerava      Kouvola     = 101.662311373
cityDistance Kerava      Joensuu     = 348.300274046
cityDistance Kerava      Lappeenranta = 183.352439235
cityDistance Kerava      Hameenlinna = 74.560410763
cityDistance Kerava      Vaasa       = 351.544523731
cityDistance Kerava      Seinajoki   = 291.269816808
cityDistance Kerava      Rovaniemi   = 680.429956142
cityDistance Kerava      Mikkeli     = 184.847232899
cityDistance Kerava      Kotka       = 101.410687855
cityDistance Kerava      Salo        = 109.010177841
cityDistance Kerava      Porvoo      = 31.053822284
cityDistance Kerava      Kokkola     = 396.115181462
cityDistance Kerava      Hyvinkaa    = 28.499408782
cityDistance Kerava      Lohja       = 59.843646375
cityDistance Kerava      Jarvenpaa   = 8.494819281
cityDistance Kerava      Rauma       = 211.727080455
cityDistance Kerava      Kajaani     = 447.06745252
cityDistance Kerava      Kerava      = 0
cityDistance Kerava      Savonlinna  = 260.711972163
cityDistance Kerava      Nokia       = 147.621931324
cityDistance Savonlinna  Helsinki    = 285.038250646
cityDistance Savonlinna  Espoo       = 293.860551654
cityDistance Savonlinna  Tampere     = 273.987919422
cityDistance Savonlinna  Vantaa      = 272.554567341
cityDistance Savonlinna  Oulu        = 389.379934342
cityDistance Savonlinna  Turku       = 389.202555712
cityDistance Savonlinna  Jyvaskyla   = 168.802011579
cityDistance Savonlinna  Lahti       = 198.551102745
cityDistance Savonlinna  Kuopio      = 129.580743204
cityDistance Savonlinna  Pori        = 377.309403328
cityDistance Savonlinna  Kouvola     = 161.216413289
cityDistance Savonlinna  Joensuu     = 93.590090222
cityDistance Savonlinna  Lappeenranta = 97.596184248
cityDistance Savonlinna  Hameenlinna = 254.835142796
cityDistance Savonlinna  Vaasa       = 398.291958134
cityDistance Savonlinna  Seinajoki   = 328.997401442
cityDistance Savonlinna  Rovaniemi   = 538.532403189
cityDistance Savonlinna  Mikkeli     = 87.11627473
cityDistance Savonlinna  Kotka       = 188.022432443
cityDistance Savonlinna  Salo        = 351.224565614
cityDistance Savonlinna  Porvoo      = 238.581727276
cityDistance Savonlinna  Kokkola     = 365.495330028
cityDistance Savonlinna  Hyvinkaa    = 255.986058784
cityDistance Savonlinna  Lohja       = 316.25226802
cityDistance Savonlinna  Jarvenpaa   = 255.409501001
cityDistance Savonlinna  Rauma       = 400.720325219
cityDistance Savonlinna  Kajaani     = 269.067263442
cityDistance Savonlinna  Kerava      = 260.711972163
cityDistance Savonlinna  Savonlinna  = 0
cityDistance Savonlinna  Nokia       = 287.611181049
cityDistance Nokia       Helsinki    = 165.73885202
cityDistance Nokia       Espoo       = 154.911459756
cityDistance Nokia       Tampere     = 13.632313971
cityDistance Nokia       Vantaa      = 154.664211262
cityDistance Nokia       Oulu        = 406.145848798
cityDistance Nokia       Turku       = 132.739540241
cityDistance Nokia       Jyvaskyla   = 145.395263603
cityDistance Nokia       Lahti       = 127.971980834
cityDistance Nokia       Kuopio      = 268.691306256
cityDistance Nokia       Pori        = 91.258245785
cityDistance Nokia       Kouvola     = 185.043203433
cityDistance Nokia       Joensuu     = 350.078102254
cityDistance Nokia       Lappeenranta = 255.304476235
cityDistance Nokia       Hameenlinna = 74.345228389
cityDistance Nokia       Vaasa       = 204.920732067
cityDistance Nokia       Seinajoki   = 149.84572218
cityDistance Nokia       Rovaniemi   = 570.413717541
cityDistance Nokia       Mikkeli     = 201.276598192
cityDistance Nokia       Kotka       = 217.455599199
cityDistance Nokia       Salo        = 123.69076917
cityDistance Nokia       Porvoo      = 168.097013488
cityDistance Nokia       Kokkola     = 263.61013888
cityDistance Nokia       Hyvinkaa    = 119.416833568
cityDistance Nokia       Lohja       = 140.125942946
cityDistance Nokia       Jarvenpaa   = 140.669686641
cityDistance Nokia       Rauma       = 113.91916397
cityDistance Nokia       Kajaani     = 373.982747084
cityDistance Nokia       Kerava      = 147.621931324
cityDistance Nokia       Savonlinna  = 287.611181049
cityDistance Nokia       Nokia       = 0
