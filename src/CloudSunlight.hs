{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module CloudSunlight
  ( cloudSunlightExperiment,
  )
where

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.List as L
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Regression as R
import qualified Statistics.Sample as S
import System.Random.MWC
import qualified System.Random.MWC.Distributions as D

meanToa :: Double
meanToa = 340.0

toa :: GenIO -> IO Double
toa = D.normal meanToa 30.0

-- | magnitude of aerosol affect on sunlight, between 1.0 and 0.0 (1.0 = max effect)
aerosolAffectSunlight :: Double
aerosolAffectSunlight = 1.0

-- | factor that cloud reduces sunlight, if there is cloud
cloudAffectSunlight :: Double
cloudAffectSunlight = 0.6

sunlight ::
  -- | non-dim measure of aerosol
  Double ->
  -- | cloud
  Bool ->
  -- | U_sun (i.e. TOA)
  Double ->
  -- | W/m2
  Double
sunlight a c toa' = if c then clearSky * cloudAffectSunlight else clearSky
  where
    clearSky = toa' * (1.0 - aerosolAffectSunlight * a)

cloud :: Double -> Double -> Bool
cloud a u
  | (a + u) > 1.0 = True
  | otherwise = False

aerosol :: Double -> Double
aerosol u = u

genTuple :: GenIO -> IO (Double, String, Double)
genTuple gen = do
  uAero <- uniform gen
  uCld <- uniform gen
  uSun <- toa gen
  let a = aerosol uAero
  let c = cloud a uCld
  let s = sunlight a c uSun
  return (a, if c then "Cloudy" else "Clear", s)

getCloud :: (a, b, c) -> b
getCloud (_a, c, _s) = c

getSun :: (a, b, c) -> c
getSun (_a, _c, s) = s

getAerosol :: (a, b, c) -> a
getAerosol (a, _c, _s) = a

calcRegression :: V.Vector Double -> Double -> Double
calcRegression ols x = x * ols V.! 0 + ols V.! 1

calcStats ::
  [(Double, String, Double)] ->
  [(Double, String, Double)] ->
  (V.Vector Double, V.Vector Double, Double, Double)
calcStats cloud' clear =
  ( olsCloud,
    olsClear,
    avgRegress olsCloud - avgRegress olsClear,
    naiveCloud - naiveClear
  )
  where
    calcAvgs xs = (as, ols, S.mean ss)
      where
        ss = V.fromList (fmap getSun xs)
        as = V.fromList (fmap getAerosol xs)
        ols = fst . R.olsRegress [as] $ ss
    (aCloud, olsCloud, naiveCloud) = calcAvgs cloud'
    (aClear, olsClear, naiveClear) = calcAvgs clear
    totAs = aCloud <> aClear
    avgRegress ols = S.mean $ V.map (calcRegression ols) totAs

cloudSunlightExperiment :: Int -> IO (Double, Double, Double)
cloudSunlightExperiment n = do
  gen <- create
  xs <- replicateM n (genTuple gen)
  let (cloud', clear) = L.partition ((== "Cloudy") . getCloud) xs
  let (olsCloud, olsClear, estDiff, naiveDiff) = calcStats cloud' clear
  let regPoints = [0, 0.1 .. 1]
  f "dat/cloudy.dat" cloud'
  f "dat/clear.dat" clear
  write
    "aerosol\tsunlight\n"
    "dat/olsCloudy.dat"
    (zip regPoints (fmap (calcRegression olsCloud) regPoints))
  write
    "aerosol\tsunlight\n"
    "dat/olsClear.dat"
    (zip regPoints (fmap (calcRegression olsClear) regPoints))
  return (estDiff, naiveDiff, (0.5 * meanToa * (cloudAffectSunlight - 1.0)))
  where
    f = write "aerosol\tcloud\tsunlight\n"
    write header' fpath =
      BL.writeFile fpath . (header' <>)
        . encodeWith
          (defaultEncodeOptions {encDelimiter = 9})
