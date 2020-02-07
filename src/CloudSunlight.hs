{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module CloudSunlight
  ( cloudSunlightExperiment
  )
where

import           Control.Monad
import qualified Data.ByteString.Lazy          as BL
import           Data.Csv
import qualified Data.List                     as L
import           System.Random.MWC
import qualified System.Random.MWC.Distributions
                                               as D


toa :: GenIO -> IO Double
toa = D.normal 340.0 30.0

-- | magnitude of aerosol affect on sunlight, between 1.0 and 0.0 (1.0 = max effect)
aerosolAffectSunlight :: Double
aerosolAffectSunlight = 1.0

-- | magnitude of aerosol affect on cloud, between 1.0 and 0.0 (1.0 = max effect)
aerosolAffectCloud :: Double
aerosolAffectCloud = 1.0

-- | factor that cloud reduces sunlight, if there is cloud
cloudAffectSunlight :: Double
cloudAffectSunlight = 0.6

sunlight
  :: Double -- ^ non-dim measure of aerosol
  -> Bool -- ^ cloud
  -> GenIO
  -> IO Double -- ^ W/m2
sunlight a c gen = toa gen >>= \toa' ->
  let clearSky = toa' * (1.0 - aerosolAffectSunlight * a)
  in  return (if c then clearSky * cloudAffectSunlight else clearSky)

cloud :: Double -> GenIO -> IO Bool
cloud a = D.bernoulli (0.5 + aerosolAffectCloud * (a - 0.5))

aerosol :: GenIO -> IO Double
aerosol = uniform

genTuple :: GenIO -> IO (Double, String, Double)
genTuple gen = do
  a <- aerosol gen
  c <- cloud a gen
  s <- sunlight a c gen
  return (a, if c then "Cloudy" else "Clear", s)

getCloud :: (a, b, c) -> b
getCloud (_a, c, _s) = c

getSun :: (a, b, c) -> c
getSun (_a, _c, s) = s

calcStats :: [(Double, String, Double)] -> Double
calcStats xs = f cld - f noCld
 where
  (cld, noCld) = L.partition ((== "Cloudy") . getCloud) xs
  f xs' = (/ fromIntegral (length xs')) . sum . fmap getSun $ xs'

cloudSunlightExperiment :: Int -> FilePath -> IO Double
cloudSunlightExperiment n fpath = do
  gen <- create
  xs  <- replicateM n (genTuple gen)
  let naiveDiff = calcStats xs
  BL.writeFile fpath
    . ("aerosol\tcloud\tsunlight\n" <>)
    . encodeWith (defaultEncodeOptions { encDelimiter = 9 })
    $ xs
  return naiveDiff
