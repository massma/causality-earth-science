{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module GraphDiagrams
  ( genericGraph
  )
where


import           Diagrams.Prelude
import           Diagrams.Backend.Cairo
import           Text.Printf

nodeSize = 3
node = circle nodeSize # pad 1.1
observed label = text label <> node

unObserved label = text label <> node # dashingN [0.01, 0.01] 0.01
-- common default of pixel per inch is 150, so 5 x 3 in = 750 x 450 pixel

renderSize = dims $ r2 (750, 450)

boundedLabel l = text l <> square (nodeSize * 2) # lcA transparent

diagramUnits x = x * nodeSize * 2 * 1.5

timeSlice (f1, f2, t) =
  ( names'
  , atPoints ps [f1 "" # named (names' !! 0), f2 "" # named (names' !! 1)]
  )
 where
  vLoc   = negate t
  ps     = fmap (\hLoc -> P (r2 (diagramUnits hLoc, diagramUnits vLoc))) [0, 1]
  names' = [vLoc * 2, vLoc * 2 + 1]

displayDiagram fpath = renderCairo fpath renderSize

diagramState f1s f2s ts = namedF
  (foldr
    (<>)
    (atPoints [P (r2 (diagramUnits 0.5, diagramUnits vLoc))]
              [observed "E(t+1)" # named (names' !! 0)]
    )
    diagrams
  )
 where
  (names, diagrams) = unzip $ fmap timeSlice (zip3 f1s f2s ts)
  namedF            = foldr (.) id $ zipWith
    (\n1s n2s -> foldr (.) id ((\n1 n2 -> connectOutside n1 n2) <$> n1s <*> n2s)
    )
    names
    (drop 1 names <> [names'])
  vLoc   = negate (last ts + 1)
  names' = [vLoc * 2]

labelState ts = atPoints ps labels
 where
  ps     = fmap (\t -> P (r2 (0, diagramUnits (negate t)))) ts
  labels = fmap
    (\t -> boundedLabel (if t == 0 then "S(t)" else (printf "S(t%1.0f)" t)))
    ts

genericGraph fpath = displayDiagram
  fpath
  (   labelState ts
  ||| diagramState [observed, unObserved, observed]
                   (replicate 3 unObserved)
                   ts
  )
  where ts = [-2, -1, 0]
