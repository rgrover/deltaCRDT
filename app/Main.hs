module Main where

import           Algebra.Lattice  (bottom, (\/))
import           Data.Maybe       (fromJust)
import qualified Data.VectorClock as VC
import           Misc.Clock
import           Misc.Pid

v1 :: Clock
v1 = VC.incWithDefault (P 1) bottom 0

v2 :: Clock
v2 = VC.incWithDefault (P 2) bottom 0

main :: IO ()
main =
    print $
    VC.incWithDefault (P 2) inc1 0
  where inc1 = VC.incWithDefault (P 1) (v1 \/ bottom) 0 :: Clock
