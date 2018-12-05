module Main where

import           Algebra.Lattice  (bottom, (\/))
import           Data.Maybe       (fromJust)
import qualified Data.VectorClock as VC
import           Misc.Clock
import           Misc.Pid

v1 = VC.fromList [(P 1, E 1), (P 2, E 0)] :: Clock

v2 = VC.fromList [(P 1, E 0), (P 2, E 1)] :: Clock

main :: IO ()
main =
    print $
    fromJust $ VC.inc (P 2) $ fromJust $ VC.inc (P 1) (v1 \/ bottom)
