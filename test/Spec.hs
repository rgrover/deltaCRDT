import           Test.QuickCheck

import qualified SimulatedConcurrency
import qualified SingleReplica
import qualified TwoReplicas

import           System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [ SingleReplica.runTests
                             , TwoReplicas.runTests
                             ]
    if good
        then exitSuccess
        else exitFailure
