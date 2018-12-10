import           Test.QuickCheck

import qualified SingleReplica

import           System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [SingleReplica.runTests]
    if good
        then exitSuccess
        else exitFailure
