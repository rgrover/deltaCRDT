import           Test.QuickCheck

gen :: Gen (Int, String)
gen = do
    i <- arbitrary
    s <- elements ["rohit", "Grover", "test"]
    return (i,s)

test :: Int -> Int -> Bool
test a b = a + b == b + a

main :: IO ()
main = quickCheck test
