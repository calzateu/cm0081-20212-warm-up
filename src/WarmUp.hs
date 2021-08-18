

import Numeric.Natural
import Test.QuickCheck(arbitrary, Arbitrary, arbitrarySizedNatural,
                     shrink, shrinkIntegral, quickCheck)

fac :: Natural -> Natural
fac n = product [1..n ]


instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

fac1 :: Natural -> Natural
fac1 n = if n == 0 
           then 1
           else n * fac1 (n-1)

fac2 :: Natural -> Natural
fac2 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac2 ((-) n 1)))))

fac3 :: Natural -> Natural
fac3 0 = 1
fac3 n = n * fac3 (n-1)

fac4 :: Natural -> Natural
fac4 n = foldr (\x g n -> g (x*n)) id [1..n] 1

fac5 :: Natural -> Natural
fac5 = foldr (*) 1 . enumFromTo 1

-- Properties

prop_fac :: [Natural -> Natural] -> Natural -> Bool
prop_fac [] num = True
prop_fac (x:xs) num = x num == fac num && prop_fac xs num



------------------------------------------------------------------------------
-- Main

main :: IO ()
main = quickCheck $ prop_fac [fac1,fac2,fac3,fac4,fac5]

