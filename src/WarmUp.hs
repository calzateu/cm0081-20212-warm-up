

import Numeric.Natural

fac :: Natural -> Natural
fac n = product [1..n ]


instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

fac1 n = if n == 0 
           then 1
           else n * fac (n-1)

fac2 = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))

fac3  0   =  1
fac (n+1) = (n+1) * fac n

fac4 0 = 1
fac n = n * fac (n-1)

fac5 n = foldr (\x g n -> g (x*n)) id [1..n] 1

-- Properties

prop_fac :: [Natural -> Natural] -> Natural -> Bool
prop_fac (x:xs) num = x n == fac num



------------------------------------------------------------------------------
-- Main

main :: IO ()
main = quickCheck $ prop_fac [fac1,fac2,fac3,fac4,fac5]

