
import Test.QuickCheck (elements, choose, Gen(..))
class Arbitrary a where
    arbitrary :: Gen a

{-
elements :: [a] -> Gen a
choose :: Random a => (a, a) -> Gen a
oneof :: [Gen a] -> Gen a
-}

data Ternary 
    = Yes
    | No
    | Unknown
    deriving (Eq, Show)

instance Arbitrary Ternary where
    arbitrary = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                    0 -> Yes
                    1 -> No
                    _ -> Unknown 


