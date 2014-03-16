import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress name namNum numCar carBil = do
    num <- M.lookup name namNum
    car <- M.lookup num numCar
    add <- M.lookup car carBil
    return add

variation3 person phoneMap carrierMap addressMap = 
    lookup phoneMap person >>= lookup carrierMap >>= addressMap
  where lookup = flip M.lookup
