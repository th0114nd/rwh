import Control.Monad(ap)

ex = [("name", Just "Attila \"The Hun\""),
 ("occupation", Just "Khan")]

data MovieReview = MovieReview {
    revTitle :: String
  , revUser :: String
  , revReview :: String
}

dumbReview :: [(String, Maybe String)] -> Maybe MovieReview
dumbReview alist = 
    case lookup "title" alist of
        Just (Just title@(_:_)) ->
            case lookup "user" alist of
                Just (Just user@(_:_)) ->
                    case lookup "review" alist of
                        Just (Just review@(_:_)) ->
                            Just (MovieReview title user review)
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing

betterReview :: [(String, Maybe String)] -> Maybe MovieReview
betterReview alist = do
    Just user@(_:_) <- lookup "user" alist
    Just title@(_:_) <- lookup "title" alist
    Just review@(_:_) <- lookup "review" alist
    return $ MovieReview title user review

lookup1 key alist = case lookup key alist of
                        Just (Just s@(_:_)) -> Just s
                        _ -> Nothing

liftedReview alist = liftM3 MovieReview (lookup1 "title" alist)
                                        (lookup1 "user" alist)
                                        (lookup1 "review" alist)

apReview alist = 
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist
