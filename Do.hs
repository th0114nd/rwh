{-
doNotation1 = do act -> translated1 = act

doNotation2 =  -> translated2 = act1 >>= do act2
    do act1
       act2
       --> finalTranslation = act1 >> act2 >> ... >> actN

doNotation3 = 
    do pattern <- act1 
       act2

       -> translated3 = 
                    let f pattern = do act2
                        f _ = fail "..."
                     in act1 >>= f
-}
robust :: [a] -> Maybe a
robust xs = do (_:x:_) <- Just xs
               return x

doNotation4 =
    do let val1 = expr1
       act1

       -> translated4 = let val1 = expr1 in do act1

semicolon = do 
  { 
    act1;
    val1 <- act2;
    let {val2 = expr1 };
    actN;
  }
semicolonTranslated = 
    act1 >>
    let f val1 = let val2 = expr1
                 in actN
        f _ = fail "..."
    in act2 >>= f
            
