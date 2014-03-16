import Data.List

data Op = Plus | Minus | Mul | Div | Pow
    deriving (Eq, Show)

data SymbolicManip a = 
    Number a
  | Symbol String
  | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
  | UnaryArith String (SymbolicManip a)
    deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    fromInteger i = Number (fromInteger i)
    signum _ = undefined

instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    a ** b = BinaryArith Pow a b
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) = 
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
        in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = 
    opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen x@(BinaryArith _ _  _) = "(" ++ prettyShow x ++ ")"
simpleParen x = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
    show = prettyShow

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i = let toList (Number x) = [show x]
                toList (Symbol x) = [x]
                toList (BinaryArith op a b) = toList a ++ toList b ++
                                              [op2str op]
                toList (UnaryArith op a) = toList a ++ [op]
            in intercalate " " (toList i)

simplify :: (Num a, Eq a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
        in case (op, sa, sb) of 
            (Mul, Number 1, b) -> b
            (Mul, a, Number 1) -> a
            (Mul, Number 0, b) -> Number 0
            (Mul, a, Number 0) -> Number 0
            (Div, a, Number 1) -> a
            (Plus, a, Number 0) -> a
            (Plus, Number 0, b) -> b
            (Minus, a, Number 0) -> a
            (Mul, Number x, Number y) -> Number (x * y)
            (Plus, Number x, Number y) -> Number (x + y)
            (Minus, Number x, Number y) -> Number (x - y)
            _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x


data Units a = Units a (SymbolicManip a)
    deriving (Eq)

instance (Num a, Eq a) => Num (Units a) where
    (Units xa ua ) + (Units xb ub) | ua == ub = Units (xa + xb) ua
                                   | otherwise = error "Mismatched units."

    (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
    (Units xa ua) - (Units xb ub) = (Units xa ua) + (Units (-xb) ub)
    negate (Units xa ua) = Units (negate xa) ua
    abs (Units xa ua) = Units (abs xa) ua
    signum (Units xa _) = Units (signum xa) 1
    fromInteger i = Units (fromInteger i) 1

instance (Fractional a, Eq a) => Fractional (Units a) where
    (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
    recip a = 1 / a
    fromRational r = Units (fromRational r) (Number 1)

instance (Floating a, Eq a) => Floating (Units a) where
    pi = (Units pi 1)
    exp _ = error "Cannot have infinite unit."
    log _ = error "Cannot have infinite unit."
    sinh _ = error "Cannot have infinite unit."
    asinh _ = error "Cannot have infinite unit."
    cosh _ = error "Cannot have infinite unit."
    tanh _ = error "Cannot have infinite unit."
    acosh _ = error "Cannot have infinite unit."
    atanh _ = error "Cannot have infinite unit."
    sin = trigWrap sin
    cos = trigWrap cos
    tan = trigWrap tan
    asin = aTrigWrap sin
    acos = aTrigWrap cos
    atan = aTrigWrap tan

trigWrap :: (Floating a, Eq a) => (a -> a) -> (Units a -> Units a) 
trigWrap f = wrappedF
    where
        wrappedF (Units xa ua) | ua == Symbol "rad" = Units (f xa) 1
                               | ua == Symbol "deg" = Units (f (deg2rad xa)) 1
                               | otherwise = error "Trig units are deg or rad."

aTrigWrap :: (Floating a, Eq a) => (a -> a) -> (Units a -> Units a)
aTrigWrap f = \(Units xa ua) -> 
            if (ua == Number 1) 
                    then Units (rad2deg $ asin xa) (Symbol "deg")
                    else error "No support for infinite units."
    

units :: a -> String -> Units a
units a b = Units a (Symbol b)

dropUnits :: Units z -> z
dropUnits (Units x _) = x

deg2rad x = 2 * pi * x / 360
rad2deg x = 360 * x / (2 * pi)

instance (Show a, Num a, Eq a) => Show (Units a) where
    show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

test :: (Num a) => a
test = 2 * 5 + 3 
