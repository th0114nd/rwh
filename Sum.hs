import Data.Char
import Data.Bits

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _ = acc

base = 655521
adler :: [Int] -> Int -> Int -> Int
adler ss off len = helper (1, 0) 0 (drop (off - 1) ss)
    where helper (a, b) i (s:ss) 
            | i >= len = bits a b
            | otherwise = helper (newa, newb) (i + 1) ss
                where newa = (a + s `mod` 256) `mod` base
                      newb = (newa + b) `mod` base
                      bits a b = (b `shiftL` 16) .|. a

adler32_try2 = helper (1, 0) 
    where helper (a, b) (x:xs) = 
            let a' = (a + (ord x .&. 0xff)) `mod` base
                b' = (a' + b) `mod` base
            in helper (a', b') xs
          helper (a, b) _  = (b `shiftL` 16) .|. a

adler32_try3 xs = (b `shiftL` 16) .|. a
    where (a, b) = foldl update (1, 0) xs
          update (u, v) x = (a', b')
           where
              a' = (u + (ord x .&. 0xff)) `mod` base
              b' = (a' + v) `mod` base
