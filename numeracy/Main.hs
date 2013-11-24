import System.Environment
import System.IO

import Data.Char
import Data.List

main :: IO ()
main = do
    a <- getArgs
    let n = read (a !! 0) :: Integer

    putStr "Base 26 (A=0):\t"
    putStrLn $ base26 n

    case base36 n of
        Nothing -> return ()
        Just b36str -> putStrLn $ "Base 36:\t" ++ b36str
    case base100 n of
        Nothing -> return ()
        Just b100str -> putStrLn $ "Base 100 (A=1):\t" ++ b100str
    case base100' n of
        Nothing -> return ()
        Just b100str -> putStrLn $ "Base 100 (A=0):\t" ++ b100str
    case ascii n of
        Nothing -> return ()
        Just str -> putStrLn $ "ASCII:\t\t" ++ str

    hFlush stdout
    putStrLn "Trying other bases..."
    let large_candidates = candidateBases n
    mapM_ (($ n) . tryBase) large_candidates
    
tryBase :: Integer -> Integer -> IO ()
tryBase d n = case baseStr d n of
    Nothing -> return ()
    Just str -> putStrLn $ "Base " ++ show d ++ ":\t" ++ str

toLetter :: Integer -> Char
toLetter x = chr ((fromInteger x) + ord 'A')

base :: Integer -> Integer -> [Integer]
base k = reverse . base_ k

-- Digits in little-endian
base_ :: Integer -> Integer -> [Integer]
base_ k 0 = []
base_ k n = (n `mod` k) : base_ k (n `div` k)

base26 :: Integer -> String
base26 = map toLetter . base 26

base36 :: Integer -> Maybe String
base36 n = if all (>= 10) digits
    then Just $ map (toLetter . subtract 10) digits
    else Nothing
    where digits = base 36 n

base100 :: Integer -> Maybe String
base100 n = if all (\x -> x >= 1 && x <= 26) digits
    then Just $ map (toLetter . subtract 1) digits
    else Nothing
    where digits = base 100 n

base100' :: Integer -> Maybe String
base100' n = if all (< 26) digits
    then Just $ map toLetter digits
    else Nothing
    where digits = base 100 n

ascii :: Integer -> Maybe String
ascii n = if all (\x -> x >= 32 && x < 128) digits
    then Just $ map (chr . fromInteger) digits
    else Nothing
    where digits = base 256 n

baseStr :: Integer -> Integer -> Maybe String
baseStr k n = if all (\x -> x >= 1 && x <= 26) digits
    then Just $ map (toLetter . subtract 1) digits
    else Nothing
    where digits = base k n

-- For a large base b, n should be between b^k and 27*b^k for
-- some k, or in other words, log n - k*log b < log 27
isCandidateBase :: Integer -> Integer -> Bool
isCandidateBase b n = log n' - log b' * fromInteger (floor (log n' / log b')) < log 27
    where n' = fromInteger n :: Double
          b' = fromInteger b :: Double

-- For a given n, exponent k, and leading digit d, the candidate
-- base is (n/d)^(1/k)
candidateBase :: Integer -> Integer -> Integer -> Integer
candidateBase n k d = extractRoot (n `div` d) k

-- Compute n^(1/k)
extractRoot n k = search 1 n
    where search lo hi = if lo == hi
            then lo
            else if g^k <= n
                 then search g hi
                 else search lo (g-1)
                 where g = (lo + hi + 1) `div` 2

-- Ignores bases that result in less than 3 letters
candidateBases :: Integer -> [Integer]
candidateBases n = map head . group . sort . filter (\x -> x >= 26 && x^3 < n) $
                   candidateBases_ n >>= (\x -> [x-26 .. x+26])
candidateBases_ :: Integer -> [Integer]
candidateBases_ n = map head . group . sort . filter (>= 26) $
                    [candidateBase n k d
                    | k <- [1 .. floor (log (fromInteger n) / log 26)]
                    , d <- [1 .. 26]
                    ]
