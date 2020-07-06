{-# LANGUAGE ScopedTypeVariables #-}


import           Control.Monad.State

-- Problem 14
dupli :: [a] -> [a]
dupli = concat . map (\a -> [a, a])

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> [ x | a <- [1 .. n] ]) xs

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n =
    let myZippedlist = zip xs $ cycle [1 .. n]
        filterNs     = filter ((/= n) . snd)
    in  map fst $ filterNs myZippedlist

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take' n xs, drop' n xs)
  where
    take' n xs = map fst $ zip xs [1 .. n]
    drop' n xs = map fst $ filter ((> n) . snd) $ zip xs [1 ..]

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs n m | n > 0 = drop (m - n) $ take m xs

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n =
    let t        = n `mod` length xs
        (us, vs) = splitAt t xs
    in  vs ++ us

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs =
    let (us, vs) = splitAt (n - 1) xs in (head vs, us ++ (drop 1 vs))

-- Problem 31
isPrime :: Int -> Bool
isPrime n = not $ any (\x -> n `mod` x == 0) [2 .. checkTill]
    where checkTill = floor . sqrt . fromIntegral $ n

-- Problem 32
gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' n m = gcd' m (n `mod` m)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b = if gcd a b == 1 then True else False

-- Problem 34
totient :: Int -> Int
totient n = length $ filter (coprime n) [1 .. (n - 1)]


-- Problem 35
primeFactors :: Int -> [Int]
primeFactors n =
    let primesToCheck = filter isPrime [2 ..]
        newList :: Int -> [Int] -> [Int] -> [Int]
        newList 1 _          divList = divList
        newList _ []         divList = divList
        newList n primesList divList = if (n `mod` head primesList == 0)
            then newList (n `div` (head primesList))
                         (primesList)
                         ((head primesList) : divList)
            else newList n (tail primesList) divList
    in  reverse $ newList n primesToCheck []


-- Problem 36


-- Problem 36

-- Problem 40
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList n

-- Tests
type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo", c + 1)
               | otherwise      = ("bar", c + 1)

stateIntString :: State Int String
stateIntString = state fromStoAandS
