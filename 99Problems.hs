{-# LANGUAGE ScopedTypeVariables #-}


-- Problem 1
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:xs) = if null xs then x else myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast (a:[]) = error "Only one item"
myButLast (x:y:xs) = if null xs then x else myButLast (y:xs)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- Problem 4
myLength :: [a] -> Int 
myLength = foldr (\a b -> b + 1) 0

-- Problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\acc a -> a : acc) []

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = foldr (\(x, y) acc -> (x == y) && acc) True (zip xs (reverse xs))

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List nxs) = concat $ foldr (\l acc -> flatten l:acc ) [] nxs

-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress = foldr (\x acc -> if (not $ null acc) && x==(head acc) then acc else x:acc) []

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

-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b =
    let primes = filter isPrime [3..b]
    in filter (\(m,n) -> m + n < b && m + n > a && m < n ) [(a,b) | a <- primes, b <- primes ]

-- Binary trees
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf x = Branch x Empty Empty

-- Problem 55
-- cbalTree :: Int -> [ Tree [Int]]
-- cbalTree n =  cbalTree' n leaf "x"
--     where cbalTree' n tree = cbal' (n-1) 
