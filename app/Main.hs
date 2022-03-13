module Main where
import Data.Maybe (isNothing)

-- Assertion help
check :: Bool -> IO ()
check False = error "test failed"
check True = putStr "!" 

-- 1: Last in array
lst :: [a] -> Maybe a
lst [] = Nothing
lst [n] = Just n
lst (n:ns) = lst ns

-- 2: Second last in array
blst :: [a] -> Maybe a
blst [] = Nothing
blst [a] = Nothing
blst [n, a] = Just n
blst (n:ns) = blst ns

-- 3: K'th Element
kth :: [a] -> Int -> Maybe a
kth [] _ = Nothing
kth (a:_) 1 = Just a
kth (a:ns) i = kth ns (i - 1)

-- 4: List length
len :: [a] -> Int
len [] = 0
len [a] = 1
len (a:as) = 1 + len as

-- 5: List reversal
rev :: [a] -> [a]
rev [a, b] = [b, a]
rev (a:ns) = rev ns ++ [a]
rev [] = []

-- 6: Palindrome
pal :: Eq a => [a] -> Bool
pal [a, b] = a == b
pal [a] = True
pal [] = True
pal (a:ns) = pal [a, last ns] && pal (init ns)

-- 7: Flatten list
data NestedList a = Elem a | List [NestedList a]

fla :: NestedList a -> [a]
fla x = case x of
    Elem x -> [x]
    List [] -> []
    List (x:xs) -> fla x ++ fla (List xs)

-- 8: Remove consecutive duplicates
rdup :: Eq a => [a] -> [a]
rdup x = case x of
    [] -> []
    [x] -> [x]
    (a:b:cs) -> if a == b then rdup (b:cs) else a:rdup (b:cs)

-- 9: Pack consecutive duplicates
pdup :: Eq a => [a] -> [[a]]
pdup x = case x of
    [] -> []
    l@(a:as) -> takeWhile (== a) l : pdup (dropWhile (== a) as)

-- 10: Run-length encoding
rle :: Eq a => [a] -> [(Int, a)]
rle [] = []
rle l = map (\x -> (length x, head x)) $ pdup l

-- 11: Modified run-length encoding
data Run a = Single a | Multiple Int a deriving (Eq, Show)

mrle :: Eq a => [a] -> [Run a]
mrle [] = []
mrle l = map run $ pdup l
    where run x = case l of
            1 -> Single h
            _ -> Multiple l h
            where l = length x; h = head x

-- 12: Decoding a run-length encoded list
unrl :: [Run a] -> [a]
unrl [] = []
unrl (a:b) = expand a ++ unrl b
    where expand r = case r of
            Single x -> [x]
            Multiple t x -> map (const x) [1..t]

-- 13: Directly encoding a run-length list
drle :: Eq a => [a] -> [Run a]
drle [] = []
drle l = foldr join [Single $ last l] (init l)
    where join i acc = case head acc of
            Single b -> add b 1
            Multiple c b -> add b c
            where add b c
                    | i == b = Multiple (c + 1) i : tail acc
                    | otherwise = Single i : acc

-- 14: Duplicate list elements
dup :: [a] -> [a]
dup l = case l of
    [] -> []
    x:xs -> [x, x] ++ dup xs

-- 15: Duplicate elements n times
dupn :: [a] -> Int -> [a]
dupn [] _ = []
dupn (x:xs) i = map (const x) [1..i] ++ dupn xs i

-- 16: Drop every n'th element
dropi :: [a] -> Int -> [a]
dropi [] _ = []
dropi l i = take (i - 1) l ++ dropi (drop i l) i

main :: IO ()
main = do
    check $ dropi "abcdefghik" 3 == "abdeghk"

    check $ dupn [1, 1] 1 == [1, 1]
    check $ dupn [1, 2] 3 == [1, 1, 1, 2, 2, 2]

    check $ dup [1, 2, 2, 3] == [1, 1, 2, 2, 2, 2, 3, 3]

    check $ drle "" == mrle ""
    check $ drle "aaabbc" == mrle "aaabbc"

    check $ unrl (mrle "aaabbbccd") == "aaabbbccd"

    check $ mrle [1, 1, 2, 3, 3] == [Multiple 2 1, Single 2, Multiple 2 3]
    check $ mrle "abbbc" == [Single 'a', Multiple 3 'b', Single 'c']

    check $ null $ rle ([] :: [Int])
    check $ rle "aaabbbccd" == [(3, 'a'), (3, 'b'), (2, 'c'), (1, 'd')]

    check $ pdup "aa" == ["aa"]
    check $ pdup "aabb" == ["aa", "bb"]
    check $ pdup "aaaabccaadeeee" == ["aaaa", "b", "cc", "aa", "d", "eeee"]

    check $ rdup "aaaabccaadeeee" == "abcade"

    check $ fla (Elem 1) == [1]
    check $ fla (List [Elem 1, Elem 2]) == [1, 2]
    check $ fla (List [Elem 1, Elem 2, List [Elem 3, Elem 4]]) == [1, 2, 3, 4]

    check $ pal [1, 1]
    check $ pal [1, 3, 1]
    check $ pal [1, 3, 3, 1]
    check $ pal "racecar"
    check $ pal ""

    check $ rev [1, 2, 3] == [3, 2, 1]
    check $ rev [3, 1, 1] == [1, 1, 3]
    check $ rev [1] == [1]
    check $ rev [2, 1] == [1, 2]

    check $ len [] == 0
    check $ len [1, 2, 3] == 3
    check $ len [1] == 1

    check $ kth ["one", "two", "three"] 3 == Just "three"
    check $ kth ["a"] 1 == Just "a"
    check $ isNothing $ kth [] 3

    check $ blst ["one", "two", "three"] == Just "two"

    check $ lst ["one", "two", "three"] == Just "three"
    check $ isNothing $ lst []

    putStr "\n"

