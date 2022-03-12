module Main where
import Data.Maybe (isNothing)

-- Assertion help
check :: Bool -> IO ()
check False = error "test failed!"
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

-- 9: Pack consecutive duplicates (not working)
pdup :: Eq a => [a] -> [[a]]
pdup x = case x of
    [] -> [[]]
    [x] -> [[x]]
    (a:b:c) -> if a == b then [[a, b]] else [[a], [b]]

main :: IO ()
main = do
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

    check $ blst ["on", "two", "three"] == Just "two"

    check $ lst ["one", "two", "three"] == Just "three"
    check $ isNothing $ lst []

    putStr "\n"

