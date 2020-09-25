--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake n xx = aux n xx 1
        where aux n [] a = []
              aux n (x:xs) a | n <= 0 = []
                             | n == a = [x] 
                             | otherwise = x : aux n xs (a + 1)

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop n xx = aux n xx 1
        where aux n [] a = []
              aux n (x:xs) a | n <= 0 = (x:xs)
                             | n == a = xs 
                             | otherwise = aux n xs (a + 1)

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xx = aux xx []
    where aux [] a = a
          aux (x:xs) a = aux xs (x:a)            -- 1:2:3:[] = [1,2,3] // el linked list los va poniendo  en reverse ---> el primero es 1
                                                            -- y queda de ultimo al final
--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app iz de = aux (rev iz) de de
    where aux [] [] a = a 
          aux [] yy a = a
          aux (x:xs) [] a = aux xs a (x:a)
          aux (x:xs) (y:ys) a = aux xs ys (x : a)

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist xx = aux xx []
    where aux [] a = rev a
          aux (x:xs) a = aux xs (x + 1 : a)

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist xx = aux xx 0
    where aux [] a = a
          aux (x:xs) a = aux xs (a + x)

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip xx yy = aux xx yy []
    where aux [] [] a = rev a
          aux [] yy a = rev a
          aux xx [] a = rev a
          aux (x:xs) (y:ys) a = aux xs ys ((x,y) : a)

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs xx yy = aux (myzip xx yy) []
    where aux [] a = rev a
          aux (x:xs) a = aux xs ((fst x + snd x) : a)

--- ### ones
-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones 

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = aux (-1)
        where aux a = (a + 1) : aux (a + 1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib =  0 : 1 : 1 : tail (aux 0 1)
    where aux a b = (b + a) : aux (b) (b + a)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x xx = aux x xx []
    where  aux n [] a = n:rev a
           aux n (y:ys) a | y > n = app (rev a) (n:y:ys)
                         | y == n = app (rev a) (y:ys) 
                         | y < n && y == last (y:ys) = (app (rev a) (app [y] [n]))
                         | otherwise = aux n ys (y:a)

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a] 
union xx [] = xx
union [] yy = yy
union (x:xs) (y:ys) | x == y = x : union xs ys
                    | x < y = x : union xs (y:ys)
                    | otherwise = y : union (x:xs) ys

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect xx yy = aux xx yy []
    where aux [] [] a = rev a
          aux [] xx a = rev a
          aux yy [] a = rev a
          aux (x:xs) (y:ys) a | x `elem` (y:ys) = aux xs (y:ys) (x:a)
                              | otherwise = aux xs (y:ys) a                                         

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = union (map (add x) (powerset xs)) (powerset xs)
    where map f [] = []
          map f (x:xs) = f x : map f xs
--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' [] = []
inclist' (x:xs) = [z + 1 | z <- (x:xs)]

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' [] = 0
sumlist' (x:xs) = (+) x (sumlist' xs)