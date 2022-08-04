-- CptS 355 - Lab 2 (Haskell) - Spring 2022
-- Name: Jiwoo Kim
-- Collaborated with: 

module Lab2

     where


-- 1
{- (a) merge2 -}

merge2 :: [a] -> [a] -> [a]
merge2 [] [] = [] -- empty empty = empty
merge2 [] (y:ys) = y:(merge2 [] ys) -- if right side list is empty then [y:ys] will print
merge2 (x:xs) [] = x:(merge2 xs []) -- if left side list is empty then [x:xs] will print
merge2 (x:xs) (y:ys) = x:y:(merge2 xs ys) -- if both are not empty then merge x and y 

                         

{- (b) merge2Tail -}

merge2Tail :: [a] -> [a] -> [a]
merge2Tail list1 list2 = mergehelper list1 list2 []
                   where mergehelper [] [] buf = (reverse buf)
                         mergehelper [] (y:ys) buf = (mergehelper [] ys (y:buf))
                         mergehelper (x:xs) [] buf = (mergehelper xs [] (x:buf))
                         mergehelper (x:xs) (y:ys) buf = (mergehelper xs ys (y:x:buf))






{- (c) mergeN -}

mergeN :: [[a]] -> [a]
mergeN [] = []
mergeN (x:xs) = foldl (merge2) x xs



-- 2
{- (a) count -}
count :: Eq a => a -> [a] -> Int
count i [] = 0
count i (x:xs) = length(filter(\s -> s == i) (x:xs))


{- (b) histogram  -}


uniqueElement :: Eq a => [a] -> [a]
uniqueElement [] = []
uniqueElement (x:xs) | (elem x xs == True) = uniqueElement xs -- if did not find the unique element then keep search it 
                     | otherwise = x : uniqueElement xs --add value if the unique element in the list

histogram :: Eq a => [a] -> [(a, Int)]
histogram list = histogramHelper (uniqueElement list)
                 where histogramHelper xs = map(\s -> (s, count s list)) xs

-- 3                
{- (a) concatAll -}

concatAll ::  [[String]] -> String 
concatAll xs = concatAllHelper (map concatAllHelper xs)
               where concatAllHelper xs = foldr (++) "" xs



{- (b) concat2Either -}               



data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)


concat2Either:: [[AnEither]] -> AnEither 
concat2Either [] = AString ""
concat2Either xs = concat2Helper (map concat2Helper xs)
               where concat2Helper xs = foldr (maybeConcat2) (AString "") xs -- use (AString "") because it is a nested list 
                     maybeConcat2 (AString x) (AString y) = AString(x ++ y) --AString "string string" append x and y
                     maybeConcat2 (AString x) (AnInt y) = AString(x ++ (show y)) --AString "string int" convert y and append x
                     maybeConcat2 (AnInt x) (AnInt y) = AString ((show x) ++ (show y)) --AString "int int" covert x and y then append
                     maybeConcat2 (AnInt x) (AString y) = AString ((show x) ++ y)-- AString "int string" convert x and append y




-- 4      
{-  concat2Str -}   



concat2Str:: [[AnEither]] -> String
concat2Str xs = foldr (++) "" (map concat2SHelper xs) 
                where 
                     concat2SHelper xs = foldr (maybeConcat2) "" xs
                     maybeConcat2 (AString "") [] = "" -- " "" print out [] "
                     maybeConcat2 (AString x) s = x ++ s -- append x and s
                     maybeConcat2 (AnInt x) s = (show x) ++ s --convert to int and append




data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}



-- 6
{- printInfix -}



--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)






