-- CptS 355 - Lab 1 (Haskell) - Spring 2022
-- Name: Jiwoo Kim
-- Collaborated with: 

module Lab1
     where


-- 1.insert 

insert 0 item [] = item:[] --will not be inserted if n is 0
insert n item [] = [] --item is empty then print empty
insert n item (x:xs) | (n==0) = item:x:xs
                     | otherwise = x:(insert (n-1) item xs)




-- 2. insertEvery

insertEvery n item iL = insertEveryHelper n n item iL
                         where
                              insertEveryHelper m 0 item [] = item:[]
                              insertEveryHelper m n item [] = []
                              insertEveryHelper m n item (x:xs) | (n==0) = item:x:(insertEveryHelper m (m-1) item xs)
                                                                | otherwise = x:(insertEveryHelper m (n-1) item xs)




-- 3. getSales
getSales day [] = 0
getSales day ((x,y):xs) | (day == x) = y + (getSales day xs)
                        | otherwise = (getSales day xs)

                                                  
-- 4. sumSales
sumSales store day [] = 0
sumSales store day ((x,y):xs) | (store == x) = getSales day y + (sumSales store day xs)
                              | otherwise = (sumSales store day xs)





-- 5. split

split v iL = splitHelper v iL []
               where 
                    splitHelper v [] n = (reverse n) : []
                    splitHelper v (x:xs) n | (x == v) = (reverse n) : (splitHelper v xs [v])
                                           | otherwise = splitHelper v xs (x:n) 



-- 6. nSplit

