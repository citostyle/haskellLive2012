data Light = On
           | Off
             deriving Show
 
switch :: Light -> Light
switch On  = Off
switch Off = On
 
pass :: Int -> Int -> Light -> Light
pass n i l = if n `mod` i == 0
             then switch l
             else l
 
exec :: [a -> a] -> a -> a
exec []     x = x
exec (f:fs) x = exec fs (f x)
 
nightGuard :: Int -> Light
nightGuard n =  exec passes Off -- foldr ($) Off passes
    where passes :: [Light -> Light]
          passes = map (pass n) [1..n]
 
 
run :: Int -> IO ()
run n = putStrLn $ show n ++ " -> " ++ show (nightGuard n)
 
main :: IO ()
main = sequence_ $ map run [3, 8191, 6241]