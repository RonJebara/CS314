localMaxima :: [Integer] -> [Integer]
localMaxima[]           = []
localMaxima(x:[])       = []
localMaxima(x:y:[])     = []
localMaxima(x:y:z:zs)
        | y > z && y > x = y:localMaxima (y:z:zs)
        | otherwise = localMaxima (y:z:zs)

countEach :: [Int] -> [Int]
countEach xs = map (subtract 1 . length) (group . sort $ [0..9] ++ filter (\x -> x >= 0 && x <=9) xs)

nrToStars :: [Int] -> [String]
nrToStars xs = map (\x -> '=' : replicate x '*' ++ replicate (maximum xs - x) ' ') xs

histogram :: [Int] -> String
histogram xs = intercalate "\n" (reverse . transpose . nrToStars . countEach $ xs) ++ "\n0123456789\n"

main = do
        print (skips ["ABCDEF"])
        print (localMaxima [1,3,2,8,9,5])
        putStr (histogram[1,0,2,4,2,4,9,8,6,1,1,1,1,1,1])
