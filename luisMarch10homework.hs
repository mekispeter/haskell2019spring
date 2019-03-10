-- convergent seriesA - performs 1+1/2^n n
convergentSeriesA:: Integer -> Double
convergentSeriesA 0 = 1
convergentSeriesA n = convergentSeriesA (n-1) + 1/2^n

-- convergentSeriesB performs  x+x/2^2
convergentSeriesB :: Integer -> Double -> Double
convergentSeriesB 0 x = 1
convergentSeriesB n x = convergentSeriesB (n-1) x + 1/x^n


--need to work on this to avoid binary tree problem
--fibonnacciA:: Integer -> Integer
--fibonnacci 0

--stroke convert generates string with n strokes
strokeConvert :: Integer -> String
strokeConvert 0 = ""
strokeConvert n = '|':strokeConvert (n-1)
