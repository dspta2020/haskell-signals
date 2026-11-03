{-
  Trying to reimplement some functions from:
  https://hackage.haskell.org/package/dsp-0.2.5.2/docs/DSP-Basic.html
-}
-- makes vector of zeros 
zeros 

-- linspace takes a start a stop and makes n evenly spaced samples
linspace 
  :: Double   -- start value
  -> Double   -- stop value
  -> Int      -- number of positions
  -> [Double] -- linearly spaced output vector
linspace start stop n
  | n <= 0    = []
  | n == 1    = [start]
  | otherwise = [start + spacing * fromIntegral i | i <- [0 .. n - 1]]
  where
    spacing = (stop - start) / fromIntegral (n - 1)

-- unit delay simply prepends a zero --
delay1 :: Num a => [a] -> [a]
delay1 []  = []
delay1 vec = 0 : vec

-- now custom delay which prepends n zeros --
delay :: Num a => [a] -> Int -> [a]
delay vec 0 = vec 
delay vec n = 0 : delay vec (n - 1) 

-- keeps only every dth sample --
downsample :: Num a => [a] -> Int -> [a]
downsample [] _ = []
downsample (x:xs) d = x : downsample (drop (d - 1) xs) d
