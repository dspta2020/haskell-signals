module Main where

import Text.Printf (printf)
import Data.Complex

-- An expression describing how bytes map to bits --
byteToBits :: Int -> [Int]
byteToBits byteIn = 
  let byte = max 0 (min 255 byteIn) -- clamp the values to prevent over/underflow 
  in [(byte `div` (2 ^ i)) `mod` 2 | i <- reverse [0 .. 7]]

-- how a vector of bytes maps to a vector of bits -- 
bytesToBitsConverter :: [Int] -> [Int]
bytesToBitsConverter bytesIn = concatMap byteToBits bytesIn

-- An expression describing how bits map to antipodal data -- 
bitToAntipodal :: Int -> Int
bitToAntipodal bitIn = bitIn * 2 - 1

-- how a vector of bits maps to a vector of antipodal data
bitsToAntipodalConverter :: [Int] -> [Int]
bitsToAntipodalConverter bitsIn = map bitToAntipodal bitsIn

-- how to map a vector of bits to a vector of upsampled bits -- 
-- this returns a function and since 
upsampleBy :: Int -> ([a] -> [a])
upsampleBy up = concatMap (replicate up) 

-- rotates a sample by a give phase step --
rotateSampleBy :: Double -> Int -> Double
rotateSampleBy phaseIncr antipodalIn = phaseIncr * fromIntegral antipodalIn

-- maps a vector of antipodal data points to a vector of rotating phase
applyPhaseRotation :: Double -> [Int] -> [Double]
applyPhaseRotation phaseIncr antipodalIn = map (rotateSampleBy phaseIncr) antipodalIn

-- accumulate phase uses helper function `accumulate`
phaseAccumulator :: [Double] -> [Double]
phaseAccumulator samplesIn = 0 : accumulate 0 samplesIn
  -- Haskell design pattern are such that where blocks fit well for helper functions
  where
    -- in the base case an empty list comes in just return an empty list which denotes the 
    -- end of the linked list object so this will terminate accumulatePhase's running list
    accumulate _   []     = []
    accumulate acc (x:xs) =
      -- let values are locally scoped so it is good for the accumulator which does not need to 
      -- be returned or input as an argument 
      let total = acc + x
      -- here the next linked list node is built with total until the end a node of [] is 
      -- added and that terminates the list.
      in total : accumulate total xs

-- prints the samples in order
printSamples :: [Complex Double] -> IO ()
printSamples [] = return ()
printSamples (x : xs) = do
  printf "% .3f + (% .3fi)\n" (realPart x) (imagPart x)
  printSamples xs

main :: IO ()
main = do

  -- printing out to the console first
  printSamples iqComplex
  printf "Total bytes in: %d\n" nBytesFrom
  printf "Total samples expected: %d\n" (nBytesFrom * 8 * upsampleFactor)
  printf "Total samples calculated: %d\n" (length iqComplex)
  putStrLn  "Done"

  where 
    bytesIn = [128, 255, 1, 5, 7, 32, 6, 193] :: [Int]    -- test bytes in 
    upsampleFactor = 24                                   -- ends up as `u` samples per bit
    sampleRateHz = 24                                     -- 1 Hz bit rate 
    deltaFreqHz = 5                                       -- 5 Hz sample deviation
    channelCenterHz = 5                                   -- Offset 5 Hz channel so '0' bits come out at DC and '1' bits come out at 10 Hz
    phaseIncrement = 2 * pi * deltaFreqHz / sampleRateHz  
    nBytesFrom = 1

    -- configure the upsampler and phase rotations
    upsampler = upsampleBy upsampleFactor
    phaseRotator = applyPhaseRotation phaseIncrement
    exponentiation = map cis 

    -- basically compose all the functions into a single function that maps from bytes to a vector of accumulating phase
    bytesToSamplesPipeline = exponentiation . phaseAccumulator . phaseRotator . upsampler . bitsToAntipodalConverter . bytesToBitsConverter

    -- run the pipeline comes out as complex 
    iqComplex =  bytesToSamplesPipeline (take nBytesFrom bytesIn)