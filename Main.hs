module Main where

import FSKMod (fskModulator, FSKConfig(..), printSamples)

main :: IO ()
main = do 
  -- print samples 
  printSamples iqComplex
  
  where 
    bytesIn = [128, 255, 1, 5, 7, 32, 6, 193]             -- test bytes in 
    upsampleFactor = 24                                   -- ends up as `u` samples per bit
    sampleRateHz = 24                                     -- 1 Hz bit rate 
    deltaFreqHz = 5                                       -- 5 Hz sample deviation
    channelCenterHz = 5                                   -- Offset 5 Hz channel so '0' bits come out at DC and '1' bits come out at 10 Hz
    phaseIncrement = 2 * pi * deltaFreqHz / sampleRateHz  

    -- setup the modulator configs
    configs = FSKConfig upsampleFactor phaseIncrement
    
    -- call the modulator 
    iqComplex = fskModulator configs bytesIn