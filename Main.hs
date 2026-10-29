module Main where

import SignalIO (print_real_tone)
import SignalGen (sine_wave_samples)

-- define the amplitude
amplitude :: Double
amplitude = 5

-- output synthesis frequency 
freq :: Double
freq = 4

-- sample rate Hz
sample_rate :: Double
sample_rate = 20

main :: IO ()
main = do 
  -- output
  print_real_tone 30 (take 10 (sine_wave_samples sample_rate freq amplitude))

  