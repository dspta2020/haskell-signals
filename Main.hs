module Main where

import SignalIO (print_real_tone)

-- sample rate to make the tone at
sample_rate_hz :: Double
sample_rate_hz = 5000 

-- tone duration_sec
duration_sec :: Double
duration_sec = 1

-- frequency to show up at 
tone_freq_hz :: Double
tone_freq_hz = 750

sample_count :: Int
sample_count = round (sample_rate_hz * duration_sec)

-- make the time vector should be a list of doubles 
-- here since n will show up as an int i need to cast it to a float using from integral
-- but since sample_rate is already a float don't need from integral 
time_vec_sec :: [Double]
time_vec_sec = [fromIntegral n / sample_rate_hz | n <- [0 .. sample_count - 1]]

-- A * cos(2 * pi * f * t)
-- double real_tone_at(f, t)
real_tone_at :: Double -> Double -> Double -> Double
real_tone_at frequency t amplitude = amplitude * cos (2 * pi * frequency * t)

samples :: [Double]
samples = [real_tone_at tone_freq_hz t 5.0 | t <- time_vec_sec]

main :: IO ()
main = print_real_tone 5 samples