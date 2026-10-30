module SignalGen (sine_wave_samples, complex_exponential_samples) where

-- for complex samples
import Data.Complex 

-- Okay so this is basically an imperative way to do this .....
-- it basically builds a time list and then maps over it which is not terrible but not efficient 
--    also its not great having that `n / fs` for every sample.

{-
    -- computes the total number of samples
    sample_count :: Double -> Double -> Int
    sample_count sample_rate duration = round (sample_rate * duration)

    -- make the time vector should be a list of doubles 
    -- here since n will show up as an int i need to cast it to a float using from integral
    -- but since sample_rate is already a float don't need from integral 
    time_vec_sec :: Double -> Double -> [Double]
    time_vec_sec sample_rate duration = [fromIntegral n / sample_rate | n <- [0 .. (sample_count sample_rate duration) - 1]]

    -- A * cos(2 * pi * f * t)
    -- double real_tone_at(f, t)
    real_tone_at :: Double -> Double -> Double -> Double
    real_tone_at frequency t amplitude = amplitude * cos (2 * pi * frequency * t)

    samples :: Double -> Double -> Double -> Double -> [Double]
    samples sample_rate duration frequency amplitude= [real_tone_at frequency t amplitude | t <- (time_vec_sec sample_rate duration)]
-}

-- Lets make this definition a little Haskell-ish and more up to a coding standard
sine_wave_samples 
  :: Double   -- sample rate in hz
  -> Double   -- frequency in hz
  -> Double   -- amplitude 
  -> [Double] -- output sample vector
sine_wave_samples fs f a = 
  [sample i | i <- [0 ..]] -- infinite list that is lazy evaluated.. can use `take` to get data from it has a weird feel to it for sure
  where
    phase_step = 2 * pi * f / fs
    sample i = a * cos (phase_step * fromIntegral i)


complex_exponential_samples
  :: Double   -- sample rate in hz
  -> Double   -- frequency in hz
  -> Double   -- amplitude 
  -> [Complex Double]
complex_exponential_samples fs f a = 
  [sample i | i <- [0 ..]] -- infinite list that is lazy evaluated.. can use `take` to get data from it has a weird feel to it for sure
  where
    phase_step = 2 * pi * f / fs

    -- here is where the data type needs to be promoted
    sample :: Int -> Complex Double
    sample i = (a :+ 0) * cis (phase_step * fromIntegral i)
