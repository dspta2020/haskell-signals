module SignalGen (samples) where

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
