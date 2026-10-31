module Main where

import Data.Complex (Complex)

import SignalIO (print_complex_tone, print_complex_to_from)
import SignalGen (complex_exponential_samples)
-- import SignalIO (print_real_tone)
-- import SignalGen (sine_wave_samples)

-- define the amplitude
amplitude :: Double
amplitude = 5

-- output synthesis frequency 
tone_freq_hz :: Double
tone_freq_hz = 4

-- sample rate Hz
fs :: Double
fs = 20

-- make the iq data here 
samples :: [Complex Double]
samples = complex_exponential_samples fs tone_freq_hz amplitude

-- Okay going to start making my waveform class here
data Waveform = Waveform
  {
    sample_rate_hz :: Double,
    center_frequency_hz :: Double,
    iq_complex :: [Complex Double]
  }

main :: IO ()
main = do 
  
  -- output
  -- print_real_tone 30 (take 10 (sine_wave_samples fs tone_freq_hz amplitude))
  -- print_complex_tone 30 (take 10 (complex_exponential_samples fs tone_freq_hz amplitude))
  -- print_complex_to_from 50 10 (take 1000 (complex_exponential_samples fs tone_freq_hz amplitude))

  -- make some IQ data here
  let wf :: Waveform
      wf = Waveform fs 0 samples
  print_complex_tone 5000 (iq_complex wf)

  