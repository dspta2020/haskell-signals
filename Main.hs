module Main where

import SignalIO (print_real_tone)
import SignalGen (samples)


main :: IO ()
main = print_real_tone 5 samples