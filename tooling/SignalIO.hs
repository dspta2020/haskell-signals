-- saving this code in a "module" makes it locally distributable
-- here i explicitly provided an export list that only exposes `print_real_tone`
-- and the real samples loop is only scoped locally to this file
module SignalIO (print_real_tone) where

-- guards are boolean checks while cases match patterns
print_real_tone :: Int -> [Double] -> IO ()
print_real_tone n samples                     -- function signature
  | n <= 0 = return ()                      -- if num samples to print is 0 return
  | otherwise =                             -- else do something
      case samples of
        [] -> return ()                     -- if empty list return
        (_:_)  -> do                        -- if non-empty list print the header then start printing samples
          putStrLn "\nPRINTING SAMPLES:"
          print_real_samples_loop ([0 .. (n - 1)]) (take n samples) -- take ensures the loop takes no more than n samples

print_real_samples_loop :: [Int] -> [Double] -> IO ()
print_real_samples_loop _ [] = return ()
print_real_samples_loop (index:ind_rest) (sample:sample_rest) = do     -- now just print out the samples
  putStrLn ("Sample " ++ show index ++ ": " ++ show sample) 
  print_real_samples_loop ind_rest sample_rest