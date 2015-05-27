module Effect.Simplest

import Effects
import Effect.State
import Effect.StdIO

effinc : { [STATE Int] } Eff Int
effinc = do i' <- get
            put (i' + 1)
            pure !get

flow : { [STDIO, STATE Int] } Eff ()
flow = do putStrLn "I begin working..."
          putStrLn $ cast 10
          -- use one of two following strings
          -- first one leads to result of 1
          -- because of default int value 0
          -- second one leads to result of 2
--          i <- effinc
          let i = runPureInit [1] effinc
          putStrLn $ cast i
          putStrLn "I've done with work"
          
main : IO ()
main = run flow
