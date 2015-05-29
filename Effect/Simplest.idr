module Effect.Simplest

import Effects
import Effect.State
import Effect.StdIO

-- analog of `effinc` but with labeled state
lbldeff : { ['Lbl ::: STATE Int] } Eff Int
lbldeff = do x <- 'Lbl :- get
             'Lbl :- put (x + 1)
             pure !('Lbl :- get)

effinc : { [STATE Int] } Eff Int
effinc = do i' <- get
            put (i' + 1)
            pure !get



-- readInt : { [STATE (Vect n Int), STDIO] ==>
--             {ok} if ok then [STATE (Vect (S n) Int), STDIO]
--                        else [STATE (Vect n Int), STDIO] } Eff Bool
-- readInt = do let x = trim !getStr
--              case all isDigit (unpack x) of
--                   False => pure False
--                   True  => do putM (cast x :: !get)
--                               pure True



-- appends to a vector if given word begins with 'a'
-- todo
-- vectorStore : String -> 
--               { [STATE (Vect n String)] ==>
--               {ok} if ok then [STATE (Vect (S n) String)]
--                          else [STATE (Vect n String)] } Eff Bool
-- vectorStore s = do case strHead s of
--                         'a' => do putM (s :: !get)
--                                   pure True
--                         _   => pure False
                   




flow : { [STDIO, STATE Int] } Eff ()
flow = do putStrLn "I begin working..."
          putStrLn $ cast 10
          -- use one of two following strings
          -- first one leads to result of 1
          -- because of default int value 0
          -- second one leads to result of 2
--          i <- effinc
--          let i = runPureInit [1] effinc
          let i = runPureInit ['Lbl := 10] lbldeff
          putStrLn $ cast i
          putStrLn "I've done with work"
          
main : IO ()
main = run flow
