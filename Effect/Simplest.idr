module Effect.Simplest

import Data.Vect
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

-- appends to a vector if given word begins with 'a'
-- todo
vectorStore : String ->
              with DepEff Eff Bool [STATE (Vect n String)]
              (\ok => if ok then [STATE (Vect (S n) String)]
                            else [STATE (Vect n String)])
vectorStore s = do case strHead s of
                        'a' => do putM (s :: !get)
                                  pureM True
                        _   => pureM False

-- make flow:
--   user enters string
--   we may be prepend it to a vector
--   user can 'ask' us for stored strings with "ask" word
--   and terminate session with "bye" word
-- vecFlow : { [STATE (Vect n String), STDIO] ==>
--               {ok} if ok then [STATE (Vect (S n) String), STDIO]
--                          else [STATE (Vect n String), STDIO ] } Eff ()
-- vecFlow = do put []
--              vectorStore !getStr
--              vecFlow


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
main = do putStrLn "Enter word, begining with a:"
          -- word <- getStr
          -- putStrLn word
