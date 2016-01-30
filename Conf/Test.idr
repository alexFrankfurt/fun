module Conf.Test

import Conf.Parser.Core

test : List $ Result String Char
test = [parserApply (char 'x') "x this is",
        parserApply item       "another env"]

run : IO ()
run = do print test
