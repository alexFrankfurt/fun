module Tests

-- init : (s : String) -> {auto ok : isCons (unpack s) = True } -> String
-- init s = pack $ init $ unpack s

data Word = Foo | Bar

total
f : Word -> Bool
f Foo = True
f Bar = False
