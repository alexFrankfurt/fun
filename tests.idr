module Tests

init : (s : String) -> {auto ok : isCons (unpack s) = True } -> String
init s = pack $ init $ unpack s
