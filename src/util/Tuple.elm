module Tuple where

mapFst : (a -> c) -> (a, b) -> (c, b)
mapFst fn (fst, snd) =
  ((fn fst), snd)
  
mapSnd : (b -> c) -> (a, b) -> (a, c)
mapSnd fn (fst, snd) =
  (fst, (fn snd))