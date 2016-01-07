module Tuple where

mapFst : (a -> c) -> (a, b) -> (c, b)
mapFst fn (fst, snd) =
  ((fn fst), snd)

mapSnd : (b -> c) -> (a, b) -> (a, c)
mapSnd fn (fst, snd) =
  (fst, (fn snd))
  
mapAll : (a -> b) -> (a, a) -> (b, b)
mapAll fn (fst, snd) =
  (fn fst, fn snd)

map : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
map fn fn2 (fst, snd) =
  (fn fst, fn2 snd)