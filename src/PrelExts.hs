module PrelExts(splitAtRev,swapAt,swapAtRev,unInterleave,zipShorterWithRest,log2Int,pupd1,prod) where

pupd :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
pupd f g (x,y) = (f x, g y)
-- pup2d :: (t1 -> t2 -> a) -> (t3 -> t4 -> b) -> (t1, t3) -> (t2, t4) -> (a, b)
-- pup2d f g (x1,x2) (y1,y2) = (f x1 y1, g x2 y2)
-- pup2dD :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
-- pup2dD f x y = pup2d f f x y
-- pupdD :: (t -> b) -> (t, t) -> (b, b)
-- pupdD f = pupd f f
prod :: (t1 -> a) -> (t2 -> b) -> (t1, t2) -> (a, b)
prod = pupd
pupd1 :: (t -> a) -> (t, b) -> (a, b)
pupd1 f (x,y) = (f x, y)
-- pupd2 :: (t -> b) -> (a, t) -> (a, b)
-- pupd2 g (x,y) = (x, g y)
-- pupt :: (t1 -> a) -> (t2 -> b) -> (t3 -> c) -> (t1, t2, t3) -> (a, b, c)
-- pupt f g h (x,y,z) = (f x, g y, h z)
-- pupt1 :: (t -> a) -> (t, b, c) -> (a, b, c)
-- pupt1 f (x,y,z) = (f x, y, z)
-- pupt2 :: (t -> b) -> (a, t, c) -> (a, b, c)
-- pupt2 g (x,y,z) = (x, g y, z)
-- pupt3 :: (t -> c) -> (a, b, t) -> (a, b, c)
-- pupt3 h (x,y,z) = (x, y, h z)
-- pupq :: (t1 -> a) -> (t2 -> b) -> (t3 -> c) -> (t4 -> d) -> (t1, t2, t3, t4) -> (a, b, c, d)
-- pupq f g h i (x,y,z,w) = (f x, g y, h z, i w)
-- pupdd :: (t1 -> a1) -> (t2 -> b1) -> (t3 -> a2) -> (t4 -> b2) -> ((t1, t2), (t3, t4)) -> ((a1, b1), (a2, b2))
-- pupdd f g h i ((x,y),(z,w)) = ((f x, g y), (h z, i w))

-- padLeftTo :: Int -> a -> [a] -> [a]
-- padLeftTo k x xs = replicate (k - length xs) x ++ xs

splitAtRev :: Int -> [a] -> ([a], [a])
splitAtRev k = pupd reverse reverse . splitAt k . reverse

swapAt, swapAtRev :: Int -> [a] -> [a]
swapAt     k xs =  flip (++) `uncurry` splitAt     k xs
swapAtRev  k xs =  (++) `uncurry` splitAtRev  k xs

-- Split a list as if it had been formed by interleaving two lists of \emph{equal} length.
unInterleave :: [a] -> ([a], [a])
unInterleave = h id id
 where
  h  cxs  cys  []            = (cxs [], cys [])
  h  cxs  cys  (x : y : zs)  = h (cxs . (x :)) (cys . (y :)) zs
  h  _    _    _             = error "PrelExt.unInterleave"

zipShorterWithRest :: [a] -> [b] -> ([(a,b)], [b])
zipShorterWithRest = h id
  where
    h acc [] ys = (acc [], ys)
    h acc xs [] = (acc $ error msg, [])
        where msg = "zipShorterWithRest: " ++ show (length xs)
    h acc (x:xs) (y:ys) = h (acc . ((x, y) :)) xs ys

-- There was a fast version of this on the Haskell mailing list, but we don't need a fast version.
log2Int :: (Integral t, Num p) => t -> p
log2Int n = if n2 == 0 then 0 else 1 + (log2Int n2)
  where
    n2 = div n 2
