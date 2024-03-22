-- Digit cancelling fractions like 49/98 = 4/8 even thugh crossing 9s isnt valid.

-- Very inelegant or statement to break it into cases, probably a better way to check each pair

digits d1 d2 = read (show d1 ++ show d2)::Int


solutions :: Int
solutions = (\[a,b]->let g = gcd a b in b `div` g) . 
  (\(p,q)->map product [p,q]) . unzip $ 
  map ((\(a,b) -> (a `div` gcd a b, b `div` gcd a b)) . 
  (\(d1,d2,d3,d4) -> (digits d1 d2, digits d3 d4))) 
    ([(d1,d2,d3,d4) | d1<-[1..9], d2<-[0..9], d3<-[1..9], d4<-[0..9], d2*d4/=0,
    (d1 == d4 && fromIntegral d2 / fromIntegral d3 == fromIntegral (digits d1 d2) / fromIntegral (digits d3 d4)) ||
    (d1 == d3 && fromIntegral d2 / fromIntegral d4 == fromIntegral (digits d1 d2) / fromIntegral (digits d3 d4))
    || (d2 == d4 && fromIntegral d1/ fromIntegral d3 == fromIntegral (digits d1 d2)/fromIntegral (digits d3 d4))
    || (d2 == d3 && fromIntegral d1 / fromIntegral d4 == fromIntegral (digits d1 d2)/fromIntegral (digits d3 d4)),
    digits d1 d2 < digits d3 d4])

main = do print solutions