
module StringAlignment where

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

s1 = "writers"
s2 = "vintner"

-- Score the alignment of two characters
score :: Char -> Char -> Int
score x '-' = scoreSpace
score '-' y = scoreSpace
score x y
    | x == y = scoreMatch
    | otherwise = scoreMismatch

-- Score the alignment of two strings
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] _ = scoreSpace
similarityScore _ [] = scoreSpace
similarityScore (x:xs) (y:ys) = maximum [ similarityScore xs ys + score x y,
                                     similarityScore xs (y:ys) + score x '-',
                                     similarityScore (x:xs) ys + score '-' y]


--with look-up table
simScoreBetter :: String -> String -> Int
simScoreBetter xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ entry i j | j<-[0..]] | i<-[0..] ]

    entry :: Int -> Int -> Int
    entry 0 0 = 0
    entry i 0 = i * scoreSpace
    entry 0 j = j * scoreSpace
    entry i j = maximum [simScore (i - 1) (j - 1) + score x y,
                        simScore (i - 1)  j + score x '-',
                        simScore  i (j - 1) + score '-' y]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)



-- append h1 to the first element of all the tuples and h2 to the second
-- element of all the tuples
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]
--maximaBy f xs = [x | x <- xs, f x == maximum (map f xs)]

type AlignmentType = (String, String)


optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] =[([],[])]
optAlignments (x:xs) []     = attachHeads x '-' $ optAlignments xs []
optAlignments []     (y:ys) = attachHeads '-' y $ optAlignments [] ys
optAlignments (x:xs) (y:ys) = maximaBy sim alignments
    where
        sim (a,b) = simScoreBetter a b
        alignments = concat [ attachHeads x y   $ optAlignments xs ys,
                              attachHeads x '-' $ optAlignments xs (y:ys),
                              attachHeads '-' y $ optAlignments (x:xs) ys ]


optAlignmentsMemo :: String -> String -> [AlignmentType]
optAlignmentsMemo xs ys = map
                          (pairApply reverse)
                          (snd $ alignment (length xs) (length ys))
    where
        pairApply f (a, b) = (f a, f b)

        alignment :: Int -> Int -> (Int, [AlignmentType])
        alignment i j = table !! i !! j

        table :: [[(Int, [AlignmentType])]]
        table = [[entry i j | j <- [0..]] | i <- [0..]]

        entry :: Int -> Int -> (Int, [AlignmentType])
        entry 0 0 = (0, [([], [])])
        entry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')])
        entry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
        entry i j = (fst $ head best, concat [snd b | b <- best])
            where
                (s1, a1) = alignment (i - 1) (j - 1)
                (s2, a2) = alignment (i - 1)  j
                (s3, a3) = alignment  i      (j - 1)
                x = xs !! (i - 1)
                y = ys !! (j - 1)
                best = maximaBy fst $ [
                    (s1 + score x y,   attachHeads x y   a1),
                    (s2 + score x '-', attachHeads x '-' a2),
                    (s3 + score '-' y, attachHeads '-' y a3)]


outputOptAlignments2 :: String -> String -> IO ()
outputOptAlignments2 string1 string2 = do
  mapM_ (\(a,b) -> putStrLn (spaces a ++ "\n" ++ spaces b ++ "\n")) $ al
  putStrLn $ "There are " ++ show (length al) ++ " optimal alignments: \n"
     where
         al = optAlignments3 string1 string2


test1 = outputOptAlignments s1 s2
