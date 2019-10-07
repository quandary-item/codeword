module TreeSearch where

data Result a = Solution a | NoSolution | Branches [a] deriving Show

seqMaybe :: [Maybe a] -> Maybe a
seqMaybe []            = Nothing
seqMaybe (Nothing:xs)  = seqMaybe xs
seqMaybe ((Just a):xs) = Just a

treeSearch :: (a -> Result a) -> a -> Maybe a
treeSearch step cnf = case step cnf of
  Solution cnf' -> Just cnf'
  NoSolution    -> Nothing
  Branches branches -> seqMaybe $ map (treeSearch step) branches

treeSearchN :: (a -> Result a) -> Int -> a -> Maybe a
treeSearchN step n cnf
  | n == 0    = Nothing
  | otherwise = case step cnf of
      Solution cnf' -> Just cnf'
      NoSolution    -> Nothing
      Branches branches -> seqMaybe $ map (treeSearchN step $ n - 1) branches
