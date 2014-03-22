-- NumberPlace.hs
module NumberPlace(q0, solve) where

import Data.List
import Data.Maybe
import Control.Monad

type Board = [String] -- 9 文字ごとに 9 要素で数独を表現。

q0 :: Board
q0 = [
 " 8    15 ",
 "4 65 9 8 ",
 "     8   ",
 "         ",
 "  2 4   3",
 "3  8 1   ",
 "9   7    ",
 "6       4",
 "15     9 "
 ]

-- 1 .. 9 を与えて行を取り出す
row :: Board -> Int -> String
row b n = b !! (n - 1)

-- 1 .. 9 を与えて列を取り出す
col :: Board -> Int -> String
col b n = map (!!(n - 1)) b

-- 1 .. 9 を与えて 3 x 3 の領域を取り出す。左上から右順で、順次下に。
box :: Board -> Int -> String
{--
box b 1 = concat $ map (take 3.drop 0.row b) [1..3]
box b 2 = concat $ map (take 3.drop 3.row b) [1..3]
box b 3 = concat $ map (take 3.drop 6.row b) [1..3]
box b 4 = concat $ map (take 3.drop 0.row b) [4..6]
box b 5 = concat $ map (take 3.drop 3.row b) [4..6]
box b 6 = concat $ map (take 3.drop 6.row b) [4..6]
box b 7 = concat $ map (take 3.drop 0.row b) [7..9]
box b 8 = concat $ map (take 3.drop 3.row b) [7..9]
box b 9 = concat $ map (take 3.drop 6.row b) [7..9]
--}
box b n = let (q,r) = divMod (n-1) 3
     in concatMap (take 3.drop (r*3).row b) [x+q*3|x<-[1..3]]

valid' :: String -> Bool
valid' = null.(\\ "123456789").filter (`elem` "123456789")

valid :: Board -> Bool
valid b = all valid'.concat.map (flip map [1..9].($ b)) $ [box, row, col]

findSp :: Board -> Maybe (Int, Int)
findSp b = let i = findIndex (==' ') (concat b)  in case i of
  Nothing -> Nothing
  Just i -> let (r,c) = divMod i 9 in Just (r+1, c+1)

nexts :: Board -> [Board]
nexts b = let pos = findSp b
  in case pos of
    Nothing -> []
    Just (r, c) -> let (bh, bt) = splitAt (r-1) b
                       (rh, rt) = splitAt (c-1) (row b r)
                       rows = map (\c -> rh ++ c:tail rt) "123456789"
                   in map (\r -> bh ++ r:tail bt) rows

solve :: (MonadPlus m) => Board -> m Board
solve b
      |findSp b == Nothing = if valid b then return b else mzero
      |otherwise = msum [solve b' |b' <- nexts b, valid b']
