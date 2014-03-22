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

-- 1..9 を与えて行を取り出す。
row :: Board -> Int -> String
row b n = b !! (n - 1)

-- 1..9 を与えて列を取り出す。
col :: Board -> Int -> String
col b n = map (!!(n - 1)) b

-- 1..9 を与えて 3x3 の領域を取り出す。左上から右順で、順次下に。
box :: Board -> Int -> String
box b n = let (q,r) = divMod (n-1) 3
     in concatMap (take 3.drop (r*3).row b) [x+q*3|x<-[1..3]]

-- 行、列、あるいは 3x3 領域の 9 文字中に重複する数字がないことを確認する。
valid' :: String -> Bool
valid' = null.(\\ "123456789").filter (`elem` "123456789")

-- 盤面上の行、列、 3x3 領域のいずれにも重複数字がないことを確認する。
valid :: Board -> Bool
valid b = all valid'.concat.map (flip map [1..9].($ b)) $ [box, row, col]

-- 盤面に空白が残っていないことを確認する。
noSpace :: Board -> Bool
noSpace = (==Nothing).(find (==' ')).concat

-- 盤面上の最左上の空白を 1..9 の数字で置き換えた盤面列を得る。
nexts :: Board -> [Board]
nexts b = let cs = concat b; p = findIndex (==' ') cs  in case p of
  Nothing -> []
  Just n  -> let (h, t) = splitAt n cs
    in [slice 9 (h ++ c:tail t) | c <- "123456789"]

-- List を与えた長さ n の部分列に分割し、この部分列の List を返す。
slice :: Int -> [a] -> [[a]]
slice n = let f xs = if null xs then mzero else (return.splitAt n) xs
  in unfoldr f

-- 数独を解く。
solve :: (MonadPlus m) => Board -> m Board
solve b
      |noSpace b = if valid b then return b else mzero
      |otherwise = msum [solve b' |b' <- nexts b, valid b']
