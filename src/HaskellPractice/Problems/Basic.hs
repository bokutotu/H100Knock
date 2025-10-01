{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}

module HaskellPractice.Problems.Basic (
    uniquePreserve,
    fibonacci,
    groupByLength,
    topKFrequent,
    isBracketBalanced,
    groupByKey,
    lookupChain,
    longestStreak,
) where

import qualified Data.List as L
import qualified Data.Map  as M
import           Data.Ord  (Down (Down), comparing)
import qualified Data.Set  as S

-- 問題1: リストから重複要素を取り除き、最初に出現した順序を保つ関数 uniquePreserve を実装せよ。
-- ヒント: 補助関数や再帰、fold など好きな方法でよい。空リストの場合は空リストを返すこと。
uniquePreserve :: (Eq a, Ord a) => [a] -> [a]
uniquePreserve = snd . foldl update (S.empty, [])
  where
    update (s, li) itm =
        if S.member itm s
            then (s, li)
            else (S.insert itm s, li ++ [itm])

-- 問題2: 非負整数 n を受け取り、n 番目のフィボナッチ数を返す関数 fibonacci を実装せよ。
-- ここでは fibonacci 0 == 0, fibonacci 1 == 1 と定義すること。負の入力はエラーとせよ。
fibonacci :: Integer -> Integer
fibonacci = \case
    n | n < 0 -> error "負はの値はダメ。とうわけであなたの†人権†没収ね"
    0 -> 0
    1 -> 1
    n -> fibonacci (n - 1) + fibonacci (n - 2)

-- 問題3: 文字列のリストを受け取り、長さごとにグループ化した結果を返す関数 groupByLength を実装せよ。
-- 出力は長さと、その長さを持つ文字列のリストのペアからなるリストとし、長さの昇順にソートされた状態にすること。
groupByLength :: [String] -> [(Int, [String])]
groupByLength = M.toList . foldl updateMap M.empty
  where
    updateMap :: M.Map Int [String] -> String -> M.Map Int [String]
    updateMap m x =
        case M.lookup (length x) m of
            Nothing -> M.insert (length x) [x] m
            Just xs -> M.update (const . Just $ xs ++ [x]) (length x) m

-- 問題4: 文字列のリストと正の整数 k を受け取り、出現回数が多い上位 k 個の文字列と回数のペアを返す関数 topKFrequent を実装せよ。
-- 出現回数が同じ場合は文字列の辞書順で昇順に並べること。k が 0 以下の場合は空リストを返すこと。
-- 例: topKFrequent 2 ["ha","ha","hs","ha","hs"] == [("ha",3),("hs",2)]
topKFrequent :: Int -> [String] -> [(String, Int)]
topKFrequent num = take num . L.sortBy (comparing (Down . snd)) . M.toList . foldl updateMap M.empty
  where
    updateMap m x =
        case M.lookup x m of
            Nothing -> M.insert x 1 m
            Just xs -> M.update (\x -> Just $ x + 1) x m

-- 問題5: 文字列に含まれる () [] {} の括弧が正しく対応づけられているかを判定する関数 isBracketBalanced を実装せよ。
-- 途中で対応しない閉じ括弧が現れた場合は False を返すこと。括弧以外の文字は無視してよいものとする。
-- 例: isBracketBalanced "([{}])" == True, isBracketBalanced "([)]" == False
isBracketBalanced :: String -> Bool
isBracketBalanced = isFine . foldl solve initState

data BracketState = BracketState {isFine :: Bool, stack :: String}

initState :: BracketState
initState = BracketState True ""

solve :: BracketState -> Char -> BracketState
solve state@BracketState{..} char
    | not isFine = state
    | otherwise = case char of
        open | open `elem` "{[("              -> state{stack = char : stack}
        close | expected <- expectedChar char -> pop expected
  where
    expectedChar = \case
        ')' -> '('
        '}' -> '{'
        ']' -> '['
    pop expected = case stack of
        s : rest | s == expected -> state{stack = rest}
        _                        -> state{isFine = False}

-- 問題6: (キー, 値) のリストを受け取り、同じキーの値をまとめた Map を返す関数 groupByKey を実装せよ。
-- 例: groupByKey [("lang","hs"),("lang","rb"),("year","2024")] == fromList [("lang", ["hs","rb"]), ("year", ["2024"])]
groupByKey :: (Ord k) => [(k, v)] -> M.Map k [v]
groupByKey = foldl updateFn M.empty
  where
    updateFn m (k, v) = case M.lookup k m of
        Just list -> M.insert k (list ++ [v]) m
        Nothing   -> M.insert k [v] m

-- 問題7: 指定したキー列を Map から順に検索し、すべて見つかれば Right に値のリストを返す。
-- 途中で見つからないキーがあれば、単に "エラー" というメッセージで Left を返す関数 lookupChain を実装せよ。
lookupChain :: (Ord k, Show k) => [k] -> M.Map k v -> Either String [v]
lookupChain query map = foldl (lookupFn map) (Right []) query
  where
    lookupFn map (Left e) itm = Left e
    lookupFn map (Right li) itm = case M.lookup itm map of
        Nothing   -> Left "エラー"
        Just hoge -> Right $ li ++ [hoge]

-- 問題8: 同じ要素が連続する最長区間の長さを求める関数 longestStreak を実装せよ。
-- 例: longestStreak [1,1,2,2,2,3] == 3, longestStreak \"aaabbbbcc\" == 4
longestStreak :: Eq a => [a] -> Int
longestStreak = error "TODO: longestStreak"
