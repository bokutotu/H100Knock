{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module HaskellPractice.Problems.Basic (
    takeEvery,
    runningTotals,
    diffAdjacent,
    firstDuplicate,
    Trend (..),
    classifyTrend,
    uniquePreserve,
    fibonacci,
    groupByLength,
    topKFrequent,
    isBracketBalanced,
    groupByKey,
    lookupChain,
    longestStreak,
    maxSumWindow,
    wordRanking,
    NestedList (..),
    flattenNested,
    segmentsBy,
    TriangleKind (..),
    classifyTriangle,
    collectMonoid,
) where

import qualified Data.List   as L
import qualified Data.Map    as M
import           Data.Ord    (Down (Down), comparing)
import qualified Data.Set    as S
import           Debug.Trace (traceShow)

-- 問題1: 正の整数 step とリストを受け取り、先頭から step ごとに要素を抜き出す関数 takeEvery を実装せよ。
-- step が 1 以下の場合は空リストを返すこと。再帰または標準ライブラリのみを使ってよい。
-- 例: takeEvery 2 [1,2,3,4,5] == [1,3,5]
takeEvery :: Int -> [a] -> [a]
takeEvery step li
    | step <= 1 = []
    | otherwise = foldr (\(idx, itm) list -> if idx `mod` step == 0 then itm : list else list) [] $ zip [0 ..] li

-- 問題2: 数値リストの累積和を求める関数 runningTotals を実装せよ。
-- 結果の各要素は入力の先頭からその位置までの合計とすること。fold を用いた実装を推奨する。
-- 例: runningTotals [3,1,4] == [3,4,8]
runningTotals :: (Num a) => [a] -> [a]
runningTotals = tail . scanl (+) 0

-- 問題3: 隣接する要素同士の差を求める関数 diffAdjacent を実装せよ。
-- 結果リストの長さは入力より 1 だけ小さくなる。空リストや要素数 1 の場合は空リストを返すこと。
-- 例: diffAdjacent [5,2,9] == [-3,7]
diffAdjacent :: (Num a) => [a] -> [a]
diffAdjacent []  = []
diffAdjacent [x] = []
diffAdjacent li  = snd $ L.mapAccumL (\l r -> (r, r - l)) (head li) (tail li)

diffAdjacent' :: (Num a) => [a] -> [a]
diffAdjacent' = \case
    [] -> []
    xs@(_ : _) -> zipWith (-) xs (tail xs)

-- 問題4: リストの中で最初に再出現する要素を返す関数 firstDuplicate を実装せよ。
-- Set を用いて、既に見た要素を管理すること。重複がなければ Nothing を返すこと。
-- 例: firstDuplicate [1,4,2,4,3] == Just 4
firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = f S.empty
  where
    f s = \case
        [] -> Nothing
        (x : xs) -> if S.member x s then Just x else f (S.insert x s) xs

-- 問題5: 数列の傾向を判定する関数 classifyTrend を実装せよ。
-- 全てが昇順なら StrictlyIncreasing、降順なら StrictlyDecreasing、全て同じなら Constant、それ以外は Mixed とせよ。
-- 空リストは Constant とみなすこと。パターンガードや型クラス制約の活用を意識せよ。
-- 例: classifyTrend [1,2,3] == StrictlyIncreasing, classifyTrend [2,2,2] == Constant
data Trend
    = StrictlyIncreasing
    | StrictlyDecreasing
    | Constant
    | Mixed
    deriving (Eq, Show)

classifyTrend :: (Ord a) => [a] -> Trend
classifyTrend xs
    | isConstant xs = Constant
    | isIncrease xs = StrictlyIncreasing
    | isDecrease xs = StrictlyDecreasing
    | otherwise = Mixed
  where
    isConstant []       = True
    isConstant (x : xs) = all (== x) xs

    isIncrease :: (Ord a) => [a] -> Bool
    isIncrease []             = False
    isIncrease [x]            = True
    isIncrease (x0 : x1 : xs) = (x0 < x1) && isIncrease (x1 : xs)

    isDecrease :: (Ord a) => [a] -> Bool
    isDecrease = isIncrease . reverse

-- 問題6: リストから重複要素を取り除き、最初に出現した順序を保つ関数 uniquePreserve を実装せよ。
-- ヒント: 補助関数や再帰、fold など好きな方法でよい。空リストの場合は空リストを返すこと。
uniquePreserve :: (Eq a, Ord a) => [a] -> [a]
uniquePreserve = snd . foldl update (S.empty, [])
  where
    update (s, li) itm =
        if S.member itm s
            then (s, li)
            else (S.insert itm s, li ++ [itm])

-- 問題7: 非負整数 n を受け取り、n 番目のフィボナッチ数を返す関数 fibonacci を実装せよ。
-- ここでは fibonacci 0 == 0, fibonacci 1 == 1 と定義すること。負の入力はエラーとせよ。
fibonacci :: Integer -> Integer
fibonacci = \case
    n | n < 0 -> error "負はの値はダメ。とうわけであなたの†人権†没収ね"
    0 -> 0
    1 -> 1
    n -> fibonacci (n - 1) + fibonacci (n - 2)

-- 問題8: 文字列のリストを受け取り、長さごとにグループ化した結果を返す関数 groupByLength を実装せよ。
-- 出力は長さと、その長さを持つ文字列のリストのペアからなるリストとし、長さの昇順にソートされた状態にすること。
groupByLength :: [String] -> [(Int, [String])]
groupByLength = M.toList . foldl updateMap M.empty
  where
    updateMap :: M.Map Int [String] -> String -> M.Map Int [String]
    updateMap m x =
        case M.lookup (length x) m of
            Nothing -> M.insert (length x) [x] m
            Just xs -> M.update (const . Just $ xs ++ [x]) (length x) m

-- 問題9: 文字列のリストと正の整数 k を受け取り、出現回数が多い上位 k 個の文字列と回数のペアを返す関数 topKFrequent を実装せよ。
-- 出現回数が同じ場合は文字列の辞書順で昇順に並べること。k が 0 以下の場合は空リストを返すこと。
-- 例: topKFrequent 2 ["ha","ha","hs","ha","hs"] == [("ha",3),("hs",2)]
topKFrequent :: Int -> [String] -> [(String, Int)]
topKFrequent num = take num . L.sortBy (comparing (Down . snd)) . M.toList . foldl updateMap M.empty
  where
    updateMap m x =
        case M.lookup x m of
            Nothing -> M.insert x 1 m
            Just xs -> M.update (\x -> Just $ x + 1) x m

-- 問題10: 文字列に含まれる () [] {} の括弧が正しく対応づけられているかを判定する関数 isBracketBalanced を実装せよ。
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

-- 問題11: (キー, 値) のリストを受け取り、同じキーの値をまとめた Map を返す関数 groupByKey を実装せよ。
-- 例: groupByKey [("lang","hs"),("lang","rb"),("year","2024")] == fromList [("lang", ["hs","rb"]), ("year", ["2024"])]
groupByKey :: (Ord k) => [(k, v)] -> M.Map k [v]
groupByKey = foldl updateFn M.empty
  where
    updateFn m (k, v) = case M.lookup k m of
        Just list -> M.insert k (list ++ [v]) m
        Nothing   -> M.insert k [v] m

-- 問題12: 指定したキー列を Map から順に検索し、すべて見つかれば Right に値のリストを返す。
-- 途中で見つからないキーがあれば、単に "エラー" というメッセージで Left を返す関数 lookupChain を実装せよ。
lookupChain :: (Ord k, Show k) => [k] -> M.Map k v -> Either String [v]
lookupChain query map = foldl (lookupFn map) (Right []) query
  where
    lookupFn map (Left e) itm = Left e
    lookupFn map (Right li) itm = case M.lookup itm map of
        Nothing   -> Left "エラー"
        Just hoge -> Right $ li ++ [hoge]

-- 問題13: 同じ要素が連続する最長区間の長さを求める関数 longestStreak を実装せよ。
-- 例: longestStreak [1,1,2,2,2,3] == 3, longestStreak "aaabbbbcc" == 4
longestStreak :: (Eq a) => [a] -> Int
longestStreak [] = 0
longestStreak [x] = 1
longestStreak (x : xs) = let (_, _, best) = solve (x, 1, 1) xs in best
  where
    solve = foldl updateLength
    updateLength (x, now, maxLen) c
        | x == c = (x, now + 1, max (now + 1) maxLen)
        | otherwise = (c, 1, max now maxLen)

-- 問題14: 正の整数 k と整数リストを受け取り、長さ k の連続部分列のうち、要素和が最大になるものの和を返す関数 maxSumWindow を実装せよ。
-- 部分列が存在しない場合は Nothing を返すこと。k <= 0 のときも Nothing とする。
-- 例: maxSumWindow 3 [1,5,-2,4,3] == Just 7 (部分列 [5,-2,4] の和)
maxSumWindow :: Int -> [Int] -> Maybe Int
maxSumWindow step xs
    | step <= 0 || length xs < step = Nothing
    | otherwise =
        let cumulativeSum = tail $ scanl (+) 0 xs
            diff = zipWith (-) (drop step cumulativeSum) cumulativeSum
         in Just $ maximum diff

-- 問題15: 文字列のリストから単語頻度ランキングを作成する関数 wordRanking を実装せよ。
-- 出現回数が多い順にランクを付け、同順位の単語はまとめて辞書順で並べる。ランク番号は 1 から始まり、同率でも欠番を作らない (dense ranking)。
-- 例: wordRanking ["red","blue","red","green","blue","red"] == [(1,["red"]),(2,["blue"]),(3,["green"])]
wordRanking :: [String] -> [(Int, [String])]
wordRanking list =
    let itmFreq = M.fromListWith (+) $ map (,1) list
        freqItm = M.fromListWith (<>) $ map (\(itm, freq) -> (freq, [itm])) $ M.toList itmFreq
     in zipWith
            (curry (\(idx, (freq, itm)) -> (idx, L.sort itm)))
            [1 ..]
            (L.sortOn (Down . fst) $ M.toList freqItm)

-- in map (\(idx, (freq, itm)) -> (idx, L.sort itm)) $ zip [1 ..] $ L.sortOn (Down . fst) $ M.toList freqItm

wordRanking' :: [String] -> [(Int, [String])]
wordRanking' list =
    let intString = M.toList $ foldl createM M.empty list
        flipedL = M.toList $ foldl flipKV M.empty intString
     in rank flipedL
  where
    createM :: M.Map String Int -> String -> M.Map String Int
    createM map item =
        M.alter
            ( \case
                Just x -> Just $ x + 1
                Nothing -> Just 1
            )
            item
            map
    flipKV :: M.Map Int [String] -> (String, Int) -> M.Map Int [String]
    flipKV map (key, freq) =
        M.alter
            ( \case
                Just x -> Just $ L.sort $ key : x
                Nothing -> Just [key]
            )
            freq
            map
    rank :: [(Int, [String])] -> [(Int, [String])]
    rank list = foldr (\(idx, (freq, itm)) li -> (idx, itm) : li) [] $ zip [1 ..] $ L.sortBy (\x y -> compare (fst y) (fst x)) list

-- 問題16: ネストしたリスト構造 NestedList を平坦化する関数 flattenNested を実装せよ。
-- 例: flattenNested (Nested [Atom 1, Nested [Atom 2, Atom 3], Atom 4]) == [1,2,3,4]
data NestedList a = Atom a | Nested [NestedList a]
    deriving (Eq, Show)

flattenNested :: NestedList a -> [a]
flattenNested (Atom a)    = [a]
flattenNested (Nested xs) = concatMap flattenNested xs

flattenNested' :: NestedList a -> [a]
flattenNested' (Atom a)          = [a]
flattenNested' (Nested [])       = []
flattenNested' (Nested (x : xs)) = flattenNested x ++ flattenNested (Nested xs)

-- 問題17: 条件を満たす要素が連続する最大区間を列挙する関数 segmentsBy を実装せよ。
-- predicate を満たす要素が続く限り区間を伸ばし、条件を満たさない要素で区切る。空区間は結果に含めないこと。
-- 例: segmentsBy even [2,4,1,6,8,3,10] == [[2,4],[6,8],[10]]
segmentsBy :: (a -> Bool) -> [a] -> [[a]]
segmentsBy f [] = []
segmentsBy f x =
    case split x of
        ([], [])     -> []
        (x, [])      -> [x]
        ([], y : ys) -> segmentsBy f ys
        (x, y : ys)  -> x : segmentsBy f ys
  where
    split = L.span f

-- 問題18: 3 辺の長さから三角形の種類を分類する関数 classifyTriangle を実装せよ。
-- 0 以下の長さがあれば Nothing を返すこと。三角形の成立条件を満たさない場合も Nothing とする。
-- 成立する場合は、全て等しいなら Equilateral、2 辺のみ等しければ Isosceles、
-- どの 2 辺の和が残りの 1 辺と等しい場合は Degenerate、それ以外は Scalene と判定せよ。
-- 複数の条件を明示するためにパターンガードの利用を意識すること。
data TriangleKind
    = Equilateral
    | Isosceles
    | Scalene
    | Degenerate
    deriving (Eq, Show)

classifyTriangle :: (Ord a, Num a) => a -> a -> a -> Maybe TriangleKind
classifyTriangle a b c
    | a <= 0 || b <= 0 || c <= 0 = Nothing
    | a == b && b == c && c == a = Just Equilateral
    | isDegenerate a b c = Just Degenerate
    | a == b || b == c || c == a = Just Isosceles
    | not $ isValid a b c = Nothing
    | otherwise = Just Scalene
  where
    isValid x y z = x + y > z && y + z > x && z + x > y
    isDegenerate x y z = x + y == z || y + z == x || z + x == y

-- 問題19: Foldable な (キー, Monoid 値) の列を受け取り、同じキーの値を Monoid の演算でまとめる関数 collectMonoid を実装せよ。
-- Ord 制約と Monoid 制約の併用に慣れることを意識し、標準ライブラリのみで実装すること。
collectMonoid :: (Foldable t, Ord k, Monoid v) => t (k, v) -> M.Map k v
collectMonoid = foldl (\map (key, value) -> M.alter (update value) key map) M.empty
  where
    update value = \case
        Just old -> Just $ old <> value
        Nothing -> Just value
