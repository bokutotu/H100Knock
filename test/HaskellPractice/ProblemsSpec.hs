module HaskellPractice.ProblemsSpec (spec) where

import           Data.Monoid        (Any (..), Sum (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import           Test.Hspec

import           HaskellPractice.Problems.Basic

spec :: Spec
spec = do
    describe "問題1: 正の整数 step ごとに要素を抜き出す関数 takeEvery を実装せよ。" $ do
        it "例: takeEvery 2 [1,2,3,4,5,6] は [1,3,5] を返す" $
            takeEvery 2 [1, 2, 3, 4, 5, 6 :: Int] `shouldBe` [1, 3, 5]
        it "例: takeEvery 3 \"haskell\" は \"hkl\" を返す" $
            takeEvery 3 "haskell" `shouldBe` "hkl"
        it "例: takeEvery 0 [1,2,3] は [] を返す" $
            takeEvery 0 [1, 2, 3 :: Int] `shouldBe` []

    describe "問題2: 累積和を返す関数 runningTotals を実装せよ。" $ do
        it "例: runningTotals [3,1,4] は [3,4,8] を返す" $
            runningTotals [3, 1, 4 :: Int] `shouldBe` [3, 4, 8]
        it "例: runningTotals [] は [] を返す" $
            runningTotals ([] :: [Int]) `shouldBe` []
        it "例: runningTotals [-1,2,-3,4] は [-1,1,-2,2] を返す" $
            runningTotals [-1, 2, -3, 4 :: Int] `shouldBe` [-1, 1, -2, 2]

    describe "問題3: 隣接要素の差を列挙する関数 diffAdjacent を実装せよ。" $ do
        it "例: diffAdjacent [5,2,9] は [-3,7] を返す" $
            diffAdjacent [5, 2, 9 :: Int] `shouldBe` [-3, 7]
        it "例: diffAdjacent [1] は [] を返す" $
            diffAdjacent [1 :: Int] `shouldBe` []
        it "例: diffAdjacent [] は [] を返す" $
            diffAdjacent ([] :: [Int]) `shouldBe` []

    describe "問題4: 最初に再出現する要素を返す関数 firstDuplicate を実装せよ。" $ do
        it "例: firstDuplicate [1,4,2,4,3] は Just 4 を返す" $
            firstDuplicate [1, 4, 2, 4, 3 :: Int] `shouldBe` Just 4
        it "例: firstDuplicate \"swim\" は Nothing を返す" $
            firstDuplicate "swim" `shouldBe` Nothing
        it "例: firstDuplicate [] は Nothing を返す" $
            firstDuplicate ([] :: [Int]) `shouldBe` Nothing

    describe "問題5: 数列の傾向を判定する関数 classifyTrend を実装せよ。" $ do
        it "例: classifyTrend [1,2,3] は StrictlyIncreasing を返す" $
            classifyTrend [1, 2, 3 :: Int] `shouldBe` StrictlyIncreasing
        it "例: classifyTrend [3,2,1] は StrictlyDecreasing を返す" $
            classifyTrend [3, 2, 1 :: Int] `shouldBe` StrictlyDecreasing
        it "例: classifyTrend [2,2,2] は Constant を返す" $
            classifyTrend [2, 2, 2 :: Int] `shouldBe` Constant
        it "例: classifyTrend [1,1,2,1] は Mixed を返す" $
            classifyTrend [1, 1, 2, 1 :: Int] `shouldBe` Mixed
        it "例: classifyTrend [] は Constant を返す" $
            classifyTrend ([] :: [Int]) `shouldBe` Constant

    describe "問題6: リストから重複要素を除き、最初の出現順を保つ関数 uniquePreserve を実装せよ。" $ do
        it "例: uniquePreserve [1,2,1,3,2] は [1,2,3] を返す" $
            uniquePreserve [1, 2, 1, 3, 2] `shouldBe` [1, 2, 3]
        it "例: uniquePreserve (\"haskell\") は \"haskel\" を返す" $
            uniquePreserve "haskell" `shouldBe` "haskel"

    describe "問題7: 非負整数 n のフィボナッチ数 fibonacci n を計算せよ。" $ do
        it "例: fibonacci 0 は 0" $
            fibonacci 0 `shouldBe` 0
        it "例: fibonacci 7 は 13" $
            fibonacci 7 `shouldBe` 13

    describe "問題8: 文字列リストを長さごとにグループ化する関数 groupByLength を実装せよ。" $ do
        it "例: groupByLength [\"ha\", \"ho\", \"code\"] は [(2,[\"ha\",\"ho\"]),(4,[\"code\"])]" $
            groupByLength ["ha", "ho", "code"]
                `shouldBe` [(2, ["ha", "ho"]), (4, ["code"])]
        it "例: groupByLength [] は []" $
            groupByLength [] `shouldBe` []

    describe "問題9: 文字列リストから出現回数上位 k 個を求める関数 topKFrequent を実装せよ。" $ do
        it "例: topKFrequent 2 [\"ha\",\"ha\",\"hs\",\"ha\",\"hs\"] は [(\"ha\",3),(\"hs\",2)]" $
            topKFrequent 2 ["ha", "ha", "hs", "ha", "hs"]
                `shouldBe` [("ha", 3), ("hs", 2)]
        it "例: topKFrequent 3 [\"foo\",\"bar\",\"foo\",\"baz\",\"bar\",\"foo\"] は [(\"foo\",3),(\"bar\",2),(\"baz\",1)]" $
            topKFrequent 3 ["foo", "bar", "foo", "baz", "bar", "foo"]
                `shouldBe` [("foo", 3), ("bar", 2), ("baz", 1)]

    describe "問題10: (), [], {} の括弧列が正しく対応づけられているかを判定する関数 isBracketBalanced を実装せよ。" $ do
        it "例: isBracketBalanced \"([{}])\" は True" $
            isBracketBalanced "([{}])" `shouldBe` True
        it "例: isBracketBalanced \"([)]\" は False" $
            isBracketBalanced "([)]" `shouldBe` False

    describe "問題11: 同じキーの値をまとめた Map を返す関数 groupByKey を実装せよ。" $ do
        it "例: groupByKey [(\"lang\",\"hs\"),(\"lang\",\"rb\"),(\"year\",\"2024\")]" $
            groupByKey [("lang", "hs"), ("lang", "rb"), ("year", "2024")]
                `shouldBe` M.fromList [("lang", ["hs", "rb"]), ("year", ["2024"])]
        it "例: groupByKey [] は空の Map" $
            groupByKey ([] :: [(String, Int)]) `shouldBe` M.empty

    describe "問題12: 指定したキー列を順に検索する関数 lookupChain を実装せよ。" $ do
        let sample = M.fromList [("host", "localhost"), ("port", "8080"), ("mode", "debug")]
        it "例: すべて見つかれば Right に値リストを返す" $
            lookupChain ["host", "mode"] sample `shouldBe` Right ["localhost", "debug"]
        it "例: 見つからないキーがあれば Left \"エラー\"" $
            lookupChain ["host", "missing", "port"] sample `shouldBe` Left "エラー"
        it "例: 空のキー列は Right []" $
            lookupChain [] sample `shouldBe` Right []

    describe "問題13: 同じ要素が連続する最長区間の長さを求める longestStreak を実装せよ。" $ do
        it "例: longestStreak [1,1,2,2,2,3] は 3" $
            longestStreak [1, 1, 2, 2, 2, 3 :: Int] `shouldBe` 3
        it "例: longestStreak \"aaabbbbcc\" は 4" $
            longestStreak "aaabbbbcc" `shouldBe` 4
        it "例: longestStreak [] は 0" $
            longestStreak ([] :: [Int]) `shouldBe` 0

    describe "問題14: 連続部分列の最大和を求める maxSumWindow を実装せよ。" $ do
        it "例: maxSumWindow 3 [1,5,-2,4,3] は Just 7" $
            maxSumWindow 3 [1, 5, -2, 4, 3] `shouldBe` Just 7
        it "例: maxSumWindow 2 [1,2,3] は Just 5" $
            maxSumWindow 2 [1, 2, 3] `shouldBe` Just 5
        it "例: maxSumWindow 4 [1,2] は Nothing" $
            maxSumWindow 4 [1, 2] `shouldBe` Nothing
        it "例: maxSumWindow 0 [1,2,3] は Nothing" $
            maxSumWindow 0 [1, 2, 3] `shouldBe` Nothing

    describe "問題15: 単語頻度ランキングを求める関数 wordRanking を実装せよ。" $ do
        it "例: wordRanking [\"red\",\"blue\",\"red\",\"green\",\"blue\",\"red\"] は [(1,[\"red\"]),(2,[\"blue\"]),(3,[\"green\"])]" $
            wordRanking ["red", "blue", "red", "green", "blue", "red"]
                `shouldBe` [(1, ["red"]), (2, ["blue"]), (3, ["green"])]
        it "例: wordRanking [\"a\",\"b\",\"a\",\"b\",\"c\"] は [(1,[\"a\",\"b\"]),(2,[\"c\"])]" $
            wordRanking ["a", "b", "a", "b", "c"]
                `shouldBe` [(1, ["a", "b"]), (2, ["c"])]
        it "例: wordRanking [] は []" $
            wordRanking [] `shouldBe` []

    describe "問題16: NestedList を平坦化する関数 flattenNested を実装せよ。" $ do
        it "例: flattenNested (Nested [Atom 1, Nested [Atom 2, Atom 3], Atom 4]) は [1,2,3,4]" $
            flattenNested (Nested [Atom 1, Nested [Atom 2, Atom 3], Atom 4] :: NestedList Int)
                `shouldBe` [1, 2, 3, 4]
        it "例: flattenNested (Atom \"hs\") は [\"hs\"]" $
            flattenNested (Atom "hs") `shouldBe` ["hs"]

    describe "問題17: 条件を満たす連続区間を抽出する関数 segmentsBy を実装せよ。" $ do
        it "例: segmentsBy even [2,4,1,6,8,3,10] は [[2,4],[6,8],[10]]" $
            segmentsBy even [2, 4, 1, 6, 8, 3, 10 :: Int]
                `shouldBe` ([[2, 4], [6, 8], [10]] :: [[Int]])
        it "例: segmentsBy (>0) [1,2,-1,3,0,4] は [[1,2],[3],[4]]" $
            segmentsBy (> 0) [1, 2, -1, 3, 0, 4 :: Int]
                `shouldBe` ([[1, 2], [3], [4]] :: [[Int]])
        it "例: segmentsBy odd [2,4,6] は []" $
            segmentsBy odd [2, 4, 6 :: Int] `shouldBe` ([] :: [[Int]])

    describe "問題18: 3 辺の長さから三角形の種類を分類する関数 classifyTriangle を実装せよ。" $ do
        it "例: classifyTriangle 3 3 3 は Just Equilateral" $
            classifyTriangle 3 3 3 `shouldBe` Just Equilateral
        it "例: classifyTriangle 3 4 4 は Just Isosceles" $
            classifyTriangle 3 4 4 `shouldBe` Just Isosceles
        it "例: classifyTriangle 3 4 5 は Just Scalene" $
            classifyTriangle 3 4 5 `shouldBe` Just Scalene
        it "例: classifyTriangle 2 3 5 は Just Degenerate" $
            classifyTriangle 2 3 5 `shouldBe` Just Degenerate
        it "例: classifyTriangle 1 2 4 は Nothing" $
            classifyTriangle 1 2 4 `shouldBe` Nothing
        it "例: classifyTriangle 3 0 3 は Nothing" $
            classifyTriangle 3 0 3 `shouldBe` Nothing

    describe "問題19: Foldable なペア列を Monoid でまとめる関数 collectMonoid を実装せよ。" $ do
        it "例: collectMonoid [(\"lang\",Sum 1),(\"lang\",Sum 2),(\"year\",Sum 3)]" $
            collectMonoid [("lang", Sum 1), ("lang", Sum 2), ("year", Sum 3)]
                `shouldBe` M.fromList [("lang", Sum 3), ("year", Sum 3)]
        it "例: collectMonoid (NE.fromList [(1,Any True),(1,Any False),(2,Any False)]) は fromList [(1,Any True),(2,Any False)]" $
            collectMonoid (NE.fromList [(1, Any True), (1, Any False), (2, Any False)])
                `shouldBe` M.fromList [(1, Any True), (2, Any False)]
        it "例: collectMonoid [] は空の Map" $
            collectMonoid ([] :: [(Int, Sum Int)]) `shouldBe` M.empty
