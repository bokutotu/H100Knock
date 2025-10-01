module HaskellPractice.ProblemsSpec (spec) where

import qualified Data.Map                       as M
import           Test.Hspec

import           HaskellPractice.Problems.Basic

spec :: Spec
spec = do
    describe "問題1: リストから重複要素を除き、最初の出現順を保つ関数 uniquePreserve を実装せよ。" $ do
        it "例: uniquePreserve [1,2,1,3,2] は [1,2,3] を返す" $
            uniquePreserve [1, 2, 1, 3, 2] `shouldBe` [1, 2, 3]
        it "例: uniquePreserve (\"haskell\") は \"haskel\" を返す" $
            uniquePreserve "haskell" `shouldBe` "haskel"

    describe "問題2: 非負整数 n のフィボナッチ数 fibonacci n を計算せよ。" $ do
        it "例: fibonacci 0 は 0" $
            fibonacci 0 `shouldBe` 0
        it "例: fibonacci 7 は 13" $
            fibonacci 7 `shouldBe` 13

    describe "問題3: 文字列リストを長さごとにグループ化する関数 groupByLength を実装せよ。" $ do
        it "例: groupByLength [\"ha\", \"ho\", \"code\"] は [(2,[\"ha\",\"ho\"]),(4,[\"code\"])]" $
            groupByLength ["ha", "ho", "code"]
                `shouldBe` [(2, ["ha", "ho"]), (4, ["code"])]
        it "例: groupByLength [] は []" $
            groupByLength [] `shouldBe` []

    describe "問題4: 文字列リストから出現回数上位 k 個を求める関数 topKFrequent を実装せよ。" $ do
        it "例: topKFrequent 2 [\"ha\",\"ha\",\"hs\",\"ha\",\"hs\"] は [(\"ha\",3),(\"hs\",2)]" $
            topKFrequent 2 ["ha", "ha", "hs", "ha", "hs"]
                `shouldBe` [("ha", 3), ("hs", 2)]
        it "例: topKFrequent 3 [\"foo\",\"bar\",\"foo\",\"baz\",\"bar\",\"foo\"] は [(\"foo\",3),(\"bar\",2),(\"baz\",1)]" $
            topKFrequent 3 ["foo", "bar", "foo", "baz", "bar", "foo"]
                `shouldBe` [("foo", 3), ("bar", 2), ("baz", 1)]

    describe "問題5: (), [], {} の括弧列が正しく対応づけられているかを判定する関数 isBracketBalanced を実装せよ。" $ do
        it "例: isBracketBalanced \"([{}])\" は True" $
            isBracketBalanced "([{}])" `shouldBe` True
        it "例: isBracketBalanced \"([)]\" は False" $
            isBracketBalanced "([)]" `shouldBe` False

    describe "問題6: (キー, 値) のリストをキーごとにまとめる関数 groupByKey を実装せよ。" $ do
        it "例: groupByKey [(\"lang\",\"hs\"),(\"lang\",\"rb\"),(\"year\",\"2024\")] は fromList [(\"lang\",[\"hs\",\"rb\"]),(\"year\",[\"2024\"])]" $
            groupByKey [("lang", "hs"), ("lang", "rb"), ("year", "2024")]
                `shouldBe` M.fromList [("lang", ["hs", "rb"]), ("year", ["2024"])]
        it "例: groupByKey [] は空の Map" $
            groupByKey ([] :: [(Int, String)]) `shouldBe` M.empty

    describe "問題7: 指定したキー列を順に検索し、すべて揃えば Right、途中で見つからない場合は \"エラー\" で Left を返す関数 lookupChain を実装せよ。" $ do
        it "例: lookupChain [\"hs\",\"rb\"] (fromList [(\"hs\",1),(\"rb\",2)]) は Right [1,2] を返す" $
            lookupChain ["hs", "rb"] (M.fromList [("hs", 1 :: Int), ("rb", 2)])
                `shouldBe` Right [1, 2]
        it "例: lookupChain [\"hs\",\"py\"] (fromList [(\"hs\",1)]) は Left \"エラー\" を返す" $
            lookupChain ["hs", "py"] (M.fromList [("hs", 1 :: Int)])
                `shouldBe` Left "エラー"

    describe "問題8: 同じ要素が連続する最長区間の長さを求める関数 longestStreak を実装せよ。" $ do
        it "例: longestStreak [1,1,2,2,2,3] は 3 を返す" $
            longestStreak [1, 1, 2, 2, 2, 3] `shouldBe` 3
        it "例: longestStreak \"aaabbbbcc\" は 4 を返す" $
            longestStreak "aaabbbbcc" `shouldBe` 4
