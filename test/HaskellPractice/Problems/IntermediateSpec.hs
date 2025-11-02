{-# LANGUAGE BlockArguments #-}

module HaskellPractice.Problems.IntermediateSpec (spec) where

import           Control.Applicative                   ((<|>))
import           Control.Monad.Trans.Except            (runExceptT)
import           System.IO                             (IOMode (ReadMode),
                                                        hClose, withFile)
import           System.IO.Temp                        (withSystemTempFile)
import           Test.Hspec

import           HaskellPractice.Problems.Intermediate

spec :: Spec
spec = do
    describe "問題1: 空行が現れるまでハンドルから行を読み取る readUntilBlank を実装せよ。" $ do
        it "例: ファイルに3行目で空行がある場合、そこまでの2行だけを返す" $ withSystemTempFile "hp.txt" $ \path handle -> do
            hClose handle
            writeFile path "alpha\nbeta\n\ngamma\n"
            withFile path ReadMode $ \readHandle -> do
                result <- readUntilBlank readHandle
                result `shouldBe` ["alpha", "beta"]

    describe "問題2: 述語を満たす行数を数える countLinesMatching を実装せよ。" $ do
        it "例: 文字数が3以上の行だけを数える" $ withSystemTempFile "hp.txt" $ \path handle -> do
            hClose handle
            writeFile path "ha\nhaskell\npr\npractice\n"
            withFile path ReadMode $ \readHandle -> do
                count <- countLinesMatching readHandle ((>= 3) . length)
                count `shouldBe` 2

    describe "問題3: ファイル末尾から n 行を取得する tailFile を実装せよ。" $ do
        it "例: 末尾3行を取得する" $ withSystemTempFile "hp.txt" $ \path handle -> do
            hClose handle
            writeFile path (unlines ["l1", "l2", "l3", "l4", "l5"])
            tailFile path 3 `shouldReturn` ["l3", "l4", "l5"]

    describe "問題4: 条件を満たす行のみを別ファイルに書き出す copyFileFiltered を実装せよ。" $ do
        it "例: 空でない行だけを抽出する" $ withSystemTempFile "hp-src.txt" $ \srcPath srcHandle ->
            withSystemTempFile "hp-dst.txt" $ \dstPath dstHandle -> do
                hClose srcHandle
                hClose dstHandle
                writeFile srcPath "keep\n\nline\n"
                copyFileFiltered srcPath dstPath (not . null)
                readFile dstPath `shouldReturn` "keep\nline\n"

    describe "問題5: Parser を Functor/Applicative/Monad/Alternative として実装せよ。" $ do
        it "例: fmap で結果を変換できる" $ do
            runParser (fmap succ (charP 'a')) "abc" `shouldBe` Right ('b', "bc")
        it "例: (<*>) で連結したパーサを実行できる" $ do
            runParser ((,) <$> charP 'a' <*> charP 'b') "abc" `shouldBe` Right (('a', 'b'), "c")
        it "例: Alternative の empty と <|> でバックトラックできる" $ do
            runParser (charP 'x' <|> charP 'a') "abc" `shouldBe` Right ('a', "bc")

    describe "問題6: 条件を満たす1文字だけを読むパーサ satisfy を実装せよ。" $ do
        it "例: 数字文字を1文字読む" $ do
            runParser (satisfy (`elem` ['0' .. '9'])) "3abc" `shouldBe` Right ('3', "abc")
        it "例: 条件不一致ならエラー" $ do
            runParser (satisfy (`elem` ['0' .. '9'])) "abc" `shouldSatisfy` either (const True) (const False)

    describe "問題7: 特定の文字を消費する charP を実装せよ。" $ do
        it "例: 先頭の 'h' を読み取る" $ do
            runParser (charP 'h') "hs" `shouldBe` Right ('h', "s")
        it "例: 文字が違う場合は失敗する" $ do
            runParser (charP 'x') "hs" `shouldSatisfy` either (const True) (const False)

    describe "問題8: 二重引用符で囲まれた stringLiteral を実装せよ。" $ do
        it "例: \"haskell\" を読み取る" $ do
            runParser stringLiteral "\"haskell\" rest" `shouldBe` Right ("haskell", " rest")

    describe "問題9: 符号付き整数を読む intLiteral を実装せよ。" $ do
        it "例: -42 を読み取る" $ do
            runParser intLiteral "-42xyz" `shouldBe` Right (-42, "xyz")

    describe "問題10: カンマ区切り列を読み取る commaSeparated を実装せよ。" $ do
        it "例: 整数列 1, 2, 3 を読み取る" $ do
            runParser (commaSeparated intLiteral) "1, 2, 3" `shouldBe` Right ([1, 2, 3], "")

    describe "問題11: 設定ファイルを読み込む loadConfig を実装せよ。" $ do
        it "例: host と port のペアを読み取る" $ withSystemTempFile "config.cfg" $ \path handle -> do
            hClose handle
            writeFile path "host=localhost\nport=8080\n"
            result <- runExceptT (loadConfig path)
            result `shouldBe` Right (Config "localhost" 8080)

    describe "問題12: AppM を実行する runAppM を実装せよ。" $ do
        it "例: Reader/State/ExceptT の結果をまとめて返す" $ do
            let config = Config "localhost" 8080
                env = AppEnv config "service" (const $ pure ())
                st = AppState 0
            result <- runAppM env st (pure "ok")
            result `shouldBe` Right ("ok", st)

    describe "問題13: カウンタを加算する incrementCounter を実装せよ。" $ do
        it "例: stateCounter を 5 増やす" $ do
            let config = Config "localhost" 8080
                env = AppEnv config "service" (const $ pure ())
                st = AppState 10
            result <- runAppM env st (incrementCounter 5)
            case result of
                Right ((), AppState counter) -> counter `shouldBe` 15
                Left err                     -> expectationFailure (show err)

    describe "問題14: Reader から設定値を取得する requireSetting を実装せよ。" $ do
        it "例: Config からポート番号を取り出す" $ do
            let config = Config "localhost" 8080
                env = AppEnv config "service" (const $ pure ())
                st = AppState 0
            result <- runAppM env st (requireSetting (Just . configPort))
            result `shouldBe` Right (8080, st)

    describe "問題15: 同時実行数の上限付き mapConcurrentlyLimited を実装せよ。" $ do
        it "例: 2 並列で平方を計算する" $ do
            result <- mapConcurrentlyLimited 2 (pure . (* 2)) [1, 2, 3]
            result `shouldBe` [2, 4, 6]

    describe "問題16: 2 つの IO を競争させる raceBoth を実装せよ。" $ do
        it "例: 左の IO が先に終わる場合" $ do
            result <- raceBoth (pure "left") (pure (42 :: Int))
            result `shouldBe` Left "left"
