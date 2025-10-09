module HaskellPractice.Problems.Intermediate (
    -- * IO section
    readUntilBlank,
    countLinesMatching,
    tailFile,
    copyFileFiltered,

    -- * Parser section
    Parser (..),
    satisfy,
    charP,
    stringLiteral,
    intLiteral,
    commaSeparated,

    -- * Application stack section
    Config (..),
    AppError (..),
    AppEnv (..),
    AppState (..),
    AppM,
    loadConfig,
    runAppM,
    incrementCounter,
    requireSetting,

    -- * Concurrency section
    mapConcurrentlyLimited,
    raceBoth
) where

import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import System.IO (Handle)

-- 問題1: ハンドルから空行が現れるまでの行を読み取り、行のリストとして返す関数 readUntilBlank を実装せよ。
readUntilBlank :: Handle -> IO [String]
readUntilBlank = error "TODO"

-- 問題2: ハンドルから全行を読み込み、述語を満たす行数を数える関数 countLinesMatching を実装せよ。
countLinesMatching :: Handle -> (String -> Bool) -> IO Int
countLinesMatching = error "TODO"

-- 問題3: ファイルパスと取得したい末尾行数 n を受け取り、ファイル末尾から n 行を返す関数 tailFile を実装せよ。
tailFile :: FilePath -> Int -> IO [String]
tailFile = error "TODO"

-- 問題4: 入力ファイルから条件を満たす行だけを抽出し、出力ファイルへ書き出す関数 copyFileFiltered を実装せよ。
copyFileFiltered :: FilePath -> FilePath -> (String -> Bool) -> IO ()
copyFileFiltered = error "TODO"

-- 問題5: 単純な文字列パーサ Parser を用意した。Functor / Applicative / Monad / Alternative の各インスタンスを定義せよ。
newtype Parser a = Parser { runParser :: String -> Either String (a, String) }

instance Functor Parser where
    fmap _ _ = error "TODO"

instance Applicative Parser where
    pure _ = error "TODO"
    (<*>) = error "TODO"

instance Monad Parser where
    (>>=) = error "TODO"

instance Alternative Parser where
    empty = error "TODO"
    (<|>) = error "TODO"

-- 問題6: 条件を満たす 1 文字を読み取るパーサ satisfy を実装せよ。
satisfy :: (Char -> Bool) -> Parser Char
satisfy = error "TODO"

-- 問題7: 指定した文字を消費するパーサ charP を実装せよ。
charP :: Char -> Parser Char
charP = error "TODO"

-- 問題8: 二重引用符で囲まれた文字列リテラルを読み取るパーサ stringLiteral を実装せよ。
stringLiteral :: Parser String
stringLiteral = error "TODO"

-- 問題9: 符号付き整数を読み取るパーサ intLiteral を実装せよ。
intLiteral :: Parser Int
intLiteral = error "TODO"

-- 問題10: 区切り記号としてカンマと任意の空白を許す、要素の列を読み取るパーサ commaSeparated を実装せよ。
commaSeparated :: Parser a -> Parser [a]
commaSeparated = error "TODO"

-- 問題11: host=..., port=... 形式の設定ファイルを読み込み、Config を返す ExceptT ベースの関数 loadConfig を実装せよ。
data Config = Config
    { configHost :: String
    , configPort :: Int
    }
    deriving (Eq, Show)

newtype AppError = AppError { unAppError :: String }
    deriving (Eq, Show)

data AppEnv = AppEnv
    { envConfig :: Config
    , envServiceName :: String
    , envLog :: String -> IO ()
    }

data AppState = AppState
    { stateCounter :: Int
    }
    deriving (Eq, Show)

type AppM = ReaderT AppEnv (StateT AppState (ExceptT AppError IO))

loadConfig :: FilePath -> ExceptT AppError IO Config
loadConfig = error "TODO"

-- 問題12: AppM を実行し、Either で結果または AppError を返す関数 runAppM を実装せよ。
runAppM :: AppEnv -> AppState -> AppM a -> IO (Either AppError (a, AppState))
runAppM = error "TODO"

-- 問題13: カウンタを任意の値だけ増加させる関数 incrementCounter を AppM 内に実装せよ。
incrementCounter :: Int -> AppM ()
incrementCounter = error "TODO"

-- 問題14: Reader 情報から値を取り出し、取得に失敗したら AppError を投げる requireSetting を実装せよ。
requireSetting :: (Config -> Maybe b) -> AppM b
requireSetting = error "TODO"

-- 問題15: 同時実行数の上限を守りながら IO アクションをマップする関数 mapConcurrentlyLimited を実装せよ。
mapConcurrentlyLimited :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcurrentlyLimited = error "TODO"

-- 問題16: 2 つの IO アクションを競争させ、先に終わったほうの結果を返す関数 raceBoth を実装せよ。
raceBoth :: IO a -> IO b -> IO (Either a b)
raceBoth = error "TODO"
