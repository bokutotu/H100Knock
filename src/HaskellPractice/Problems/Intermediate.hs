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
    raceBoth,
) where

import           Control.Applicative              (Alternative (..))
import           Control.Monad.Except             (MonadError (catchError, throwError))
import           Control.Monad.Trans              (MonadIO (liftIO))
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.Reader       (ReaderT)
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Foldable                    (traverse_)
import           Data.Functor                     (($>), (<$>))
import           Data.List                        (isPrefixOf)
import           System.IO                        (Handle,
                                                   IOMode (ReadMode, WriteMode),
                                                   hGetContents, hGetContents',
                                                   hPutStrLn, openFile,
                                                   withFile)

-- 問題1: ハンドルから行を読み込み、空行（空文字列）が現れた時点で読み取りを停止し、それまでに読んだ行だけを順番に返す関数 readUntilBlank を実装せよ。
-- 空行そのものと、その後に続く行は結果に含めないこと。空行が最初に現れた場合は空リストを返すこと。
readUntilBlank :: Handle -> IO [String]
readUntilBlank h = takeWhile (/= "") . lines <$> hGetContents h

-- 問題2: ハンドルから全行を読み込み、述語を満たす行数を数える関数 countLinesMatching を実装せよ。
countLinesMatching :: Handle -> (String -> Bool) -> IO Int
countLinesMatching h f = length . filter f . lines <$> hGetContents h

-- 問題3: ファイルパスと取得したい末尾行数 n を受け取り、ファイル末尾から n 行を返す関数 tailFile を実装せよ。
tailFile :: FilePath -> Int -> IO [String]
tailFile _ n | n <= 0 = pure []
tailFile path n = withFile path ReadMode (fmap (lastN . lines) . hGetContents')
  where
    -- hGetContentsは遅延評価なので、Specファイルの実際に値を見る部分まで、計算が行われない。
    -- 一方で、withFileはwithFileのブロックの部分でしか、ファイルを開いていない。(ブロックを抜けた場合ファイルを閉じる)
    -- そのため、実際にspecで値を評価する頃にはwithFileは閉じているため、エラーが出て落ちる。
    lastN l = drop (max (length l - n) 0) l

tailFile' :: FilePath -> Int -> IO [String]
tailFile' path numLines = do
    -- このコードは、ファイルハンドルを閉じていないので、いつファイルハンドルが閉じるかはGCが決めるのであんまりいいコードではない
    h <- openFile path ReadMode
    reverse . take numLines . reverse . lines <$> hGetContents h

-- 問題4: 入力ファイルから条件を満たす行だけを抽出し、出力ファイルへ書き出す関数 copyFileFiltered を実装せよ。
copyFileFiltered :: FilePath -> FilePath -> (String -> Bool) -> IO ()
copyFileFiltered input output f = withFile input ReadMode (withFile output WriteMode . filterLine)
  where
    filterLine inputHandle outputHandle =
        hGetContents inputHandle
            >>= traverse_ (hPutStrLn outputHandle)
                . filter f
                . lines

-- traverse_ (hPutStrLn outputHandle) . filter f . lines =<< hGetContents inputHandle

-- 問題5: 単純な文字列パーサ Parser を用意した。Functor / Applicative / Monad / Alternative の各インスタンスを定義せよ。
newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err       -> Left err
        Right (a, str) -> Right (f a, str)

instance Applicative Parser where
    pure a = Parser (\input -> Right (a, input))
    Parser pf <*> Parser pa = Parser $ \input -> do
        (f, s1) <- pf input
        (a, s2) <- pa s1
        pure (f a, s2)

-- Parser pf <*> Parser pa = Parser $ \input ->
--     case pf input of
--         Left e -> Left e
--         Right (fab, s1) -> case pa s1 of
--             Left e        -> Left e
--             Right (a, s2) -> Right (fab a, s2)

instance Monad Parser where
    Parser pf >>= f = Parser $ \input -> do
        (a, s1) <- pf input
        let Parser pa = f a
        pa s1

instance Alternative Parser where
    empty = Parser $ \_ -> Left "empty parser"
    Parser p1 <|> Parser p2 = Parser $ \input ->
        case p1 input of
            Left _ -> p2 input
            result -> result

-- 問題6: 条件を満たす 1 文字を読み取るパーサ satisfy を実装せよ。
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
    (c : cs)
        | f c -> Right (c, cs)
    _ -> Left "satisfy: no match"

-- 問題7: 指定した文字を消費するパーサ charP を実装せよ。
charP :: Char -> Parser Char
charP = satisfy . (==)

-- 問題8: 二重引用符で囲まれた文字列リテラルを読み取るパーサ stringLiteral を実装せよ。
stringLiteral :: Parser String
stringLiteral = do
    _ <- charP '"'
    content <- many (satisfy (/= '"'))
    _ <- charP '"'
    return content

-- 問題9: 符号付き整数を読み取るパーサ intLiteral を実装せよ。
intLiteral :: Parser Int
intLiteral = Parser $ \case
    ('+' : cs) -> parseDigits cs
    ('-' : cs) -> case parseDigits cs of
        Left err        -> Left err
        Right (n, rest) -> Right (-n, rest)
    cs -> parseDigits cs
  where
    parseDigits s =
        let (digits, rest) = span (`elem` ['0' .. '9']) s
         in if null digits
                then Left "intLiteral: no digits"
                else Right (read digits, rest)

-- 問題10: 区切り記号としてカンマと任意の空白を許す、要素の列を読み取るパーサ commaSeparated を実装せよ。
commaSeparated :: Parser a -> Parser [a]
commaSeparated (Parser p) = Parser $ \input -> do
    case p input of
        Left _ -> Right ([], input)
        Right (firstElem, rest1) -> case runParser (spaces *> charP ',' *> spaces) rest1 of
            Left _ -> Right ([firstElem], rest1)
            Right (_, rest2) -> do
                (elems, finalRest) <- runParser (commaSeparated (Parser p)) rest2
                Right (firstElem : elems, finalRest)
  where
    spaces = many (satisfy (`elem` [' ', '\t', '\n']))

commaSeparated' :: Parser a -> Parser [a]
commaSeparated' p = ((:) <$> p <*> many (spaces *> charP ',' *> spaces *> p)) <|> pure []
  where
    spaces = many (satisfy (`elem` [' ', '\t', '\n']))

-- 問題11: host=..., port=... 形式の設定ファイルを読み込み、Config を返す ExceptT ベースの関数 loadConfig を実装せよ。
data Config = Config
    { configHost :: String
    , configPort :: Int
    }
    deriving (Eq, Show)

newtype AppError = AppError {unAppError :: String}
    deriving (Eq, Show)

data AppEnv = AppEnv
    { envConfig      :: Config
    , envServiceName :: String
    , envLog         :: String -> IO ()
    }

newtype AppState = AppState
    { stateCounter :: Int
    }
    deriving (Eq, Show)

type AppM = ReaderT AppEnv (StateT AppState (ExceptT AppError IO))

loadConfig :: FilePath -> ExceptT AppError IO Config
loadConfig path = do
    content <- catchError (liftIO $ readFile path) (\e -> throwError $ AppError ("Failed to read config file: " ++ show e))
    let l = lines content
    hostLine <- case filter (isPrefixOf "host=") l of
        (l : _) -> return l
        []      -> throwError $ AppError "Missing host configuration"
    portLine <- case filter (isPrefixOf "port=") l of
        (l : _) -> return l
        []      -> throwError $ AppError "Missing port configuration"
    let host = drop (length "host=") hostLine
    let portStr = drop (length "port=") portLine
    port <- case reads portStr of
        [(n, "")] -> return n
        _         -> throwError $ AppError "Invalid port number"
    return $ Config host port

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
