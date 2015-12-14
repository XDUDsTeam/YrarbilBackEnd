




% Main/Config.lhs
% 设置 User-Agent
% 设置 全局
% 设置 数据库

%导入导言区
\input{preamble}
\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
\end{code}
\subsection{模块}
\begin{code}
module Main.Config
      ( isUserAgnetRight
      , getSqlConn
      , toConConfig
      , SqlConn(..)
      ) where
\end{code}
\subsection{导入}
 Yesod。
\begin{code}
        import Yesod
\end{code}
处理 Monad。
\begin{code}
        import Control.Monad.Trans.Resource
\end{code}
处理 ByteString 。
\begin{code}
        import Data.String(fromString)
        import Data.ByteString.Lazy(toStrict)
        import qualified Data.ByteString.Internal as DBI
        import Data.ByteString.Lazy.Internal(ByteString(..))
\end{code}

使用 ConnectionString。
\begin{code}
        import Database.Persist.Postgresql(ConnectionString)
\end{code}
处理 JSON
\begin{code}
        import Data.Aeson
\end{code}
获取参数。
\begin{code}
        import System.Environment(getArgs)
\end{code}
使用输入与输出。
\begin{code}
        import System.IO
\end{code}

\subsection{YrarbilBackend 访问}
测试后端

是否是“认证”的客户端。
\begin{code}
        isUserAgnetRight :: MonadResourceBase m => HandlerT site m Bool
        isUserAgnetRight = liftHandlerT (lookupHeaders "User-Agent") >>= (\xs -> return $ or $ map (\x-> elem x xs) ["YrarbilBackend-testclient"])          
\end{code}


\subsection{全局函数}

\subsection{连接 String}
%"host=qinka-s.jios.org dbname=postgres user=qinka password=johnjing port=2999"
连接 String 由 主机名
\footnote{域名或者端口}
数据库名，用户名，密码，端口。
\begin{code}
        data SqlConn = SqlConn
          { hostName :: String
          , port :: String
          , userName :: String
          , passWord :: String
          , dbName :: String
          , connCtr :: Int
          }
\end{code}
实现 ToJSON 类型类。
\begin{code}
        instance FromJSON SqlConn where
          parseJSON (Object v) =
            SqlConn
              <$> v .: "host"
              <*> v .: "port"
              <*> v .: "user"
              <*> v .: "password"
              <*> v .: "dbname"
              <*> v .: "connectionLmt"
\end{code}
获取设置
\begin{code}

        getSqlConn :: IO (Maybe SqlConn)
        getSqlConn = do
          json <- getHandle>>= getIn
          return $ decode $ fromString $ concat json

        getIn :: Handle -> IO [String]
        getIn h = hIsEOF h  >>= (\is ->
          if is
            then return []
            else do
              r <- hGetLine h
              getIn h>>= return.(r:))

        getHandle :: IO Handle
        getHandle = do
          args <- getArgs
          case args of
            (x:_) -> openFile x ReadMode
            _ -> return stdin
\end{code}
生成 连接字串。
\begin{code}
        toConConfig :: Maybe SqlConn -> Maybe (DBI.ByteString,Int)
        toConConfig (Just SqlConn{..})= Just (toStrict $
           fromString $ "host=\'"++hostName
                  ++ "\' port=\'"++port
                  ++ "\' user=\'"++userName
                  ++ "\' password=\'"++passWord
                  ++ "\' dbname=\'"++dbName
                  ++ "\'",connCtr)
        toConConfig _ = Nothing

\end{code}

