




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
      , getConfig
      , toConfigT
      , Config(..)
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


\subsection{全局设置}
\begin{code}
        data Config = Config
          { sqlConfig :: SqlConn
          , exePort :: Int
          }
\end{code}
实现 FromJSON 类型类。
\begin{code}
        instance FromJSON Config where
          parseJSON (Object v) = Config
            <$> v .: "sqlconn"
            <*> v .: "port"
\end{code}

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
实现 FromJSON 类型类。
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

        getConfig :: IO (Maybe Config)
        getConfig = do
          json <- getIn
          return $ decode $ fromString $ concat json

        getIn :: IO [String]
        getIn = hIsEOF h  >>= (\is ->
          if is
            then return []
            else do
              r <- hGetLine h
              getIn h>>= return.(r:))
          where
            h = stdin

\end{code}
生成 所需数据
\begin{code}
        toConfig :: Maybe Config -> Maybe (DBI.ByteString,Int,Int)
        toConfig Config{..} = (s,c,exePort)
          where
            (s,c) = toConConfig sqlConfig
            toConConfig SqlConn{..} = (toStrict $
              fromString $ "host=\'"++hostName
                     ++ "\' port=\'"++port
                     ++ "\' user=\'"++userName
                     ++ "\' password=\'"++passWord
                     ++ "\' dbname=\'"++dbName
                     ++ "\'",connCtr)
        toConfig _ = Nothing

\end{code}

