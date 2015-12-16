




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
module Yrarbil.Backend.Config
      ( getConfig
      , toConfigT
      , Config(..)
      , SqlConn(..)
      ) where
\end{code}
\subsection{导入}
处理 ByteString 。
\begin{code}
        import Data.String(fromString)
        import Data.ByteString.Lazy(toStrict)
        import qualified Data.ByteString.Internal as DBI
\end{code}

处理 JSON
\begin{code}
        import Data.Aeson
\end{code}
使用输入与输出。
\begin{code}
        import System.IO
\end{code}


\subsection{全局设置}
\begin{code}
        data Config = Config
          { exePort :: Int
          , sqlConfig :: SqlConn
          }
\end{code}
实现 FromJSON 类型类，与 ToJSON 类型类。
\begin{code}
        instance FromJSON Config where
          parseJSON (Object v) = Config
            <$> v .: "port"
            <*> v .: "sqlconn"
        instance ToJSON Config where
          toJSON Config{..} = object
            [ "sqlconn" .= sqlConfig
            , "port" .= exePort
            ]
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
实现 FromJSON 类型类，与 ToJSON 类型类。
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
        instance ToJSON SqlConn where
          toJSON SqlConn{..} = object
            [ "host" .= hostName
            , "port" .= port
            , "user" .= userName
            , "password" .= passWord
            , "dbname" .= dbName
            , "connectionLmt" .= connCtr
            ]
\end{code}
获取设置
\begin{code}

        getConfig :: IO (Maybe Config)
        getConfig = do
          j <- getIn
          return $ decode $ fromString $ concat j

        getIn :: IO [String]
        getIn = isEOF >>= (\is ->
          if is
            then return []
            else do
              r <- getLine
              getIn >>= return.(r:)
          )

\end{code}
生成 所需数据
\begin{code}
        toConfigT :: Maybe Config -> Maybe (DBI.ByteString,Int,Int)
        toConfigT (Just Config{..}) = let (s,c) = toConConfig sqlConfig in Just (s,c,exePort)
          where
            toConConfig SqlConn{..} = (toStrict $
              fromString $ "host=\'"++hostName
                     ++ "\' port=\'"++port
                     ++ "\' user=\'"++userName
                     ++ "\' password=\'"++passWord
                     ++ "\' dbname=\'"++dbName
                     ++ "\'",connCtr)
        toConfigT _ = Nothing

\end{code}
