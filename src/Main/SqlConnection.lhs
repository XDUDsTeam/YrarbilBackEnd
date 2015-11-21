




%Main/SqlConnection.lhs
%设置 PosgreSQL 连接设置

%导入导言区
\input{preamble}

使用特性。
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

\begin{code}
module Main.SqlConnection
      ( getSqlConn

      ) where
\end{code}

处理 ByteString 。
\begin{code}
        import Data.ByteString.Char8(pack)
\end{code}

使用 ConnectionString。
\begin{code}
        import Database.Persist.Postgresql(ConnectionString)
\end{code}
使用解析库解析。
\begin{code}
        import Text.Parsec(Parsec,many,noneOf,char,space,(<|>),parse,oneOf,anyChar,string,try)
\end{code}

\subsection{连接 String}
%"host=qinka-s.jios.org dbname=postgres user=qinka password=johnjing port=2999"
连接 String 由 主机名
\footnote{域名或者端口}
，数据库名，用户名，密码，端口。
\begin{code}
        data SqlConn = SqlConn
          { hostName :: String
          , port :: String
          , userName :: String
          , passWord :: String
          , dbName :: String
          , connCtr :: Int
          }

        getSqlConn :: FilePath -> IO (ConnectionString,Int)
        getSqlConn = (>>= return.toConnString.getEither.parse sqlConnE "error :(".(++"\0")).readFile
          where
            getEither (Right x) = x
            getEither (Left x) = error $ show x
            toConnString :: SqlConn -> (ConnectionString,Int)
            toConnString (SqlConn a b c d e f) = (pack $ "host=\'"++a++"\' port=\'"++b++"\' user=\'"++c++"\' password=\'"++d++"\' dbname=\'"++e++"\'",f)
\end{code}

解析 ConnectionString。
\begin{code}
        sqlConnE :: Parsec String () SqlConn
        sqlConnE = do
          hN <- string "host:" *> (many (noneOf ":\n\0 \t")) <* oneOf ":\n\0 \t"
          po <- string "port:" *> (many (oneOf "0123456789")) <* oneOf ":\n\0 \t"
          uN <- string "user:" *> (many (noneOf ":\n\0 \t")) <* oneOf ":\n\0 \t"
          pw <- string "password:" *> (many (noneOf ":\n\0 \t")) <* oneOf ":\n\0 \t"
          db <- string "dbname:" *> (many (noneOf ":\n\0 \t")) <* oneOf ":\n\0 \t"
          cc <- string "lmt:" *> (many (oneOf "0123456789")) <* oneOf ":\n\0 \t"
          return $ SqlConn hN po uN pw db $ read cc
\end{code}
