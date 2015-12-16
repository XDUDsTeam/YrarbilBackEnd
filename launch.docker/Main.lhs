




% start/DaocloudMain.lhs
% Docker 容器 启动的配置，目前是 道客云版本。

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}
\subsection{程序主模块}
\begin{code}
module Main
       ( main
       ) where
\end{code}

\subsection{导入}
处理 进程。
\begin{code}
         import  System.Process
\end{code}
处理 环境变量。
\begin{code}
         import  System.Environment
\end{code}
IO
\begin{code}
         import  System.IO
\end{code}
处理数据
\begin{code}
         import Yrarbil.Backend.Config
\end{code}
处理JSON
\begin{code}
         imoprt Data.Aeson
\end{code}
处理 ByteStrings
\begin{code}
		 imoprt Data.ByteString.Internal
\end{code}
\subsection{主函数}
\begin{code}
         main :: IO()
         main = do
         (hIn,_,_,_) <- createProcess $ shell "yb.bin"
         getConfig >>= (hPutStrLn hIn . read . show . encode)
\end{code}
\subsection{写入函数}
将特定环境变量等写入后端主函数的标准输入流。
\begin{code}
         getConfig :: IO Config
         getConfig = do
           ybPort <- getEnv "YRARBIL_BACKEND_PORT"
           ybCLmt <- getEnv "YRARBIL_BACKEND_CONNECTIONLIMIT"
           pgAddr <- getEnv "POSTGRESQL_PORT_5432_TCP_ADDR"
           pgPort <- getEnv "POSTGRESQL_PORT_5432_TCP_PORT"
           pgDBN  <- getEnv "POSTGRESQL_INSTANCE_NAME"
           pgPass <- getEnv "POSTGRESQL_PASSWORD"
           pgUser <- getEnv "POSTGRESQL_USERNAME"
           return $ Config ybPort $
             SqlConn pgAddr pgPort pgUser pgPass pgDBN ybCLmt
\end{code}
