




% start/DaocloudMain.lhs
% Docker 容器 启动的配置，目前是 道客云版本。

% 导入导言区
\input{../src/preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
         import Data.Aeson
\end{code}
处理 ByteStrings
\begin{code}
         import Data.ByteString.Internal
\end{code}
处理 命令行参数。
\begin{code}
         import Args
\end{code}
处理 Maybe
\begin{code}
         import Data.Maybe
\end{code}
\subsection{主函数}
\begin{code}
         main :: IO ()
         main = runArgs toConfig >>= main'
         main' :: Config-> IO()
         main' c = do
           (hIn,_,_,_) <- createProcess (shell "yb.bin") {std_in=CreatePipe}
           hPutStrLn (fromMaybe stdout hIn) $ (read $ show $ encode c)
           hClose (fromMaybe stdout hIn)
\end{code}
\subsection{写入函数}
将特定环境变量等写入后端主函数的标准输入流。
\begin{code}
         toConfig :: Launch -> IO Config
         toConfig Launch{..} = do
           ybPort <- getEnv yrarbil_backend_port
           ybCLmt <- getEnv yrarbil_backend_conlmt
           pgAddr <- getEnv postgresql_addr
           pgPort <- getEnv postgresql_port
           pgDBN  <- getEnv postgresql_db
           pgPass <- getEnv postgresql_pw
           pgUser <- getEnv postgresql_usr
           return $ Config (read ybPort) $
             SqlConn pgAddr pgPort pgUser pgPass pgDBN (read ybCLmt)
\end{code}
