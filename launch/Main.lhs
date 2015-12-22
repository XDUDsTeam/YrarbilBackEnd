




% start/NormalMain.lhs
% 常规的开启后端的程序。

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
\end{code}
\subsection{模块}
\begin{code}
module Main (main) where
\end{code}

\subsection{导入}
处理  参数。
\begin{code}
        import Args
\end{code}
处理 进程。
\begin{code}
        import System.Process
        import System.Exit
\end{code}
处理目录。
\begin{code}
        import System.Directory
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
处理 Maybe
\begin{code}
        import Data.Maybe
\end{code}
\subsection{main 主函数}

\begin{code}
        main :: IO ()
        main = putStrLn "unfinished!"
\end{code}

\subsection{处理Args}
\begin{code}
        run :: Launch -> IO()
        run Launch{..} = do
          path <- getAppUserDataDirectory "yb"
          case (isEtc,isOut) of
            (True,True) -> do
              rt <- openFile (path++"/config.json") ReadMode >>= getIn
              putStrLn $ unlines rt
            (False,True) -> do
              putStrLn $ read $ show $ encode $ toConfig
            (True,False) -> do
              rt <- openFile (path++"/config.json") ReadMode >>= getIn
              toProcess  $ unlines rt
            (False,False) -> do
              toProcess $ read $ show $ encode toConfig
          where
            toConfig = Config ybPort $
              SqlConn dbAddr dbPort dbUsr dbPass dbNme ybCml
            toProcess :: String -> IO()
            toProcess str = do
              (hIn',_,_,_) <- createProcess (shell $ "yb.bin" ++ exeExtension) {std_in=CreatePipe}
              let hIn = fromMaybe stdout hIn'
              hPutStr hIn str
              hClose hIn
\end{code}
处理读入的函数。
\begin{code}
        getIn :: Handle -> IO [String]
        getIn h = hIsEOF h >>= (\is ->
          if is
            then return []
            else do
              r <- hGetLine h
              getIn h >>= return.(r:)
          )
\end{code}
