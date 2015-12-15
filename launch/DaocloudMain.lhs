




% start/DaocloudMain.lhs
% Docker 容器 启动的配置，目前是 道客云版本。

% 导入导言区
\input{preamble}

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

\subsection{主函数}
\begin{code}
         main :: IO()
         main = putStrLn "helloworld"
\end{code}
\subsection{写入函数}
将特定环境变量等写入后端主函数的标准输入流。
