




% Management的辅助
% Management/Data.lhs

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE  QuasiQuotes       #-}
{-# LANGUAGE  TemplateHaskell   #-}
{-# LANGUAGE  TypeFamilies      #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  ViewPatterns      #-}
\end{code}

\subsection{模块 Management.Data}
\begin{code}
module Management.Data where
\end{code}

导入Yesod 与 PostgreSQl 数据库。
\begin{code}
        import Yesod
        import Database.Persist.Postgresql
\end{code}
Text
\begin{code}
        import Data.Text.Lazy
\end{code}
使用 Safe 确定安全
\begin{code}
        import SafeS
\end{code}

\subsection{子站 Management的路由}
\begin{code}
        data Management = Management ConnectionPool
        mkYesodSubData "Management" [parseRoutes|
          /5527da67/#Text BooklendR POST
          /3689884/#Text BookreturnR POST
          /4a2a356d/#Text BookrenewR POST
          |]
\end{code}
