




% AddDel/Data.lhs
% 辅助与路由

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies	     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns	     #-}
\end{code}
\subsection{模块 AddDel.Data}
\begin{code}
module AddDel.Data where
\end{code}
导入Yesod与PostgreSQL。
\begin{code}
        import Yesod
        import Database.Persist.Postgresql
\end{code}
导入对Text的支持。
\begin{code}
        import Data.Text.Lazy
\end{code}
使用SafeS确定安全
\begin{code}
        import SafeS
\end{code}

\subsection{子站 AddDel 的路由}
\begin{code}
        data AddDel = AddDel ConnectionPool
        mkYesodSubData "AddDel" [parseRoutes|
          /8769ab8/#Text PutinR POST
          /761b31a3/#Text RemoveoutR POST
          |]
\end{code}
