




% InfoFinder/Data.lhs
% 辅助/路由

%导入导言区
\input{preamble}
\subsection{特性}
\begin{code}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies	     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns	     #-}
\end{code}
\subsection{模块 InfoFinder.Data}
\begin{code}
module InfoFinder.Data where
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

\subsection{子站 InfoFinder 的路由}
\begin{code}
        data InfoFinder = InfoFinder ConnectionPool
        mkYesodSubData "InfoFinder" [parseRoutes|
          /ons InfonsR GET
          /9c4f7bfef/#Text InfocipR GET
          |]
\end{code}
