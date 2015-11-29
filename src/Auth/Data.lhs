




%Auth/Data.lhs
%辅助 Auth.lhs
%导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE  QuasiQuotes       #-}
{-# LANGUAGE  TemplateHaskell   #-}
{-# LANGUAGE  TypeFamilies      #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE  ViewPatterns      #-}
\end{code}
\subsection{模块 Auth.Data}
\begin{code}
module Auth.Data where
\end{code}

导入 Yesod 与 PostgreSQL
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

\subsection{子站Auth 的路由}
\begin{code}
        data Auther = Auther ConnectionPool
        mkYesodSubData "Auther" [parseRoutes|
          /b8ab91a6/#Text/fc8252b7/#Text AdmininR POST
          /a050ba73/#Text/fc8252b7/#Text AdminoutR POST
          /b8ab91a6/#Text/5ece85ba/#Text ReaderinR POST
          /a050ba73/#Text/5ece85ba/#Text ReaderoutR POST
          |]
\end{code}
