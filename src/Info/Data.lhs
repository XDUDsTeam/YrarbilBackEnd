




%Info/Data.hs
%Info.lhs 由于语法分割出来的
%导入导言区
\input{preamble}


\subsection{特性}
\begin{code}
{-# LANGUAGE  QuasiQuotes       #-}
{-# LANGUAGE  TemplateHaskell   #-}
{-# LANGUAGE  TypeFamilies      #-}
\end{code}
\subsection{模块 Info.Data}
\begin{code}
module Info.Data where
\end{code}
\subsection{导入}
Yesod。
\begin{code}
        import  Yesod
\end{code}
PostgreSQL。
\begin{code}
        import  Database.Persist.Postgresql
\end{code}

\subsection{子站-Info}
InfoR 提供版本与帮助内容。
\begin{code}
        data Information = Information ConnectionPool
        mkYesodSubData "Information" [parseRoutes|
          / InfoR GET
          |]
\end{code}
