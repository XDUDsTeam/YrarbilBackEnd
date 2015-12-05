




% Index.lhs
% 图书检索

%导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
\end{code}
\subsection{模块}
\begin{code}
module Index
    ( Index
    , Index.Data
    ) where
\end{code}

\subsection{导入}
导入 Yesod 与 Index.Data。
\begin{code}
        import Yesod
        import Index.Data
\end{code}
使用 Aeson 处理 JSON。
\begin{code}
        import Data.Aeson
\end{code}
对 Text 的支持。
\begin{code}
        import Prelude hiding ()
        import Data.Text.Lazy
\end{code}
Persistent \& PostgreSQL
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
处理时间。
\begin{code}
        import Data.Time
\end{code}

\subsection{数据库}
使用 Persistent数据库处理。
\begin{code}
        instance YesodPersist Index where
          type YesodPersistBackend Index = SqlBackend
          runDB a = do
            Index p <- getYesod
            runSqlPool a p
\end{code}
