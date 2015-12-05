




% 图书信息查询
% InfoFinder.lhs

% 导入导言区
\input{preamble}


\subsection{使用的特性}
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
module InfoFinder
    ( module InfoFinder
    , module InfoFinder.Data
    ) where
\end{code}
\subsection{导入}
导入 Yesod 与 InfoFinder.Data
\begin{code}
        import Yesod
        import InfoFinder.Data
\end{code}
导入 aeson 处理 JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
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
对时间的处理。
\begin{code}
        import Data.Time
\end{code}

\subsection{数据库处理}
\begin{code}
        instance YesodPersist InfoFinder where
          type YesodPersistBackend InfoFinder = SqlBackend
          runDB a = do
            InfoFinder p <- getYesod
            runSqlPool a p
\end{code}
