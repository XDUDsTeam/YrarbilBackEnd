




% ConstFinder.lhs
% 常量查询

% 导入导言区
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
\subsection{模块 ConstFinder}
\begin{code}
module ConstFinder
    ( module ConstFinder
    , module ConstFinder.Data
    ) where
\end{code}
\subsection{导入}
导入 Yesod 与 ConstFinder.Data 。
\begin{code}
        import Yesod
        import ConstFinder.Data
\end{code}
使用 Aeson 处理 JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
\end{code}
处理 Text。
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

\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist ConstFinder where
          type YesodPersistBackend ConstFinder = SqlBackend
          runDB a = do
            ConstFinder p <- getYesod
            runSqlPool a p
\end{code}
