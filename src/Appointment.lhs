




% Appointment.lhs
% 图书预约

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
{-# LANGUAGE DeriveGeneric              #-}
\end{code}

\subsection{模块 Appointment}
\begin{code}
module Appointment
      ( module Appointment
      , module Appointment.Data
      ) where
\end{code}
\subsection{导入}
导入 Yesod Common Data。
\begin{code}
        import Yesod
        import Common
        import Appointment.Data
\end{code}
处理 JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe 。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的支持。
\begin{code}
        import Prelude hiding ()
        import qualified Prelude as P
        import Data.Text.Lazy
\end{code}
对数据库的处理，使用 Persistent \& PostgreSQL。
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
处理时间。
\begin{code}
        import Data.Time
        import Data.Time.Clock
\end{code}
\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist Appointment where
          type YesodPersistBackend Appointment = SqlBackend
          runDB a = do
            Appointment p <- getYesod
            runSqlPool a p
\end{code}
数据库的表。
\begin{description}
  \item[]
\end{description}
