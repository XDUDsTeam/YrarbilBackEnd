




% Appointment/Data.lhs
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
\subsection{模块 Appointment/Data}
\begin{code}
module Appointment.Data where
\end{code}
导入 Yesod 与 PostgreSQL。
\begin{code}
        import Yesod
        import Database.Persist.Postgresql
\end{code}
导入对 Text 的支持。
\begin{code}
        import Data.Text.Lazy
\end{code}
使用 SafeS 确定安全。
\begin{code}
        import SafeS
\end{code}

\subsection{子站 Appointment 的路由}
\begin{code}
        data Appoint = Appoint ConnectionPool
        mkYesodSubData "Appoint" [parseRoutes|
          /on AppointonR POST
          /off AppointoffR POST
          |]
\end{code}
