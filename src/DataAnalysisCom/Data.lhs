




% DataAnalysisCom/Data.lhs
%辅助与路由

%导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies	     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns	     #-}
{-# LANGUAGE DataKinds         #-}
\end{code}
\subsection{模块 DataAnalysisCom.Data}
\begin{code}
module DataAnalysisCom.Data where
\end{code}
导入Yesod与PostgreSQL。
\begin{code}
        import Yesod
        import Database.Persist.Postgresql
\end{code}
处理 Text。
\begin{code}
        import Data.Text.Lazy
\end{code}
使用SafeS确定安全
\begin{code}
        import SafeS
\end{code}

\subsection{子站 DataAnalysisCom 的路由}
\begin{code}
        data AnaCom = AnaCom ConnectionPool
        mkYesodSubData "AnaCom" [parseRoutes|
          /34b7da9d/#Text        BookinfolistR          POST
          /8bd0aa8/#Text         BookitemlistR          POST
          /5e2ba697/#Text        ReaderlistR            POST
          /989abddd5/#Text       BookoptinlistR         POST
          /988adff/#Text         BookoptoutlistR        POST
          /1123aa/#Text          BookoptmainlistR       POST
          /90f7aa8/#Text         PunishlistR            POST
          |]
\end{code}
