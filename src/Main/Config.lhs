




% Main/Config.lhs
% 设置 User-Agent
% 设置 全局
% 设置 数据库

%导入导言区
\input{preamble}
\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
\end{code}
\subsection{模块}
\begin{code}
module Main.Config
      ( isUserAgnetRight
      , module Yrarbil.Backend.Config
      ) where
\end{code}
\subsection{导入}
 Yesod。
\begin{code}
        import Yesod
\end{code}
处理 Monad。
\begin{code}
        import Control.Monad.Trans.Resource
\end{code}
处理 Config
\begin{code}
        import Yrarbil.Backend.Config
\end{code}

是否是“认证”的客户端。
\begin{code}
        isUserAgnetRight :: MonadResourceBase m => HandlerT site m Bool
        isUserAgnetRight = liftHandlerT (lookupHeaders "User-Agent") >>= (\xs -> return $ or $ map (\x-> elem x xs) ["YrarbilBackend-testclient"])
\end{code}
