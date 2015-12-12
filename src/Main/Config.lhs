




% Main/Config.lhs
% 设置 User-Agent

%导入导言区
\input{preamble}
\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
\end{code}
\subsection{模块}
\begin{code}
module Main.Config
    ( isUserAgnetRight
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
\subsection{YrarbilBackend 访问}
测试后端

是否是“认证”的客户端。
\begin{code}
        isUserAgnetRight :: MonadResourceBase m => HandlerT site m Bool
        isUserAgnetRight = liftHandlerT (lookupHeaders "User-Agent") >>= (\xs -> return $ or $ map (\x-> elem x xs) ["YrarbilBackend-testclient"])          
\end{code}
