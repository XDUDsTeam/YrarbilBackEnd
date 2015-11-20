




%Main.lhs
%包含导言文件
%\input{preamble}

\ignore{


> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE QuasiQuotes #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}


> module Main
>       (
>         main
>       ) where

}

\subsection{导入}
需要 Yesod 。
\begin{code}
        import        Yesod
\end{code}

\subsection{定义主程序类型}
HelloWorld 类型。
\begin{code}
        data HelloWorld = HelloWorld
        instance Yesod HelloWorld where
\end{code}

\subsection{Yesod 路由}
Yesod 路由表。
\begin{code}
        mkYesod "HelloWorld" [parseRoutes|
        / HomeR GET
        |]
\end{code}
\subsection{访问主页}
主页由\textbf{getHomeR}生成。
\begin{code}
        getHomeR :: Handler Html
        getHomeR = defaultLayout [whamlet|Hello,World!|]
\end{code}

\subsection{主函数}
主函数。,访问 \href{http://localhost:3000/}{端口为3000的本地地址}，可看到。。。
\begin{code}
        main :: IO()
        main = warp 3000 HelloWorld
\end{code}
