




%Main.lhs
%包含导言文件
%\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
\end{code}

\subsection{模块 Main}
\begin{code}
module Main where
\end{code}


\subsection{导入}
需要 Yesod 。
\begin{code}
        import        Yesod
        import        Yesod.Auth
\end{code}
Persistent \& Postgresql
\begin{code}
        import    Database.Persist
        import    Database.Persist.TH
        import    Database.Persist.Postgresql
\end{code}
monad-logger。
\begin{code}
        import    Control.Monad.Logger
\end{code}
导入 Sql 设置。
\begin{code}
        import    Main.SqlConnection
\end{code}
导入子站-Version（Information）。
\begin{code}
        import    Info
\end{code}
导入子站-认证（Auther）。
\begin{code}
        import    Auth
\end{code}
Data.Text
\begin{code}
        import Data.Text.Lazy
\end{code}

\subsection{定义主程序类型}
YrabrilBackEnd 后端 主数据
\begin{code}
        data YrarbilBackEnd = YrarbilBackEnd
          { connPool :: ConnectionPool
          , getInformation :: Information
          , getAuther :: Text->Auther
          }
\end{code}

\subsection{数据库}
\begin{code}
        instance YesodPersist YrarbilBackEnd where
          type YesodPersistBackend YrarbilBackEnd = SqlBackend
          runDB a = do
            YrarbilBackEnd p _ _ <- getYesod
            runSqlPool a p
\end{code}




\subsection{Yesod 路由}
Yesod 路由表。
\begin{code}
        mkYesod "YrarbilBackEnd" [parseRoutes|
        / HomeR GET
        /version SubsiteVR Information getInformation
        /1daa62b/#Text SubsiteAR Auther getAuther
        |]
        instance Yesod YrarbilBackEnd where
          isAuthorized (SubsiteAR _ (AdmininR _ _)) _ = return Authorized
          isAuthorized (SubsiteAR _ (ReaderinR _ _)) _ = return Authorized
          isAuthorized (SubsiteAR _ _) _ = do
            tidk' <- lookupPostParam "tidk"
            if tidk' == Nothing then return $ Unauthorized ":("
              else do
                let tidk = (pack.read.show.(\(Just x)->x)) tidk'
                rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
                let rt = Prelude.map lam rt'
                if Prelude.null rt then return $ Unauthorized ":("
                  else do
                    let (Tid _ time _) = Prelude.head rt
                    tnow <- liftIO $ getCurrentTime
                    if diffUTCTime time tnow <0 then return $ Unauthorized ":("
                      else return Authorized
            where
              lam (Entity _ x) = x

          isAuthorized _ _ = do
            tidk' <- lookupGetParam "tidk"
            if tidk' == Nothing then return $ Unauthorized ":("
              else do
                let tidk = (pack.read.show.(\(Just x)->x)) tidk'
                rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
                let rt = Prelude.map lam rt'
                if Prelude.null rt then return $ Unauthorized ":("
                  else do
                    let (Tid _ time _) = Prelude.head rt
                    tnow <- liftIO $ getCurrentTime
                    if diffUTCTime time tnow <0 then return $ Unauthorized ":("
                      else return Authorized
            where
              lam (Entity _ x) = x
\end{code}
\subsection{访问主页}
主页由\textbf{getHomeR}生成。
\begin{code}
        getHomeR :: HandlerT YrarbilBackEnd IO Html
        getHomeR = do
          defaultLayout [whamlet|
                Hello,wordld!
            |]
\end{code}

\subsection{主函数}
主函数。,访问 \href{http://localhost:3000/}{端口为3000的本地地址}，可看到。。。
\begin{code}
        main :: IO()
        main = do
          (st,lmt) <- getSqlConn "sqlserver.cfg"
          runStderrLoggingT $ withPostgresqlPool st lmt $
            \pool ->liftIO $
              warp 3000 $ YrarbilBackEnd pool (Information pool) (\_->Auther pool)
\end{code}
