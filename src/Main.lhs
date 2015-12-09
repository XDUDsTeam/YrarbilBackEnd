




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
导入 Common。
\begin{code}
        import Common
\end{code}
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
导入子站 - 借阅 Management。
\begin{code}
        import    Management
\end{code}
Data.Text
\begin{code}
        import Data.Text.Lazy
\end{code}
设置延迟
\begin{code}
        import Control.Concurrent(threadDelay)
\end{code}

\subsection{定义主程序类型}
YrabrilBackEnd 后端 主数据
\begin{code}
        data YrarbilBackEnd = YrarbilBackEnd
          { connPool :: ConnectionPool
          , getInformation :: Information
          , getAuther :: Text->Auther
          , getManagement :: Text -> Management
          }
\end{code}

\subsection{数据库}
\begin{code}
        instance YesodPersist YrarbilBackEnd where
          type YesodPersistBackend YrarbilBackEnd = SqlBackend
          runDB a = do
            YrarbilBackEnd p _ _ _ <- getYesod
            runSqlPool a p
\end{code}




\subsection{Yesod 路由}
Yesod 路由表。
\begin{code}
        mkYesod "YrarbilBackEnd" [parseRoutes|
        / HomeR GET
        /version SubsiteVR Information getInformation
        /1daa62b/#Text SubsiteAR Auther getAuther
        /a3cab3a/#Text SubsiteMR Management getManagement
        |]
\end{code}
YrarbilBackend 实现 Yesod 类型类。
\begin{code}
        instance Yesod YrarbilBackEnd where
\end{code}
设置 错误句柄 的函数。
\begin{description}
\item[NotFound] 404，找不见页面。
\item[NotAuthenticated] 没有权限。
\item[PermissionDenied] 没有权限。
\item[InvalidArgs] 参数错误。
\item[BadMethod] HTTP 请求方式错误。
\item[InternalError] 交互错误。
\end{description}
\begin{code}
          errorHandler NotFound= selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("not found" ::Text)
              ]
          errorHandler NotAuthenticated = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("not logged in" ::Text)
              ]
          errorHandler (PermissionDenied msg) = selectRep$ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("PermissionDenied" ::Text)
              , "msg" .= msg
              ]
          errorHandler (InvalidArgs ia) = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("InvalidArgs" ::Text)
              , "args" .= ia
              ]
          errorHandler (BadMethod _) = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("BadMethod" ::Text)
              ]
          errorHandler (InternalError t) = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("InternalError" ::Text)
              , "msg" .= t
              ]
\end{code}
访问权限设置。
\begin{code}
          isAuthorized HomeR  _ = return Authorized
          isAuthorized (SubsiteVR _) _ = return Authorized
          isAuthorized (SubsiteAR _ (AdmininR _ _)) _ = return Authorized
          isAuthorized (SubsiteAR _ (ReaderinR _ _)) _ = return Authorized
          isAuthorized (SubsiteAR _ _) _ = postAuthTidk
          isAuthorized _ _ = getAuthTidk
\end{code}
post、get 获得 tidk 的函数。
\begin{code｝
        postAuthTidk,getAuthTidk :: HandlerT YrarbilBackEnd IO AuthResult
        postAuthTidk  = do
          tidk' <- lookupPostParam "tidk"
          if tidk' == Nothing then return $ Unauthorized ":("
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [Auth.TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return $ Unauthorized ":("
                else do
                  let (Auth.Tid _ time _) = Prelude.head rt
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return $ Unauthorized ":("
                    else return Authorized
          where
            lam (Entity _ x) = x
        getAuthTidk = do
          tidk' <- lookupGetParam "tidk"
          if tidk' == Nothing then return $ Unauthorized ":("
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [Auth.TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return $ Unauthorized ":("
                else do
                  let (Auth.Tid _ time _) = Prelude.head rt
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
          sqlC <- getSqlConn
          case toConConfig sqlC of
            Just (st,lmt) -> do
              runStderrLoggingT $ withPostgresqlPool st lmt $
                \pool ->liftIO $
                warp 3000 $ YrarbilBackEnd pool (Information pool) (\_->Auther pool) (\_ -> Management pool)
            Nothing -> error "error config"
\end{code}
