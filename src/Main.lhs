




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
导入 Sql 设置与其他配置。
\begin{code}
        import    Main.Config
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
导入子站 - 图书的销毁与录入。
\begin{code}
        import    AddDel
\end{code}
Data.Text
\begin{code}
        import Data.String
        import Data.Text.Lazy hiding (null)
        import Data.Text as DT (unpack)
\end{code}
设置延迟
\begin{code}
        import Control.Concurrent(threadDelay)
\end{code}
处理 文件夹。
\begin{code}
        import System.Directory
\end{code}

\subsection{定义主程序类型}
YrabrilBackEnd 后端 主数据
\begin{code}
        data YrarbilBackend = YrarbilBackend
          { connPool :: ConnectionPool
          , getInformation :: Information
          , getAuther :: Text->Auther
          , getManagement :: Text -> Management
          , getAddDel :: Text -> AddDel
          }
\end{code}

\subsection{数据库}
\begin{code}
        instance YesodPersist YrarbilBackend where
          type YesodPersistBackend YrarbilBackend = SqlBackend
          runDB a = do
            YrarbilBackend p _ _ _ _ <- getYesod
            runSqlPool a p
\end{code}




\subsection{Yesod 路由}
Yesod 路由表。
\begin{code}
        mkYesod "YrarbilBackend" [parseRoutes|
        / HomeR GET
        /version SubsiteVR Information getInformation
        /1daa62b/#Text SubsiteAR Auther getAuther
        /a3cab3a/#Text SubsiteMR Management getManagement
        /c7b70ceb/#Text SubadddelDR AddDel getAddDel
        /favicon.ico FaviconR GET
        |]
\end{code}
YrarbilBackend 实现 Yesod 类型类。
\begin{code}
        instance Yesod YrarbilBackend where
\end{code}
设置 错误句柄 的函数。
\begin{description}
\item[NotFound] 404，找不见页面。
\end{description}
\begin{code}
          errorHandler NotFound= selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("not found" ::Text)
              ]
\end{code}
\begin{description}
\item[NotAuthenticated] 没有权限。
\end{description}
\begin{code}
          errorHandler NotAuthenticated = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("not logged in" ::Text)
              ]
\end{code}
\begin{description}
\item[PermissionDenied] 没有权限。
\end{description}
\begin{code}
          errorHandler (PermissionDenied msg) = selectRep$ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("PermissionDenied" ::Text)
              , "msg" .= msg
              ]
\end{code}
\begin{description}
\item[InvalidArgs] 参数错误。
\end{description}
\begin{code}
          errorHandler (InvalidArgs ia) = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("InvalidArgs" ::Text)
              , "args" .= ia
              ]
\end{code}
\begin{description}
\item[BadMethod] HTTP 请求方式错误。
\end{description}
\begin{code}
          errorHandler (BadMethod _) = selectRep $ provideRep $ do
            liftIO $ threadDelay 10000000
            liftHandlerT $ addHeader "Content-Type" "application/json"
            returnTJson $ object
              [ "status" .= ("error" ::Text)
              , "reason" .= ("BadMethod" ::Text)
              ]
\end{code}
\begin{description}
\item[InternalError] 交互错误。
\end{description}
\begin{code}
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
          isAuthorized FaviconR _ = return Authorized
          isAuthorized (SubsiteVR _) _ = return Authorized
          isAuthorized (SubsiteAR _ (AdmininR _ _)) _ = return Authorized
          isAuthorized (SubsiteAR _ (ReaderinR _ _)) _ = return Authorized
          isAuthorized (SubsiteMR _ _) _ = postAuthTidk
          isAuthorized (SubsiteAR _ _) _ = postAuthTidk
          isAuthorized (SubadddelDR _ _) _ = postAuthTidk 
          isAuthorized _ _ = getAuthTidk
\end{code}
post、get 获得 tidk 的函数。
\begin{code}
        postAuthTidk,getAuthTidk :: HandlerT YrarbilBackend IO AuthResult
        postAuthTidk  = do
          tidk' <- lookupPostParam "tidk"
          if tidk' == Nothing then return $ Unauthorized ":("
            else do
              let tidk = (fromString.DT.unpack.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [Auth.TidTid ==. tidk] []
              if Prelude.null rt' then return $ Unauthorized ":("
                else do
                  let (Auth.Tid _ time _) = lam $ Prelude.head rt'
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return $ Unauthorized ":("
                    else return Authorized
          where
            lam (Entity _ x) = x
        getAuthTidk = do
          tidk' <- lookupGetParam "tidk"
          if tidk' == Nothing then return $ Unauthorized ":("
            else do
              let tidk = (fromString.DT.unpack.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [Auth.TidTid ==. tidk] []
              if Prelude.null rt' then return $ Unauthorized ":("
                else do
                  let (Auth.Tid _ time _) = lam $ Prelude.head rt'
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return $ Unauthorized ":("
                    else return Authorized
          where
            lam (Entity _ x) = x
\end{code}
\subsection{访问主页}
主页由\textbf{getHomeR}生成。
\begin{code}
        getHomeR :: HandlerT YrarbilBackend IO Html
        getHomeR = do
          right <- liftHandlerT $ isUserAgnetRight
          defaultLayout [whamlet|
            $doctype 5
            <h1> Yrarbil Backend "Home"
            $if right
              <h3> 你正在使用一个正常的浏览器 或 工具 访问 或 获取信息
            $else
              <h2> 你正在使用一个老掉牙，恶心，破旧，与现代社会不相符的浏览器或工具 访问我们的后端。有能力换个好的！
            <br>
            <p> 欢迎访问 Yrarbil 的后端，当前版本为测试版本。没事别调戏，管理员可以看见您的 IP 地址等信息。
              同时我们保留通过一切合法形式维护我们权益的权利。
            <p> Yrarbil 是西安电子科技大学 软件学院 2014级 本科生 数据结构课程的大作业。
            <p> Yrarbil 项目 遵循 BSD－3 开源协议，但是我们也有计划更换这一协议。
            <p> 可以在 GitHub 上找到我们的项目，我们的 项目主页是 http://yrarbil.iok.la 。
            <P> 可以在 YrarbilRelease 中找到我们的项目中发布的文档甚至是打包好的源代码、二进制代码。
            |]
\end{code}

\subsection{主函数}
主函数。,访问 \href{http://localhost:3000/}{端口为3000的本地地址}
\footnote{端口号，需要通过启动器输入，不一定会是 $3000$。使用Docker 版本的启动器，端口应该在设置 环境变量中设置 $ YRARBIL_BACKEND_PORT$}
，可看到。。。
\begin{code}
        main :: IO()
        main = do
          config <- getConfig
          case toConfigT config of
            Just (st,lmt,p) -> do
              runStderrLoggingT $ withPostgresqlPool st lmt $
                \pool ->liftIO $
                warp p $ YrarbilBackend pool
                  (Information pool)
                  (\_->Auther pool)
                  (\_ -> Management pool)
                  (\_ -> AddDel pool)
            Nothing -> error "error config"
\end{code}

\subsection{icons}
\begin{code}
        getFaviconR :: HandlerT YrarbilBackend IO ()
        getFaviconR = do
          path <- liftIO $ getAppUserDataDirectory "yb"
          sendFile "application/x-ico" $ path ++ "/favicon.ico"
\end{code}