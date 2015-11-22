




%Info.lhs
%网页获取帮助文档-SubSite
%API 0.0.1.0-base/未分类 的内容
%包含导言文件


\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
\end{code}

\subsection{模块 Info}
\begin{code}
module Info
      ( module Info.Data
      , module Info
      ) where
\end{code}

\subsection{导入}
Yesod 基本内容与子站。
\begin{code}
        import Yesod
        import Info.Data
\end{code}
JSON 处理。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的支持.
\begin{code}
        import Prelude hiding(words)
        import Data.Text.Lazy hiding(head)
        import Data.Text.Lazy.Encoding(decodeUtf8)
        import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
\end{code}
Persistent & PostgreSQL
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
transformers:
\begin{code}
        import Control.Monad.Trans.Reader
\end{code}
monad-logger:
\begin{code}
        import Control.Monad.Logger
\end{code}

\subsection{数据库部分}
Persistent 的处理，是的可用数据库。
\begin{code}
        instance YesodPersist Information where
          type YesodPersistBackend Information = SqlBackend
          runDB action = do
            Information pool <- getYesod
            runSqlPool action pool
\end{code}
$^{\tiny{Template Haskell}}$
数据库 api 版本的。指向数据库中 表 table\_version
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
        DbApiVer json sql=table_version
          Id     sql=
          main   Int
          snd    Int
          trd    Int
          fix    Int
          tag    Text
          Primary main snd trd fix tag
          deriving Show Eq
        |]
\end{code}
获得数据库 API 版本的查询。
\begin{code}
        getDbApiVer :: ReaderT SqlBackend (HandlerT site IO) [Entity DbApiVer]
        getDbApiVer = selectList [] []
\end{code}

\subsection{处理 GET 请求的函数}
\begin{code}
        getInfoR :: Yesod master
                 => HandlerT Information (HandlerT master IO) Text
        getInfoR = do
          ver <- liftHandlerT $ runDB getDbApiVer
          let vers = Prelude.map lam ver
          pa <- lookupGetParam "nojson"
          case fromMaybe "false" pa of
              "false" -> jsonOne vers
              _ -> htmlOne vers
          where
              jsonOne :: Yesod master
                       => [DbApiVer] -> HandlerT Information (HandlerT master IO) Text
              jsonOne vers= do
                liftHandlerT $ addHeader "Content-Type" "application/json"
                return $ decodeUtf8 $ encode $ object
                  [ "BackendAPIVersion" .= beApiVerData
                  , "DatabaseAPIVersion" .= vers
                  ]
              htmlOne :: Yesod master
                       => [DbApiVer] -> HandlerT Information (HandlerT master IO) Text
              htmlOne vers = do
                liftHandlerT $ addHeader "Content-Type" "text/html"
                liftHandlerT $ addHeader "Accept-Charset" "utf-8"
                return $ decodeUtf8 $ renderHtml [shamlet|
                    $doctype 5
                    <html>
                      <head>
                        <meta charset="utf-8">
                      <body>
                        <h1>数据库支持的 API
                        <ul>
                          $forall (DbApiVer aa ba ca da ea) <- vers
                            <li> #{aa}.#{ba}.#{ca}.#{da}-#{ea}
                        <h1>后端支持的 API
                        <ul>
                          $forall (BeApiVer m s t f ta) <- beApiVerData
                            <li> #{m}.#{s}.#{t}.#{f}-#{ta}
                  |]
                --return "<html><body><h1>d</h1></body></html>"
              lam (Entity _ (DbApiVer a b c d e)) = DbApiVer a b c d $ head $ words e
        instance Yesod master => YesodSubDispatch Information (HandlerT master IO) where
          yesodSubDispatch = $(mkYesodSubDispatch resourcesInformation)

\end{code}

\subsection{后端支持的 API 版本}
数据类型定义。
\begin{code}
        data BeApiVer = BeApiVer Int Int Int Int Text
          deriving (Eq,Show)
        instance ToJSON BeApiVer where
          toJSON (BeApiVer m s t f ta) = object
            [ "main" .= m
            , "snd" .= s
            , "trd" .= t
            , "fix" .= f
            , "tag" .= ta
            ]
\end{code}



数据。
\begin{code}
        beApiVerData :: [BeApiVer]
        beApiVerData =
          [ BeApiVer 0 0 0 0 "tag"
          , BeApiVer 0 0 0 0 "B"
          , BeApiVer 0 0 1 0 "base"
          ]
\end{code}
