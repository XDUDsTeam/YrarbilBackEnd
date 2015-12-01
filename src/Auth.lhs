




%Auth.lhs
% 管理员与读者的用户的的认证
% 0.1.0.0-base 用户认证

%包含导言区
\input{preamble}
\subsection{使用的特性}
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
\end{code}
\subsection{模块}
\begin{code}
module Auth
      ( module Auth
      , module Auth.Data
      , module Data.Time
      ) where
\end{code}
\subsection{导入}
导入 Yesod 与 Auth.Data
\begin{code}
        import Yesod
        import Yesod.Auth
        import Auth.Data
\end{code}
导入 aeson 处理 JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的支持.
\begin{code}
        import Prelude hiding(words,length,concat,splitAt)
        import Data.Text.Lazy hiding(head,read)
        import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
        import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
        import Data.Text.Internal (showText)
        import qualified Data.Text.Internal as DTI(showText,Text)
        import Data.Text.Internal.Encoding.Utf8()
        import qualified Data.String as DS
\end{code}
Persistent \& PostgreSQL
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
时间类数据
\begin{code}
        import Data.Time
\end{code}
设置延迟
\begin{code}
        import Control.Concurrent(threadDelay)
\end{code}
MD5 与 SHA256 加密
\begin{code}
        import Data.Digest.Pure.MD5
        import Data.Digest.Pure.SHA
\end{code}

\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist Auther where
          type YesodPersistBackend Auther = SqlBackend
          runDB a = do
            Auther p <- getYesod
            runSqlPool a p
\end{code}
数据库 中管理员的表 table\_adminer。
数据库 中读者的表 table\_reader。
临时 ID 秘钥表 table\_tmpidkey。
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Adminer json sql=table_adminer
            Id sql=
            uid Int sql=admin_id
            passwd Text sql=admin_passwd
            Primary uid
            deriving Show Eq
          Readert json sql=table_reader
            Id sql=
            barcode Text
            name Text sql=reader_name
            idcardtype Text sql=idcard_type
            idcardid Text sql=idcard_id
            debt Double sqltype=money
            password Text sql=enter_password
            Primary barcode
            deriving Show Eq
          Tid json sql=table_tmpidkey
            Id sql=
            tid Text sql=tmpid
            time UTCTime sql=timeend
            uid Text sql=id
            Primary tid
            deriving Show Eq
          |]
\end{code}

\subsection{请求处理}
对于 管理员登陆的函数。
\begin{code}
        postAdmininR :: Yesod master
                     => Text
                     -> Text
                     -> HandlerT Auther (HandlerT master IO) Text
        postAdmininR _ _= do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          adminUid <- lookupPostParam "uid"
          passwd <- lookupPostParam "key"
          if adminUid == Nothing || passwd == Nothing then do
            return $ decodeUtf8 $ encode $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("no admin\'s id or password" ::String)
              ]
            else do
              let uid = (read.show.(\(Just x)->x)) adminUid ::String
              let pw = (read.show.(\(Just x) ->x)) passwd ::String
              let searchUid = toInt uid :: Int
              rt' <- liftHandlerT $ runDB $ selectList [AdminerUid ==. searchUid] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then do
                return $ decodeUtf8 $ encode $ object
                  [ "status" .= ( "failed" ::String)
                  , "reason" .=( "no such a admin" ::String)
                  ]
                else do
                  let (Adminer _ adpw) = head rt
\end{code}
此处 设置延迟 10秒，用于防止暴力破解密码.
\begin{code}
                  if le adpw /= pw then do
                    liftIO $ threadDelay 10000000
                    return $ decodeUtf8 $ encode $ object
                      [ "status" .= ("failer" ::String)
                      , "reason" .= ("password error" ::String)
                      ]
                      else do
                      time <- liftIO $ getCurrentTime
                      let endtime = addUTCTime (8*60*60) time
                      let tk = show uid ++ show pw ++ show time
                      let rtTidKeyMD5 = pack $ show $ md5 $ encodeUtf8 $ pack tk
                      let rtTidKeySHA = pack $ show $ sha256 $ encodeUtf8 $ pack tk
                      let tt = concat [ fst $ splitAt (quot (length rtTidKeyMD5) 2) rtTidKeyMD5,snd $ splitAt (quot (length rtTidKeySHA) 2) rtTidKeySHA ]
                      liftHandlerT $ runDB $ insert $ Tid tt endtime $ pack uid
                      return $ decodeUtf8 $ encode $ object
                        [ "status" .= ("success" ::String)
                        , "tidk" .= tt
                        , "time" .= show  endtime
                        ]
          where
            lam (Entity _ x) = x
            toInt :: String -> Int
            toInt = Prelude.read
            le =  show.md5.encodeUtf8
\end{code}

用户登陆。
\begin{code}
        postReaderinR :: Yesod master
                     => Text
                     -> Text
                     -> HandlerT Auther (HandlerT master IO) Text
        postReaderinR _ _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          usruid <- lookupPostParam "uid"
          passwd <- lookupPostParam "password"
          if passwd == Nothing || usruid == Nothing then do
            returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("no reader id or null password" ::String)
              ]
            else do
              let uid = (read.show.(\(Just x)->x)) usruid ::String
              let pw = (read.show.(\(Just x) -> x)) passwd :: String
              let searchid = pack uid
              rt' <- liftHandlerT $ runDB $ selectList [ReadertBarcode ==. searchid] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then do
                returnTJson $ object
                  [ "status" .= ("failed" ::String)
                  , "reason" .= ("no such a user" ::String)
                  ]
                else do
                  let upw = (\(Readert _ _ _ _ _ p)->p) $ head rt
                  if le upw /= pw then do
                    liftIO $ threadDelay 10000000
                    return $ decodeUtf8 $ encode $ object
                      [ "status" .= ("failer" ::String)
                      , "reason" .= ("password error" ::String)
                      ]
                    else do
                      time <- liftIO $ getCurrentTime
                      let endtime = addUTCTime (60*60) time
                      let tk = show uid ++ show pw ++ show time
                      let rtTidKeyMD5 = pack $ show $ md5 $ encodeUtf8 $ pack tk
                      let rtTidKeySHA = pack $ show $ sha256 $ encodeUtf8 $ pack tk
                      let tt = concat [ fst $ splitAt (quot (length rtTidKeyMD5+4) 2) rtTidKeyMD5,snd $ splitAt (quot (length rtTidKeySHA+4) 2) rtTidKeySHA ]
                      liftHandlerT $ runDB $ insert $ Tid tt endtime $ pack uid
                      return $ decodeUtf8 $ encode $ object
                        [ "status" .= ("success" ::String)
                        , "tidk" .= tt
                        , "time" .= show  endtime
                        ]
          where
            lam (Entity _ x) = x
            le = show.md5.encodeUtf8

\end{code}
管理员登出
\begin{code}
        postAdminoutR :: Yesod master
                      => Text
                      -> Text
                      -> HandlerT Auther (HandlerT master IO) Text
        postAdminoutR _ _ = do
          tid' <- lookupPostParam "tidk"
          uid' <- lookupPostParam "uid"
          case (tid',uid') of
            (Nothing,Nothing) -> returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("null param" ::String)
              ]
            (Just tid,Nothing) -> do
              liftHandlerT $ runDB $ deleteWhere [TidTid ==. toIt tid]
              returnTJson $ object
                ["status" .= ("success" ::String)]
            (Nothing,Just uid) -> do
              liftHandlerT $ runDB $ deleteWhere [TidUid ==. toIt uid]
              returnTJson $ object
                ["status" .= ("success" ::String)]
            (Just tid,Just uid) -> do
              liftHandlerT $ runDB $ deleteWhere ([TidTid ==. toIt tid] ||. [TidUid ==. toIt uid])
              returnTJson $ object
                ["status" .= ("success" ::String)]
          where
            toIt = pack.read.show
\end{code}
 读者登出
\begin{code}
        postReaderoutR :: Yesod master
                        => Text
                        -> Text
                        -> HandlerT Auther (HandlerT master IO) Text
        postReaderoutR _ _ = do
          tid' <- lookupPostParam "tidk"
          uid' <- lookupPostParam "uid"
          case (tid',uid') of
            (Nothing,Nothing) -> returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("null param" ::String)
              ]
            (Just tid,Nothing) -> do
              liftHandlerT $ runDB $ deleteWhere [TidTid ==. toIt tid]
              returnTJson $ object
                ["status" .= ("success" ::String)]
            (Nothing,Just uid) -> do
              liftHandlerT $ runDB $ deleteWhere [TidUid ==. toIt uid]
              returnTJson $ object
                ["status" .= ("success" ::String)]
            (Just tid,Just uid) -> do
              liftHandlerT $ runDB $ deleteWhere ([TidTid ==. toIt tid] ||. [TidUid ==. toIt uid])
              returnTJson $ object
                ["status" .= ("success" ::String)]
          where
            toIt = pack.read.show
\end{code}
用于各个网页的认证部分。
\begin{spec}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Tid json sql=table_tmpidkey
            Id sql=
              tid Text sql=tmpid
              time UTCTime sql=timeend
              uid Text sql=id
              Primary tid
              deriving Show Eq
          |]
        authAdminGet :: Yesod master => HandlerT Auther (HandlerT master IO) Bool
        authAdminGet = do
          tidk' <- lookupGetParam "tidk"
          if tidk' == Nothing then return False
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return False
                else do
                  let (Tid _ time _) = head rt
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return False
                    else return True
          where
            lam (Entity _ x) = x

        authAdminPost :: Yesod master => HandlerT Auther (HandlerT master IO) Bool
        authAdminPost = do
          tidk' <- lookupPostParam "tidk"
          if tidk' == Nothing then return False
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return False
                else do
                  let (Tid _ time _) = head rt
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return False
                    else return True
          where
            lam (Entity _ x) = x

        authReaderGet :: Yesod master => HandlerT Auther (HandlerT master IO) Bool
        authReaderGet = do
          tidk' <- lookupGetParam "tidk"
          if tidk' == Nothing then return False
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return False
                else do
                  let (Tid _ time _) = head rt
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow <0 then return False
                    else return True
          where
            lam (Entity _ x) = x

        authReaderPost ::  Yesod master => HandlerT Auther (HandlerT master IO) Bool
        authReaderPost = do
          tidk' <- lookupPostParam "tidk"
          if tidk' == Nothing then return False
            else do
              let tidk = (pack.read.show.(\(Just x)->x)) tidk'
              let sqlQ = DS.fromString $ "SELECT * FROM table_tmpidkey WHERE tmpid=" ++ unpack tidk ::DTI.Text
              rt' <- liftHandlerT $ runDB $ selectList [TidTid ==. tidk] []
              let rt = Prelude.map lam rt'
              if Prelude.null rt then return False
                else do
                  let (Tid _ time _) = head rt
                  tnow <- liftIO $ getCurrentTime
                  if diffUTCTime time tnow < 0 then return False
                    else return True
          where
            lam (Entity _ x) = x
\end{spec}
ReturnTextValue
\begin{code}
        returnTJson :: Monad m =>  Value -> HandlerT site m Text
        returnTJson = return.decodeUtf8.encode
\end{code}
实现 YesodSubDispatch
\begin{code}
        instance Yesod master => YesodSubDispatch Auther (HandlerT master IO) where
          yesodSubDispatch = $(mkYesodSubDispatch resourcesAuther)
\end{code}
Data.Text.Internal.Text => Data.Lazy.Text
\begin{code}
        t2t :: DTI.Text -> Text
        t2t = pack.showText
\end{code}
