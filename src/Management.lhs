




% 图书借阅管理
% Management.lhs

% 导入导言区
\input{preamble}

\subsection{特性}
\begin{code}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
\end{code}

\subsection{模块 Management}
\begin{code}
module Management
      ( module Management
      , module Management.Data
      ) where
\end{code}

\subsection{导入}
导入 Yesod 、 Common 与 Management.Data。
\begin{code}
        import Yesod
        import Management.Data
        import Common
\end{code}
处理JSON。
\begin{code}
        import Data.Aeson
\end{code}
处理 Maybe。
\begin{code}
        import Data.Maybe
\end{code}
处理 Either。
\begin{code}
        import Data.Either
\end{code}
对 Text 的支持.
\begin{code}
        import Prelude hiding(words,length,concat,splitAt)
        import qualified Prelude as P
        import Data.Text.Lazy hiding(head,read,null)
        import Data.Text.Internal (showText)
        import Data.Text.Lazy.Encoding(decodeUtf8,encodeUtf8)
\end{code}
Persistent \& PostgreSQL
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
处理时间。
\begin{code}
        import Data.Time
\end{code}
对数字字符的处理。
\begin{code}
        import Data.Char(isDigit)
\end{code}
\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist Management where
          type YesodPersistBackend Management = SqlBackend
          runDB a = do
            Management p <- getYesod
            runSqlPool a p
\end{code}
数据库的表。
\begin{description}
\item[图书信息表] table_bookinfo
\item[图书实体表] table_bookitem
\item[图书操作表] table_bookopt_main
\item[处罚记录表] table_punish
\item[读者数据表] table_reader
\item[操作记录] table_opt
\end{description}
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Bookinfo json sql=table_bookinfo
            Id sql=
            isbn Int
            name Text Maybe sql=bookname
            author Text Maybe
            plocal Text Maybe sql=publish_local
            phouse Text Maybe sql=publish_house
            pdate Day Maybe sql=publish_date
            llocal Text Maybe sql=library_local
            llindex Text Maybe sql=library_index
            Primary isbn
            deriving Eq Show
          Bookitem json sql=table_bookitem
            Id sql=
            barcode Int
            isbn Int
            onshelf Bool sql=on_shelf
            there Bool sql=is_there
            lastid Text Maybe sql=latest_opt_id
            bprice Int Maybe sql=bought_price
            bdate Day Maybe sql=bought_date
            Primary barcode
            deriving Eq Show
          Bookopt json sql=table_bookopt_main
            Id sql=
            bsm Text sql=big_serial_number
            rbc Text sql=reader_barcode
            bbc Int sql=book_barcode
            times Int
            rtdate Day Maybe sql=return_date
            isrt Bool sql=is_return
            Primary bsm
            deriving Eq Show
          Punish json sql=table_punish
            Id sql=
            bsm Text sql=big_serial_number
            rbc Text sql=reader_barcode
            cash Double sqltype=money
            reason Text
            Primary bsm
            deriving Eq Show
          Reader json sql=table_reader
            Id sql=
            barcode Text
            idctp Text Maybe sql=idcard_type
            idcid Text Maybe sql=idcard_id
            debt Double sqltype=money
            epw Text sql=enter_password
            Primary barcode
            deriving Eq Show
          Opt json sql=table_opt
            Id sql=
            ssm Int sql=small_serial_number
            date Day sql=opt_date
            usrtp Int sql=opt_usr_type
            usrid Text sql=opt_usr_id
            Primary ssm date
            deriving Eq Show
          Tid json sql=table_tmpidkey
            Id sql=
            tid Text sql=tmpid
            time UTCTime sql=timeend
            uid Text sql=id
            Primary tid
            deriving Show Eq
          |]
\end{code}
图书借出的处理函数。
\begin{code}
        postBooklendR :: Yesod master
                      => Text
                      -> HandlerT Management (HandlerT master IO) Text
        postBooklendR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          rId <- lookupPostParam "rid"
          bId <- lookupPostParam "bid"
          case (rId,bId) of
            (Just rid,Just bid') -> let bid = read $ read $ show bid' in
                                   isBookOs bid True $
                                   checkBC (t2t rid) $
                                   lendBook (t2t rid) bid
\end{code}
如果参数不齐，则返回错误。
\begin{code}
            _ -> do
              returnTJson $ object
                [ "status" .= ("failed" ::String)
                , "reason" .= ("No reader's id or book's id(barcode)." ::String)
                ]
          where
\end{code}
借阅图书。
\begin{code}
            lendBook :: Yesod master
                     => Text
                     -> Int
                     -> HandlerT Management (HandlerT master IO) Text
            lendBook rid bid = do
              time <- liftIO $ getCurrentTime
              tidk' <- lookupPostParam "tidk"
              let (Just tidk) = tidk'
              (Entity _ (Tid _ _ uid):_) <- liftHandlerT $ runDB $ selectList [TidTid ==. t2t tidk] []
              sn <- liftIO $ getRandom
              liftHandlerT $ runDB $ insert $ Opt sn (utctDay time) 0 uid
              liftHandlerT $ runDB $ insert $
                Bookopt
                  (pack (show sn ++ "@" ++ show time))
                  rid
                  bid
                  1
                  (Just $ addDays 30 $ utctDay time)
                  False
              liftHandlerT $ runDB $ updateWhere [BookitemBarcode ==. bid] [BookitemOnshelf =. False]
              returnTJson $ object
                [ "status" .= ("success" ::String)
                , "date" .= (addDays 30 $ utctDay time)
                ]
\end{code}
一次可以借阅 15 本。
\begin{code}
        checkBC :: Yesod master
                    => Text
                    -> HandlerT Management (HandlerT master IO) Text
                    -> HandlerT Management (HandlerT master IO) Text
        checkBC rid a = do
              bs <- liftHandlerT $ runDB $ selectList
                [BookitemLastid ==. (Just rid)] []
              if P.length bs >= 15
                then returnTJson $ object
                  [ "status" .= ("failed" ::String)
                  , "reason" .= ("Can not lend more!"::String)
                  ]
                else a
\end{code}
检查图书是否在架。
\begin{code}
        isBookOs :: Yesod master
                 => Int
                 -> Bool -- True 不在架 False 在架
                 -> HandlerT Management (HandlerT master IO) Text
                 -> HandlerT Management (HandlerT master IO) Text
        isBookOs bc b a = do
          ioshelf <- liftHandlerT $ runDB $ selectList
            [BookitemBarcode ==. bc,BookitemOnshelf ==. True,BookitemThere ==. True] []
          if null ioshelf == b
            then returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("The book is not on the shelf or there." ::String)
              ]
            else a
\end{code}
查看是否有超期。
\begin{code}
        isOD :: Entity Bookopt -> Day -> Bool
        isOD (Entity _ (Bookopt _ _ _ _ (Just rd) _)) day = diffDays rd day < 0
        isOD _ _ = False
        isHasOverDate :: Yesod master
                      => Text
                      -> HandlerT Management (HandlerT master IO) Text
                      -> HandlerT Management (HandlerT master IO) Text
        isHasOverDate rid a = do
          rt' <- liftHandlerT $ runDB $ selectList [BookoptRbc ==. rid,BookoptIsrt ==. False] []
          time <- liftIO $ getCurrentTime
          let day = utctDay time
          let rt = [ r | r <-rt' , isOD r day ]
          if P.null rt
            then returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("not return the book overdate" ::String)
              ]
            else a
        isBookOverData :: Yesod master
                       => Int
                       -> HandlerT Management (HandlerT master IO) Text
                       -> HandlerT Management (HandlerT master IO) Text
        isBookOverData bid a = do
          rt' <- liftHandlerT $ runDB $ selectList [BookoptBbc ==. bid,BookoptIsrt ==. False] []
          time <- liftIO $ getCurrentTime
          let day = utctDay time
          let rt = [ r | r<- rt', isOD r day]
          if P.null rt
            then returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("This book is overDATE." ::String)
              ]
            else a
\end{code}
归还图书处理函数。
\begin{code}
        postBookreturnR :: Yesod master
                      => Text
                      -> HandlerT Management (HandlerT master IO) Text
        postBookreturnR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          bId <- lookupPostParam "bid"
          case bId of
            (Just bid') -> let bid = read $ read $ show bid' in
                      isBookOs bid False $ isBookOverData bid $ returnBook bid
            _ -> returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("where is the book??" ::String)
              ]

          where
            returnBook :: Yesod master
                       => Int
                       -> HandlerT Management (HandlerT master IO) Text
            returnBook bid = do
              liftHandlerT $ runDB $ updateWhere [BookoptBbc ==. bid,BookoptIsrt ==. False] [BookoptIsrt =. True]
              returnTJson $ object
                [ "status" .= ("success" :: String)]

\end{code}

处理续借的图书。
\begin{code}
        postBookrenewR :: Yesod master
                       => Text
                       -> HandlerT Management (HandlerT master IO) Text
        postBookrenewR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          bId <- lookupPostParam "bid"
          case bId of
            Just bid' -> do
              let bid = read $ read $ show bid'
              rId <- findReaderId bid
              case rId of
                Just (Just rid) -> isHasOverDate rid $ renew bid
                _ -> returnTJson $ object
                  [ "status" .= ( "fail" :: String)
                  , "reason" .= ( "On shelf or is no your" ::String)
                  ]
            _ -> returnTJson $ object
              [ "status" .= ("failed" ::String)
              , "reason" .= ("No book id"::String)
              ]
          where
            lam (Entity _ x) = x
            findReaderId bid = do
              r <- liftHandlerT $ runDB $ selectList [BookitemBarcode ==. bid] []
              case lam $ head r of
                Bookitem _ _ False _ rid _ _ -> return $ Just rid
                _ -> return Nothing
            renew bid = do
              item <- liftHandlerT $ runDB $ selectList [BookoptBbc ==. bid] []
              key <- liftHandlerT $ runDB $ selectKeysList [BookoptBbc ==. bid,BookoptIsrt ==. False] []
              let (Bookopt _ _ _ t d' _) = lam $ head $ item
              case d' of
                Just d -> do
                  liftHandlerT $ runDB $ update (head key) [BookoptTimes =. (t+1),BookoptRtdate =. Just (addDays 30 d)]
                  returnTJson $ object
                    [ "status" .= ("success" ::String)
                    ]
                _ -> returnTJson $ object
                  [ "status" .= ("error" ::String)
                  , "reason" .= ("inline=>return-date=>Nothing" ::String)
                  ]
\end{code}


实现 YesodSubDispatch
\begin{code}
        instance Yesod master => YesodSubDispatch Management (HandlerT master IO) where
          yesodSubDispatch = $(mkYesodSubDispatch resourcesManagement)
\end{code}
