




% AddDel.lhs
% 图书的录入与销毁

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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
\end{code}
\subsection{模块 AddDel}
\begin{code}
module AddDel
    ( module AddDel
    , module AddDel.Data
    ) where
\end{code}
导入 Yesod , Common 与 AddDel.Del。
\begin{code}
        import Yesod
        import Common
        import AddDel.Data
\end{code}
使用 aeson 处理 JSON 。
\begin{code}
        import Data.Aeson
\end{code}
对 Maybe 的处理。
\begin{code}
        import Data.Maybe
\end{code}
对 Text 的处理。
\begin{code}
        import Prelude hiding ()
        import Data.Text.Lazy hiding (null)
        import Data.String
        import Data.Text.Internal(showText)
\end{code}
Persist \& PostgreSQL。
\begin{code}
        import Database.Persist
        import Database.Persist.TH
        import Database.Persist.Postgresql
\end{code}
时间数据。
\begin{code}
        import Data.Time
\end{code}

\subsection{数据处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist AddDel where
          type YesodPersistBackend AddDel = SqlBackend
          runDB a = do
            AddDel p <- getYesod
            runSqlPool a p
\end{code}
图书记录的表 table\_bookinfo。
图书实体记录的表 table\_bookitem。
图书购入记录的表 table\_bookopt\_in。
图书销毁记录的表 table\_bookpt\_out。
操作记录的表 table\_opt。
\begin{code}
        share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
          Bookinfo json sql=table_bookinfo
            Id sql=
            isbn Int
            bookname Text Maybe
            bookauth Text Maybe sql=author
            publocal Text Maybe sql=publish_local
            pubhouse Text Maybe sql=publish_house
            pubdate Day Maybe sql=publish_date
            liblical Text Maybe sql=library_local
            libindex Text Maybe sql=library_index
            Primary isbn
            deriving Show Eq
          Bookitem json sql=table_bookitem
            Id sql=
            barcode Int
            isbn Int
            shelf Bool sql=on_shelf
            there Bool sql=is_there
            latestopt Text Maybe sql=latest_opt_id
            bdate Day Maybe sql=bought_date
            dprice Double Maybe sqltype=money sql=bought_price
            Primary barcode
            deriving Show Eq
          Bookoptin json sql=table_bookopt_in
            Id sql=
            bsm Text sql=big_serial_number
            price Double sql=bought_price sqltype=money
            isbn Int
            barcode Int
            note Text Maybe
            Primary bsm
            deriving Show Eq
          Bookoptout json sql=table_bookopt_out
            Id sql=
            bsm Text sql=big_serial_number
            reason Text
            note Text Maybe
            Primary bsm
            deriving Show Eq
          Opt json sql=table_opt
            Id sql=
            ssm Int sql=small_serial_number
            date  Day sql=opt_date
            utype Int sql=opt_usr_type
            uid Text sql=opt_usr_id
            Primary ssm
            deriving Show Eq
          |]
\end{code}

\subsection{添加图书}
添加图书实体。
\begin{code}
        postPutinR :: Yesod master
                   => Text
                   -> HandlerT AddDel (HandlerT master IO) Text
        postPutinR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          isbn' <- lookupPostParam "isbn"
          bc' <- lookupPostParam "barcode"
          case (isbn',bc') of
            (Just isbn'',Just bc'') ->
              let isbn = read $ read $ show isbn''
                  bc = read $ read $ show bc''
              in checkISBN isbn $ checkBarcode bc $ addNew isbn bc
            _ -> returnTJson $ object
              [ "status" .= ("error" ::String)
              , "reason" .= ("no isbn or barcode"::String)
              ]
          where
            addNew :: Yesod master
                   => Int
                   -> Int
                   -> HandlerT AddDel (HandlerT master IO) Text
            addNew isbn bc = do
              liftHandlerT $ runDB $ insert $ Bookitem bc isbn True True Nothing Nothing Nothing
              ssm <- liftIO $ getRandom
              time <- liftIO $ getCurrentTime
              liftHandlerT $ runDB $ insert $ Opt ssm (utctDay time) 1 "nothing"
              liftHandlerT $ runDB $ insert $ Bookoptin (fromString ((show ssm) ++ "@" ++ (show $ utctDay time))) 0.0 isbn bc Nothing
              returnTJson $ object
                [ "status" .= ("success" :: String)]
            checkISBN :: Yesod master
                      => Int
                      -> HandlerT AddDel (HandlerT master IO) Text
                      -> HandlerT AddDel (HandlerT master IO) Text
            checkISBN isbn f = do
              rt <- liftHandlerT $ runDB $ selectList [BookinfoIsbn ==. isbn] []
              if null rt
                then returnTJson $ object
                  [ "status" .= ("failed" :: String)
                  , "reason" .= ("no such a ISBN code" ::String)
                  ]
                else f
            checkBarcode :: Yesod master
                         => Int
                         -> HandlerT AddDel (HandlerT master IO) Text
                         -> HandlerT AddDel (HandlerT master IO) Text
            checkBarcode bc f = do
              rt <- liftHandlerT $ runDB $ selectList [BookitemBarcode ==. bc] []
              if null rt
                then f
                else returnTJson $ object
                  [ "status" .= ("failed" ::String)
                  , "reason" .= ("another book used it."::String)
                  ]
\end{code}
添加图书信息，非实体。
\begin{code}
        postAddnewR :: Yesod master
                    => Text
                    -> HandlerT AddDel (HandlerT master IO) Text
        postAddnewR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          isbn' <- lookupPostParam "isbn"
          title' <- lookupPostParam "title"
          auth' <- lookupPostParam "auth"
          publocal' <- lookupPostParam "publocal"
          pubh' <- lookupPostParam "pubh"
          pubd' <- lookupPostParam "pubd"
          zth' <- lookupPostParam "zth"
          case (isbn',title',auth',publocal',pubh',pubd',zth') of
            (Just isbn'',title,auth,publ,pubh,pubd'',zth) ->
              let isbn = read $ read $ show isbn''
                  pubd = fmap (read.read.show) pubd'' :: Maybe Day
              in checkISBN isbn $ addNew
                isbn
                (fmap t2t title)
                (fmap t2t auth)
                (fmap t2t publ)
                (fmap t2t pubh)
                pubd
                (fmap t2t zth)
            _ -> returnTJson $ object
                [ "status" .= ("failed"::String)
                , "reason" .= ("some params missing." ::String)
                ]
          where
            addNew :: Yesod master
                   => Int -- isbn
                   -> Maybe Text -- title (bookname)
                   -> Maybe Text -- author
                   -> Maybe Text -- where it was published
                   -> Maybe Text -- the publish house
                   -> Maybe Day --  when it was published
                   -> Maybe Text -- 中图分类号
                   -> HandlerT AddDel (HandlerT master IO) Text
            addNew isbn title auth publ pubh pubd zth = do
              liftHandlerT $ runDB $ insert $ Bookinfo isbn title auth publ pubh pubd Nothing zth
              returnTJson $ object
                [ "status" .= ("success" ::String)]
            checkISBN :: Yesod master
                      => Int
                      -> HandlerT AddDel (HandlerT master IO) Text
                      -> HandlerT AddDel (HandlerT master IO) Text
            checkISBN isbn f = do
              rt <- liftHandlerT $ runDB $ selectList [BookinfoIsbn ==. isbn] []
              if null rt
                then f
                else returnTJson $ object
                  [ "status" .= ("failed" :: String)
                  , "reason" .= ("has such a ISBN code" ::String)
                  ]
\end{code}
\subsection{删除图书}
删除图书实体,但不图书信息。
\begin{code}
        postRemoveoutR :: Yesod master
                       => Text
                       -> HandlerT AddDel (HandlerT master IO) Text
        postRemoveoutR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          isbn' <- liftHandlerT $ lookupPostParam "isbn"
          barcode' <- liftHandlerT $ lookupPostParam "barcode"
          let barcode = fmap (read.read.show) barcode'
          let isbn = fmap (read.read.show) isbn'
          bb <- b barcode
          ii <- i isbn
          gother [bb,ii]
          where
            i isbn = case isbn of
              Nothing -> return Nothing
              Just ib -> deleteBYisbn ib
            b barcode = case barcode of
              Nothing -> return Nothing
              Just bc -> deleteBYbarcode bc
            gother :: Yesod master
                   => [Maybe Text]
                   -> HandlerT AddDel (HandlerT master IO) Text
            gother xs = do
              let x = togo xs
              if null x
                then returnTJson $ object
                  [ "status" .= ("success" ::String)]
                else returnTJson $ object
                  [ "status" .= ("failed" ::String)
                  , "reason" .= x
                  ]
            togo :: [Maybe Text] -> [Text]
            togo [] = []
            togo (Nothing:xs) = togo xs
            togo (Just x:xs) = x:togo xs
            checkISBN :: Yesod master
                      => Int
                      -> HandlerT AddDel (HandlerT master IO) Bool
            checkISBN isbn =
              (return.not.null) =<< (liftHandlerT $ runDB $ selectList [BookitemIsbn ==. isbn] [])
            checkBarcode :: Yesod master
                         => Int
                         -> HandlerT AddDel (HandlerT master IO) Bool
            checkBarcode bc =
              (liftHandlerT $ runDB $ selectList [BookitemBarcode ==. bc] []) >>= (return.not.null)
            deleteBYisbn :: Yesod master
                         => Int
                         -> HandlerT AddDel (HandlerT master IO) (Maybe Text)
            deleteBYisbn isbn = do
              is <- checkISBN isbn
              if is
                then do
                  liftHandlerT $ runDB $ deleteWhere [BookitemIsbn ==. isbn]
                  ssm <- liftIO $ getRandom
                  time <- liftIO $ getCurrentTime
                  liftHandlerT $ runDB $ insert $ Opt ssm (utctDay time) 1 "nothing"
                  liftHandlerT $ runDB $ insert $ Bookoptout (fromString ((show ssm) ++ "@" ++ (show $ utctDay time)))  "just remove it!" Nothing
                  return Nothing
                else return $ Just "no such books"
            deleteBYbarcode :: Yesod master
                            => Int
                            -> HandlerT AddDel (HandlerT master IO) (Maybe Text)
            deleteBYbarcode bc = do
              is <- checkBarcode bc
              if is
                then do
                  liftHandlerT $ runDB $ deleteWhere [BookitemBarcode ==. bc]
                  ssm <- liftIO $ getRandom
                  time <- liftIO $ getCurrentTime
                  liftHandlerT $ runDB $ insert $ Opt ssm (utctDay time) 1 "nothing"
                  liftHandlerT $ runDB $ insert $ Bookoptout (fromString ((show ssm) ++ "@" ++ (show $ utctDay time)))  "just remove it!" Nothing
                  return Nothing
                else return $ Just "no such book"
\end{code}
实现 YesodSubDispatch
\begin{code}
        instance Yesod master => YesodSubDispatch AddDel (HandlerT master IO) where
          yesodSubDispatch = $(mkYesodSubDispatch resourcesAddDel)
\end{code}
