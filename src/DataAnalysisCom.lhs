




%数据分析借口
%DataAnalysisCom.lhs

%导入导言区
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

\subsection{模块}
\begin{code}
module DataAnalysisCom
      ( module DataAnalysisCom
      , module DataAnalysisCom.Data
      ) where
\end{code}

\subsection{导入}
导入 Yesod 、 Common 与 Management.Data。
\begin{code}
        import Yesod
        import DataAnalysisCom.Data
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
处理 Text 与 ByteStrings。
\begin{code}
        import Prelude hiding ()
        import Data.Text.Lazy hiding (map,concat)
        import Data.String
        import Data.Text.Internal(showText)
\end{code}
处理 Either。
\begin{code}
        import Data.Either
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
处理Monad。
\begin{code}
        import Control.Monad
\end{code}

\subsection{数据库处理}
使用 Persistent 处理数据库。
\begin{code}
        instance YesodPersist AnaCom where
          type YesodPersistBackend AnaCom = SqlBackend
          runDB a = do
            AnaCom p <- getYesod
            runSqlPool a p
\end{code}
数据库的表。
\begin{description}
  \item[图书实体信息] tabel_bookinfo
  \item[图书信息] table_bookitem
  \item[读者信息] table_reader
  \item[图书录入] table_bookopt_in
  \item[图书销毁] table_bookopt_out
  \item[图书操作主记录] table_bookopt_main
  \item[操作流水号] table_opt
  \item[处罚记录] table_punish
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
          |]
\end{code}

\subsection{处理函数}
BookinfolistR，处理图书信息访问列表。
\begin{code}
        postBookinfolistR :: Yesod master
                          => Text
                          -> HandlerT AnaCom (HandlerT master IO) Text
        postBookinfolistR _ = do
          liftHandlerT $ addHeader "Content-Type" "application/json"
          isbn <- lookupPostParam "isbn"
          title <- lookupPostParam "title"
          auth <- lookupPostParam "author"
          publ <- lookupPostParam "publishlocal"
          pubh <- lookupPostParam "publishhouse"
          pubd <- lookupPostParam "publishdate"
          libl <- lookupPostParam "librarylocal"
          libi <- lookupPostParam "zth"
          let
           just = Just . t2t
           isbn' = case isbn of
            Just x -> [BookinfoIsbn ==. (read $ read $ show x)]
            Nothing -> []
           title' = case title of
            Just x -> [BookinfoName ==. just x]
            Nothing -> []
           auth' = case auth of
            Just x -> [BookinfoAuthor ==. just x]
            Nothing -> []
           publ' = case publ of
            Just x -> [BookinfoPlocal ==. just x]
            Nothing -> []
           pubh' = case pubh of
            Just x -> [BookinfoPhouse ==. just x]
            Nothing -> []
           pubd' = case pubd of
            Just x -> [BookinfoPdate ==. Just (read $ read $ show x)]
            Nothing -> []
           libl' = case libl of
            Just x -> [BookinfoLlocal ==. just x]
            Nothing -> []
           libi' = case libi of
            Just x -> [BookinfoLlindex ==. just x]
            Nothing -> []
           sql = concat
            [ isbn'
            , title'
            , auth'
            , publ'
            , pubh'
            , pubd'
            , libl'
            , libi'
            ]in do
            rt' <- liftHandlerT $ runDB $ selectList sql []
            let rt = map lam rt'
            returnTJson $ object
              [ "status" .= ("success" ::String)
              , "result" .= rt
              ]
          where
            lam (Entity _ x) = x
\end{code}
BookitemlistR，处理图书实体访问。
\begin{code}
        postBookitemlistR :: Yesod master
                          => Text
                          -> HandlerT AnaCom (HandlerT master IO) Text
        postBookitemlistR _ = undefined
\end{code}
ReaderlistR，处理读者信息访问。
\begin{code}
        postReaderlistR :: Yesod master
                        => Text
                        -> HandlerT AnaCom (HandlerT master IO) Text
        postReaderlistR _ = undefined
\end{code}
BookoptinlistR，处理录入操作信息访问。
\begin{code}
        postBookoptinlistR :: Yesod master
                           => Text
                           -> HandlerT AnaCom (HandlerT master IO) Text
        postBookoptinlistR _ = undefined
\end{code}
BookoptoutlistR，处理销毁操作信息访问。
\begin{code}
        postBookoptoutlistR :: Yesod master
                            => Text
                            -> HandlerT AnaCom (HandlerT master IO) Text
        postBookoptoutlistR _ = undefined
\end{code}
BookoptmainlistR，处理借阅操作信息访问操作。
\begin{code}
        postBookoptmainlistR :: Yesod master
                             => Text
                             -> HandlerT AnaCom (HandlerT master IO) Text
        postBookoptmainlistR _ = undefined
\end{code}
PunishlistR，处理处罚信息访问操作。
\begin{code}
        postPunishlistR :: Yesod master
                        => Text
                        -> HandlerT AnaCom (HandlerT master IO) Text
        postPunishlistR _ = undefined
\end{code}

实现 YesodSubDispatch
\begin{code}
        instance Yesod master => YesodSubDispatch AnaCom (HandlerT master IO) where
          yesodSubDispatch = $(mkYesodSubDispatch resourcesAnaCom)
\end{code}
